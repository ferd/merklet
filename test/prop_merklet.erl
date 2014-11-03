-module(prop_merklet).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(OPTS, [{numtests,1000}, {to_file, user}]).
-define(run(Case), {timeout, timer:seconds(60),
                    ?_assert(proper:quickcheck(Case, ?OPTS))}).

eunit_test_() ->
    [?run(prop_diff()),
     ?run(prop_dist_diff()),
     ?run(prop_delete()),
     ?run(prop_modify())].

regression_diff_test() ->
    T1 = insert_all_zipper([{<<1>>,<<1>>},{<<2>>,<<2>>},{<<3>>,<<3>>}]),
    T2 = insert_all_zipper([{<<1>>,<<0>>}]),
    ?assertEqual([<<1>>,<<2>>,<<3>>], merklet_zipper:diff(T1,T2)),
    ?assertEqual([<<1>>,<<2>>,<<3>>], merklet_zipper:diff(T2,T1)).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_diff() ->
    %% All differences between trees can be found no matter the order,
    %% and returns the list of different keys.
    ?FORALL({KV1,KV2}, diff_keyvals(),
            begin
                Keys = [K || {K,_} <- KV2],
                T1 = insert_all_zipper(KV1),
                T2 = insert_all_zipper(KV2, T1),
                Diff1 = merklet_zipper:diff(T1,T2),
                Diff2 = merklet_zipper:diff(T2,T1),
                Diff1 =:= Diff2
                andalso
                Diff1 =:= lists:sort(Keys)
            end).

prop_dist_diff() ->
    %% All differences between trees can be found no matter the order,
    %% and returns the list of different keys. Same as previous case, but
    %% uses the internal serialization format and distribution API
    %% functions of merklet to do its thing.
    ?FORALL({KV1,KV2}, diff_keyvals(),
            begin
                Keys = [K || {K,_} <- KV2],
                T1 = insert_all(KV1),
                T2 = insert_all(KV2, T1),
                %% remmote version of the trees, should be handled
                %% by merklet:unserialize/1. In practice, this kind
                %% of thing would take place over the network, and
                %% merklet:access_serialize/2, and R1 and R2 would be
                %% be wrapped in other functions to help.
                R1 = merklet:access_serialize(T1),
                R2 = merklet:access_serialize(T2),
                %% Remote diffing
                Diff1 = merklet:dist_diff(T1,merklet:access_unserialize(R2)),
                Diff2 = merklet:dist_diff(T2,merklet:access_unserialize(R1)),
                Diff1 =:= Diff2
                andalso
                Diff1 =:= lists:sort(Keys)
            end).

prop_delete() ->
    %% Having a tree and deleting a percentage of it yields the same tree
    %% without said keys.
    ?FORALL({All, Partial, ToDelete}, delete_keyvals(0.50),
            begin
                Tree = insert_all_zipper(All),
                PartialTree = insert_all_zipper(Partial),
                DeletedTree = delete_keys_zipper(ToDelete, Tree),
                [] =:= merklet_zipper:diff(PartialTree, DeletedTree)
                andalso
                merklet_zipper:keys(DeletedTree) =:= merklet_zipper:keys(PartialTree)
                andalso
                (DeletedTree =:= PartialTree % shrinking test
                 orelse undefined =:= PartialTree) % zipper wrapper causes failures
            end).

prop_modify() ->
    %% Updating records' values should show detections as part of merklet's
    %% diff operations, even if none of the keys change.
    ?FORALL({All, ToChange}, modify_keyvals(0.50),
            begin
                Tree = insert_all_zipper(All),
                KVSet = [{K, term_to_binary(make_ref())} || K <- ToChange],
                Modified = insert_all_zipper(KVSet, Tree),
                merklet_zipper:keys(Tree) =:= merklet_zipper:keys(Modified)
                andalso
                lists:sort(ToChange) =:= merklet_zipper:diff(Tree, Modified)
                andalso
                lists:sort(ToChange) =:= merklet_zipper:diff(Modified, Tree)
            end).

%%%%%%%%%%%%%%%%
%%% Builders %%%
%%%%%%%%%%%%%%%%
insert_all(KeyVals) -> insert_all(KeyVals, undefined).
insert_all(KeyVals, Tree) -> lists:foldl(fun merklet:insert/2, Tree, KeyVals).

insert_all_zipper(KeyVals) -> insert_all_zipper(KeyVals, undefined).
insert_all_zipper(KeyVals, Tree) -> lists:foldl(fun({K,V}, T) ->  merklet_zipper:insert(K,V,T) end, Tree, KeyVals).

delete_keys(Keys, Tree) -> lists:foldl(fun merklet:delete/2, Tree, Keys).
delete_keys_zipper(Keys, Tree) -> lists:foldl(fun merklet_zipper:delete/2, Tree, Keys).

keyvals() -> list({binary(), binary()}).

diff_keyvals() ->
    ?SUCHTHAT({KV1,KV2}, {keyvals(), keyvals()},
              begin
                K1 = [K || {K,_} <- KV1],
                K2 = [K || {K,_} <- KV2],
                lists:all(fun(K) -> not lists:member(K,K2) end, K1)
                 andalso
                length(lists:usort(K2)) =:= length(K2)
              end).

delete_keyvals(Rate) ->
    ?LET(KeyVals, keyvals(),
         begin
          Rand = random:uniform(),
          ToDelete = [Key || {Key,_} <- KeyVals, Rate > Rand],
          WithoutDeleted = [{K,V} || {K,V} <- KeyVals, Rate < Rand],
          {KeyVals, WithoutDeleted, ToDelete}
         end).

modify_keyvals(Rate) ->
    % similar as delete_keyvals but doesn't allow duplicate updates
    ?SUCHTHAT({_,ToChange}, 
              ?LET(KeyVals, keyvals(),
                begin
                  Rand = random:uniform(),
                  ToDelete = [Key || {Key,_} <- KeyVals, Rate > Rand],
                  {KeyVals, lists:usort(ToDelete)}
                end),
              ToChange =/= []).
