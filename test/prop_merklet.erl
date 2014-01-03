-module(prop_merklet).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(OPTS, [{numtests,1000}, {to_file, user}]).
-define(run(Case), {timeout, timer:seconds(60),
                    ?_assert(proper:quickcheck(Case, ?OPTS))}).

eunit_test_() ->
    [?run(prop_diff()),
     ?run(prop_delete()),
     ?run(prop_modify())].

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_diff() ->
    %% All differences between trees can be found no matter the order,
    %% and returns the list of different keys.
    ?FORALL({KV1,KV2}, diff_keyvals(),
            begin
                Keys = [K || {K,_} <- KV2],
                T1 = insert_all(KV1),
                T2 = insert_all(KV2, T1),
                Diff1 = merklet:diff(T1,T2),
                Diff2 = merklet:diff(T2,T1),
                Diff1 =:= Diff2
                andalso
                Diff1 =:= lists:sort(Keys)
            end).

prop_delete() ->
    %% Having a tree and deleting a percentage of it yields the same tree
    %% without said keys.
    ?FORALL({All, Partial, ToDelete}, delete_keyvals(0.50),
            begin
                Tree = insert_all(All),
                PartialTree = insert_all(Partial),
                DeletedTree = delete_keys(ToDelete, Tree),
                [] =:= merklet:diff(PartialTree, DeletedTree)
                andalso
                merklet:keys(DeletedTree) =:= merklet:keys(PartialTree)
                andalso
                DeletedTree =:= PartialTree
            end).

prop_modify() ->
    %% Updating records' values should show detections as part of merklet's
    %% diff operations, even if none of the keys change.
    ?FORALL({All, ToChange}, modify_keyvals(0.90),
            begin
                Tree = insert_all(All),
                KVSet = [{K, term_to_binary(make_ref())} || K <- ToChange],
                Modified = insert_all(KVSet, Tree),
                merklet:keys(Tree) =:= merklet:keys(Modified)
                andalso
                lists:sort(ToChange) =:= merklet:diff(Tree, Modified)
            end).

%%%%%%%%%%%%%%%%
%%% Builders %%%
%%%%%%%%%%%%%%%%
insert_all(KeyVals) -> insert_all(KeyVals, undefined).
insert_all(KeyVals, Tree) -> lists:foldl(fun merklet:insert/2, Tree, KeyVals).

delete_keys(Keys, Tree) -> lists:foldl(fun merklet:delete/2, Tree, Keys).

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
    ?LET(KeyVals, keyvals(),
         begin
          Rand = random:uniform(),
          ToDelete = [Key || {Key,_} <- KeyVals, Rate > Rand],
          {KeyVals, lists:usort(ToDelete)}
         end).
