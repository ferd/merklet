-module(prop_model).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(OPTS, [{numtests,1000}, {to_file, user}]).
-define(run(Case), {timeout, timer:seconds(60),
                    ?_assert(proper:quickcheck(Case, ?OPTS))}).

eunit_test_() ->
    [?run(prop_add_many()),
     ?run(prop_delete_random()),
     ?run(prop_delete_members())].

prop_add_many() ->
    ?FORALL(Entries, keyvals(),
            merklet_model:keys(merklet_model:insert_many(Entries,undefined))
            =:=
            merklet_zipper:keys(merklet_zipper:insert_many(Entries,undefined))
           ).

prop_delete_random() ->
    ?FORALL({Entries, ToDelete}, {keyvals(), list(binary())},
            merklet_model:keys(
                delete(merklet_model,
                       ToDelete,
                       merklet_model:insert_many(Entries,undefined)))
            =:=
            merklet_zipper:keys(
                delete(merklet_zipper,
                       ToDelete,
                       merklet_zipper:insert_many(Entries,undefined)))
           ).

prop_delete_members() ->
    ?FORALL({Entries, ToDelete}, delete_keyvals(0.5),
            merklet_model:keys(
                delete(merklet_model,
                       ToDelete,
                       merklet_model:insert_many(Entries,undefined)))
            =:=
            merklet_zipper:keys(
                delete(merklet_zipper,
                       ToDelete,
                       merklet_zipper:insert_many(Entries,undefined)))
           ).

delete(_Mod, [], Struct) -> Struct;
delete(Mod, [H|T], Struct) -> delete(Mod, T, Mod:delete(H, Struct)).


%%%%%%%%%%%%%%%%%%
%%% GENERATORS %%%
%%%%%%%%%%%%%%%%%%
keyvals() -> list({binary(), binary()}).

delete_keyvals(Rate) ->
    ?LET(KeyVals, keyvals(),
         begin
          Rand = random:uniform(),
          ToDelete = [Key || {Key,_} <- KeyVals, Rate > Rand],
          {KeyVals, ToDelete}
         end).

