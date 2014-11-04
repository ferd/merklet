-module(prop_model).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(OPTS, [{numtests,1000}, {to_file, user}]).
-define(run(Case), {timeout, timer:seconds(60),
                    ?_assert(proper:quickcheck(Case, ?OPTS))}).

eunit_test_() ->
    [?run(prop_insert_many()),
     ?run(prop_delete_random()),
     ?run(prop_delete_members()),
     ?run(prop_overwrite()),
     ?run(prop_insert_same_diff()),
     ?run(prop_insert_mixed_diff()),
     ?run(prop_insert_disjoint_diff()),
     ?run(prop_delete_random_diff()),
     ?run(prop_delete_members_diff()),
     ?run(prop_overwrite_diff()),
     ?run(prop_mixed_diff())
    ].
%% prop_mixed_diff

prop_insert_many() ->
    ?FORALL(Entries, keyvals(),
            merklet_model:keys(merklet_model:insert_many(Entries,undefined))
            =:=
            merklet:keys(merklet:insert_many(Entries,undefined))
           ).

prop_delete_random() ->
    ?FORALL({Entries, ToDelete}, {keyvals(), list(binary())},
            merklet_model:keys(
                delete(merklet_model,
                       ToDelete,
                       merklet_model:insert_many(Entries,undefined)))
            =:=
            merklet:keys(
                delete(merklet,
                       ToDelete,
                       merklet:insert_many(Entries,undefined)))
           ).

prop_delete_members() ->
    ?FORALL({Entries, ToDelete}, delete_keyvals(0.5),
            merklet_model:keys(
                delete(merklet_model,
                       ToDelete,
                       merklet_model:insert_many(Entries,undefined)))
            =:=
            merklet:keys(
                delete(merklet,
                       ToDelete,
                       merklet:insert_many(Entries,undefined)))
           ).

prop_overwrite() ->
    ?FORALL({Entries, ToUpdate}, overwrite_keyvals(0.5),
            merklet_model:keys(
                merklet_model:insert_many(ToUpdate,
                    merklet_model:insert_many(Entries,undefined)))
            =:=
            merklet:keys(
                merklet:insert_many(ToUpdate,
                    merklet:insert_many(Entries,undefined)))
           ).

prop_insert_same_diff() ->
    ?FORALL(Entries, keyvals(),
            merklet_model:diff(merklet_model:insert_many(Entries,undefined),
                               merklet_model:insert_many(Entries,undefined))
            =:=
            merklet:diff(merklet:insert_many(Entries,undefined),
                                merklet:insert_many(Entries,undefined))
           ).

prop_insert_mixed_diff() ->
    ?FORALL({Entries1, Entries2}, {keyvals(), keyvals()},
            merklet_model:diff(merklet_model:insert_many(Entries1,undefined),
                               merklet_model:insert_many(Entries2,undefined))
            =:=
            merklet:diff(merklet:insert_many(Entries1,undefined),
                                merklet:insert_many(Entries2,undefined))
           ).

prop_insert_disjoint_diff() ->
    ?FORALL(Lists, disjoint_keyvals(),
        begin
            {Entries1, Entries2} = Lists,
            merklet_model:diff(merklet_model:insert_many(Entries1,undefined),
                               merklet_model:insert_many(Entries2,undefined))
            =:=
            merklet:diff(merklet:insert_many(Entries1,undefined),
                                merklet:insert_many(Entries2,undefined))
        end).

prop_delete_random_diff() ->
    ?FORALL({Entries, ToDelete}, {keyvals(), list(binary())},
        begin
            ModelFull = merklet_model:insert_many(Entries,undefined),
            ModelDelete = delete(merklet_model, ToDelete, ModelFull),
            MerkletFull = merklet:insert_many(Entries,undefined),
            MerkletDelete = delete(merklet, ToDelete, MerkletFull),
            (merklet_model:diff(ModelFull, ModelDelete)
             =:= merklet:diff(MerkletFull, MerkletDelete))
            andalso
            (merklet_model:diff(ModelDelete, ModelFull)
             =:= merklet:diff(MerkletDelete, MerkletFull))
        end).

prop_delete_members_diff() ->
    ?FORALL({Entries, ToDelete}, delete_keyvals(0.5),
        begin
            ModelFull = merklet_model:insert_many(Entries,undefined),
            ModelDelete = delete(merklet_model, ToDelete, ModelFull),
            MerkletFull = merklet:insert_many(Entries,undefined),
            MerkletDelete = delete(merklet, ToDelete, MerkletFull),
            (merklet_model:diff(ModelFull, ModelDelete)
             =:= merklet:diff(MerkletFull, MerkletDelete))
            andalso
            (merklet_model:diff(ModelDelete, ModelFull)
             =:= merklet:diff(MerkletDelete, MerkletFull))
        end).

prop_overwrite_diff() ->
    ?FORALL({Entries, ToUpdate}, overwrite_keyvals(0.5),
        begin
            ModelFull = merklet_model:insert_many(Entries, undefined),
            ModelUpdate = merklet_model:insert_many(ToUpdate, ModelFull),
            MerkletFull = merklet:insert_many(Entries, undefined),
            MerkletUpdate = merklet:insert_many(ToUpdate, MerkletFull),
            (merklet_model:diff(ModelFull, ModelUpdate)
             =:= merklet:diff(MerkletFull, MerkletUpdate))
            andalso
            (merklet_model:diff(ModelUpdate, ModelFull)
             =:= merklet:diff(MerkletUpdate, MerkletFull))
        end).

prop_mixed_diff() ->
    ?FORALL({{Entries, ToUpdate}, ToDelete}, {overwrite_keyvals(0.5), list(binary())},
        begin
            ModelFull = merklet_model:insert_many(Entries, undefined),
            ModelDelete = delete(merklet_model, ToDelete, ModelFull),
            ModelUpdate = merklet_model:insert_many(ToUpdate, ModelDelete),
            MerkletFull = merklet:insert_many(Entries, undefined),
            MerkletDelete = delete(merklet, ToDelete, MerkletFull),
            MerkletUpdate = merklet:insert_many(ToUpdate, MerkletDelete),
            %% Full vs. Update
            (merklet_model:diff(ModelFull, ModelUpdate)
             =:= merklet:diff(MerkletFull, MerkletUpdate))
            andalso
            (merklet_model:diff(ModelUpdate, ModelFull)
             =:= merklet:diff(MerkletUpdate, MerkletFull))
            %% Full vs. Delete
            andalso
            (merklet_model:diff(ModelFull, ModelDelete)
             =:= merklet:diff(MerkletFull, MerkletDelete))
            andalso
            (merklet_model:diff(ModelDelete, ModelFull)
             =:= merklet:diff(MerkletDelete, MerkletFull))
            %% Delete vs. Update
            andalso
            (merklet_model:diff(ModelUpdate, ModelDelete)
             =:= merklet:diff(MerkletUpdate, MerkletDelete))
            andalso
            (merklet_model:diff(ModelDelete, ModelUpdate)
             =:= merklet:diff(MerkletDelete, MerkletUpdate))
        end).


%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%
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

overwrite_keyvals(Rate) ->
    ?LET(KeyVals, keyvals(),
         begin
          Rand = random:uniform(),
          ToUpdate = [{Key, <<0,Val/binary>>} || {Key,Val} <- KeyVals, Rate > Rand],
          {KeyVals, ToUpdate}
         end).

disjoint_keyvals() ->
    ?SUCHTHAT({KV1, KV2}, {keyvals(), keyvals()},
            begin
              KS1 = [K || {K, _} <- KV1],
              KS2 = [K || {K, _} <- KV2],
              lists:all(fun(K) -> not lists:member(K,KS2) end, KS1)
            end).
        
