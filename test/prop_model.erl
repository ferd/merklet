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

prop_overwrite() ->
    ?FORALL({Entries, ToUpdate}, overwrite_keyvals(0.5),
            merklet_model:keys(
                merklet_model:insert_many(ToUpdate,
                    merklet_model:insert_many(Entries,undefined)))
            =:=
            merklet_zipper:keys(
                merklet_zipper:insert_many(ToUpdate,
                    merklet_zipper:insert_many(Entries,undefined)))
           ).

prop_insert_same_diff() ->
    ?FORALL(Entries, keyvals(),
            merklet_model:diff(merklet_model:insert_many(Entries,undefined),
                               merklet_model:insert_many(Entries,undefined))
            =:=
            merklet_zipper:diff(merklet_zipper:insert_many(Entries,undefined),
                                merklet_zipper:insert_many(Entries,undefined))
           ).

prop_insert_mixed_diff() ->
    ?FORALL({Entries1, Entries2}, {keyvals(), keyvals()},
            merklet_model:diff(merklet_model:insert_many(Entries1,undefined),
                               merklet_model:insert_many(Entries2,undefined))
            =:=
            merklet_zipper:diff(merklet_zipper:insert_many(Entries1,undefined),
                                merklet_zipper:insert_many(Entries2,undefined))
           ).

prop_insert_disjoint_diff() ->
    ?FORALL(Lists, disjoint_keyvals(),
        begin
            {Entries1, Entries2} = Lists,
            merklet_model:diff(merklet_model:insert_many(Entries1,undefined),
                               merklet_model:insert_many(Entries2,undefined))
            =:=
            merklet_zipper:diff(merklet_zipper:insert_many(Entries1,undefined),
                                merklet_zipper:insert_many(Entries2,undefined))
        end).

prop_delete_random_diff() ->
    ?FORALL({Entries, ToDelete}, {keyvals(), list(binary())},
        begin
            ModelFull = merklet_model:insert_many(Entries,undefined),
            ModelDelete = delete(merklet_model, ToDelete, ModelFull),
            ZipperFull = merklet_zipper:insert_many(Entries,undefined),
            ZipperDelete = delete(merklet_zipper, ToDelete, ZipperFull),
            (merklet_model:diff(ModelFull, ModelDelete)
             =:= merklet_zipper:diff(ZipperFull, ZipperDelete))
            andalso
            (merklet_model:diff(ModelDelete, ModelFull)
             =:= merklet_zipper:diff(ZipperDelete, ZipperFull))
        end).

prop_delete_members_diff() ->
    ?FORALL({Entries, ToDelete}, delete_keyvals(0.5),
        begin
            ModelFull = merklet_model:insert_many(Entries,undefined),
            ModelDelete = delete(merklet_model, ToDelete, ModelFull),
            ZipperFull = merklet_zipper:insert_many(Entries,undefined),
            ZipperDelete = delete(merklet_zipper, ToDelete, ZipperFull),
            (merklet_model:diff(ModelFull, ModelDelete)
             =:= merklet_zipper:diff(ZipperFull, ZipperDelete))
            andalso
            (merklet_model:diff(ModelDelete, ModelFull)
             =:= merklet_zipper:diff(ZipperDelete, ZipperFull))
        end).

prop_overwrite_diff() ->
    ?FORALL({Entries, ToUpdate}, overwrite_keyvals(0.5),
        begin
            ModelFull = merklet_model:insert_many(Entries, undefined),
            ModelUpdate = merklet_model:insert_many(ToUpdate, ModelFull),
            ZipperFull = merklet_zipper:insert_many(Entries, undefined),
            ZipperUpdate = merklet_zipper:insert_many(ToUpdate, ZipperFull),
            (merklet_model:diff(ModelFull, ModelUpdate)
             =:= merklet_zipper:diff(ZipperFull, ZipperUpdate))
            andalso
            (merklet_model:diff(ModelUpdate, ModelFull)
             =:= merklet_zipper:diff(ZipperUpdate, ZipperFull))
        end).

prop_mixed_diff() ->
    ?FORALL({{Entries, ToUpdate}, ToDelete}, {overwrite_keyvals(0.5), list(binary())},
        begin
            ModelFull = merklet_model:insert_many(Entries, undefined),
            ModelDelete = delete(merklet_model, ToDelete, ModelFull),
            ModelUpdate = merklet_model:insert_many(ToUpdate, ModelDelete),
            ZipperFull = merklet_zipper:insert_many(Entries, undefined),
            ZipperDelete = delete(merklet_zipper, ToDelete, ZipperFull),
            ZipperUpdate = merklet_zipper:insert_many(ToUpdate, ZipperDelete),
            %% Full vs. Update
            (merklet_model:diff(ModelFull, ModelUpdate)
             =:= merklet_zipper:diff(ZipperFull, ZipperUpdate))
            andalso
            (merklet_model:diff(ModelUpdate, ModelFull)
             =:= merklet_zipper:diff(ZipperUpdate, ZipperFull))
            %% Full vs. Delete
            andalso
            (merklet_model:diff(ModelFull, ModelDelete)
             =:= merklet_zipper:diff(ZipperFull, ZipperDelete))
            andalso
            (merklet_model:diff(ModelDelete, ModelFull)
             =:= merklet_zipper:diff(ZipperDelete, ZipperFull))
            %% Delete vs. Update
            andalso
            (merklet_model:diff(ModelUpdate, ModelDelete)
             =:= merklet_zipper:diff(ZipperUpdate, ZipperDelete))
            andalso
            (merklet_model:diff(ModelDelete, ModelUpdate)
             =:= merklet_zipper:diff(ZipperDelete, ZipperUpdate))
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
        
