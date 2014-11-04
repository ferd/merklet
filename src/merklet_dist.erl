%% Make merklet usable past the data structure.
%% Particularly, it's very expensive to diff given the default access function
%% passes in a zipper as state; every branch or recursive look down the tree
%% then forks the 
-module(merklet_dist).
-compile(export_all).

start() -> spawn_link(fun() -> loop(undefined) end).

diff(Pid, TargetPid) ->
    req({diff, TargetPid}, Pid).

ask(Term, Path, Pid) ->
    case req({access, Term, Path}, Pid) of
        Resp when Term == at; Term == child_at ->
            {merklet_zipper:unserialize(Resp), Pid};
        Resp when Term == keys; element(1,Term) == keys ->
            merklet_zipper:unserialize(Resp)
    end.

insert(Key, Val, Pid) ->
    req({insert, Key, Val}, Pid).

insert_many(KVs, Pid) ->
    req({insert_many, KVs}, Pid).

delete(Key, Pid) ->
    req({delete, Key}, Pid).

keys(Pid) ->
    req(keys, Pid).

req(Req, Pid) ->
    Ref = monitor(process, Pid),
    Pid ! {self(), Ref, Req},
    receive
        {'DOWN', Ref, _, _, Info} ->
            error({down, Pid, Info});
        {Ref, Resp} -> 
            erlang:demonitor(Ref),
            Resp
    end.

loop(Zipper) ->
    receive
        {From, Ref, Req} -> 
            {Resp, NewZipper} = handle_req(Req, Zipper),
            From ! {Ref, Resp},
            ?MODULE:loop(NewZipper)
    end.

handle_req({insert, Key, Val}, Zipper) ->
    {self(), merklet_zipper:insert(Key, Val, Zipper)};
handle_req({insert_many, KVs}, Zipper) ->
    {self(), merklet_zipper:insert_many(KVs, Zipper)};
handle_req({delete, Key}, Zipper) ->
    {self(), merklet_zipper:delete(Key, Zipper)};
handle_req(keys, Zipper) ->
    {merklet_zipper:keys(Zipper), Zipper};
handle_req({access, Term, Path}, Zipper) ->
    case merklet_zipper:access_serialize(Term, Path, Zipper) of
        {Resp, NewZipper} -> {Resp, NewZipper};
        Resp -> {Resp, Zipper}
    end;
handle_req({diff, Target}, Zipper) ->
    Fun = fun ask/3,
    State = Target,
    Diff = merklet_zipper:diff(Zipper, Fun, State),
    {Diff, Zipper}.
        
