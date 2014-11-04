-module(merklet_model).
-export([insert/3, insert_many/2, delete/2, keys/1]).
-export([diff/2]).

insert(K,V,undefined) -> [{K,V}];
insert(K,V,[]) -> [{K,V}];
insert(K,V,[{K,_}|T]) -> [{K,V}|T];
insert(K,V,[H|T]) -> [H|insert(K,V,T)].

insert_many(L, Init) ->
    lists:foldl(fun({K,V}, Acc) -> insert(K,V,Acc) end, Init, L).

delete(_, undefined) -> undefined;
delete(_, []) -> [];
delete(K, [{K,_}|T]) -> T;
delete(K, [H|T]) -> [H|delete(K,T)].

keys(undefined) -> [];
keys(L) -> lists:usort([K || {K,_} <- L]).

diff(undefined, undefined) -> [];
diff(undefined, L2) -> keys(L2);
diff(L1, undefined) -> keys(L1);
diff(L1, L2) -> lists:usort(diff(lists:sort(L1), lists:sort(L2), [])).

diff([], [], Acc) -> Acc;
diff(L, [], Acc) -> keys(L) ++ Acc;
diff([], L, Acc) -> keys(L) ++ Acc;
diff([H|As],[H|Bs], Acc) -> diff(As,Bs,Acc);
diff([{K,_}|As], [{K,_}|Bs], Acc) -> diff(As,Bs,[K|Acc]);
diff([A={K,_}|As], [B|Bs], Acc) when A < B -> diff(As,[B|Bs],[K|Acc]);
diff([A|As], [B={K,_}|Bs], Acc) when A > B -> diff([A|As],Bs,[K|Acc]).

