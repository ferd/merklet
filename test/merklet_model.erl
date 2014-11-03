-module(merklet_model).
-export([insert/3, insert_many/2, delete/2, keys/1]).

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
