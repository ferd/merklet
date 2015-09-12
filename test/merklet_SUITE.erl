-module(merklet_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [regression_diff].

regression_diff(_) ->
    T1 = insert_all([{<<1>>,<<1>>},{<<2>>,<<2>>},{<<3>>,<<3>>}]),
    T2 = insert_all([{<<1>>,<<0>>}]),
    ?assertEqual([<<1>>,<<2>>,<<3>>], merklet:diff(T1,T2)),
    ?assertEqual([<<1>>,<<2>>,<<3>>], merklet:diff(T2,T1)).

%%%%%%%%%%%%%%%%
%%% Builders %%%
%%%%%%%%%%%%%%%%
insert_all(KeyVals) -> insert_all(KeyVals, undefined).
insert_all(KeyVals, Tree) -> lists:foldl(fun merklet:insert/2, Tree, KeyVals).
