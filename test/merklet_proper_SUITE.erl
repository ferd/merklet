-module(merklet_proper_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [operations, model].

operations(_Config) ->
    ?assertEqual([], proper:module(prop_merklet, [{numtests,1000}, {to_file, user}])).

model(_Config) ->
    ?assertEqual([], proper:module(prop_model, [{numtests,5000}, {to_file, user}])).
