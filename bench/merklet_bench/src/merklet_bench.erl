-module(merklet_bench).

%% API exports
-export([build/1, catch_up_diff/1, rewrite_diff/1]).

%%====================================================================
%% API functions
%%====================================================================
build([N, Tries]) ->
    Pairs = pairs(N),
    BO = eministat:s("build-old", fun() -> build(merklet_old, Pairs) end, Tries),
    BN = eministat:s("build-new", fun() -> build(merklet, Pairs) end, Tries),
    BA = eministat:s("build-array", fun() -> build(merklet_array, Pairs) end, Tries),
    eministat:x(95.0, BO, BN),
    eministat:x(95.0, BO, BA),
    eministat:x(95.0, BN, BA).

catch_up_diff([N, Pct, Tries]) ->
    Total = N+round((Pct/100)*N),
    Full = pairs(Total),
    Partial = lists:sublist(Full,N),
    FullOld = build(merklet_old, Full),
    PartialOld = build(merklet_old, Partial),
    FullNew = build(merklet, Full),
    PartialNew = build(merklet, Partial),
    FullArray = build(merklet_array, Full),
    PartialArray = build(merklet_array, Partial),
    CO = eministat:s("catch-up-diff-old", fun() -> diff(merklet_old, FullOld, PartialOld) end, Tries),
    CN = eministat:s("catch-up-diff-new", fun() -> diff(merklet, FullNew, PartialNew) end, Tries),
    CA = eministat:s("catch-up-diff-array", fun() -> diff(merklet_array, FullArray, PartialArray) end, Tries),
    eministat:x(95.0, CO, CN),
    eministat:x(95.0, CO, CA),
    eministat:x(95.0, CN, CA).

rewrite_diff([N, Pct, Tries]) ->
    Rewrite = round((Pct/100)*N),
    Full = pairs(N),
    Partial = [{X, <<"someval">>} || {X, _} <- pairs(Rewrite)] ++ lists:nthtail(Rewrite, Full),
    FullOld = build(merklet_old, Full),
    PartialOld = build(merklet_old, Partial),
    FullNew = build(merklet, Full),
    PartialNew = build(merklet, Partial),
    FullArray = build(merklet_array, Full),
    PartialArray = build(merklet_array, Partial),
    CO = eministat:s("catch-up-diff-old", fun() -> diff(merklet_old, FullOld, PartialOld) end, Tries),
    CN = eministat:s("catch-up-diff-new", fun() -> diff(merklet, FullNew, PartialNew) end, Tries),
    CA = eministat:s("catch-up-diff-array", fun() -> diff(merklet_array, FullArray, PartialArray) end, Tries),
    eministat:x(95.0, CO, CN),
    eministat:x(95.0, CO, CA),
    eministat:x(95.0, CN, CA).

%%====================================================================
%% Internal functions
%%====================================================================
pairs(N) ->
    %% Generate `N' key/vals and build a tree
    [{BinTerm, crypto:hash(sha, BinTerm)}
     || X <- lists:seq(1,N),
        BinTerm <- [term_to_binary(X)]].

build(Mod, Pairs) ->
    Mod:insert_many(Pairs, undefined).

diff(Mod, A, B) ->
    Mod:diff(A,B).
