# Merklet #

Reimplementation of Riak's old merkle tree module, but in a more readable
manner (according to me). Other difference include:

- Renaming some functions just because
- Insertion wants only binaries for keys and values, but makes no assumption
  with regards to hashing.
- Deleting nodes doesn't leave empty inner nodes as part of the tree, and
  inner nodes with a single child node see the child promoted to the current
  level.
- Slightly more efficient diffing. This is due to not having as many leftover
  inner nodes, and using difference lists on offsets of two inner nodes instead
  of iterating across all offsets. I.e. the cost is linear with the number of
  children of an inner node, rather than a flat minimal 255 rounds for each.
- The implementation compares key sets, but not values yet, making it useless
  until this is fixed.

Further documentation (and changes) to come.

Todo list:

- I'd like to add a serialization function that would be more or less
  language-agnostic.
- I'd like to add a functionality that is more protocol-like and would
  make it possible to do incremental tree diffing over the network, as
  opposed to the current one, which requires both trees to fully be on the
  node.

## Compiling ##

    $ rebar compile

## Running Tests

    $ rebar get-deps compile --config test.rebar.config
    $ rebar eunit --config test.rebar.config skip_deps=true

## Usage

```
1> T4 = merklet:insert({term_to_binary("wf"), <<"hehe">>}, T3 = merklet:insert({term_to_binary("c"), <<"third">>}, T2 = merklet:insert({<<"b">>, <<"other">>}, T1 = merklet:insert({<<"a">>, <<"val">>}, T0 = undefined)))).
{inner,<<187,102,122,111,219,4,33,214,19,0,236,157,96,173,
         61,138,72,147,222,29>>,
       [{134,
         {leaf,<<"a">>,
               <<134,247,228,55,250,165,167,252,225,93,29,220,185,234,
                 234,234,55,118,103,184>>,
               <<57,246,156,39,143,70,22,84,71,243,13,16,172,245,66,
                 119,170,163,213,...>>}},
        {211,
         {inner,<<182,193,131,122,12,111,38,40,73,84,96,221,226,
                  186,198,111,197,136,109,171>>,
                [{29,
                  {leaf,<<131,107,0,1,99>>,
                        <<211,29,241,167,251,188,205,162,69,235,0,101,90,...>>,
                        <<52,251,51,0,185,167,123,235,220,152,142,195,...>>}},
                 {191,
                  {leaf,<<131,107,0,2,119,102>>,
                        <<211,191,171,22,1,47,67,156,204,229,77,113,...>>,
                        <<66,82,91,182,211,176,220,6,187,120,174,...>>}}],
                1}},
        {233,
         {leaf,<<"b">>,
               <<233,215,31,94,231,201,45,109,201,233,47,253,173,23,184,
                 189,73,65,...>>,
               <<208,148,30,104,218,143,56,21,31,248,106,97,252,89,
                 247,197,207,...>>}}],
       0}
2> merklet:delete(term_to_binary("wf"), T4).
{inner,<<81,6,255,22,224,211,76,116,234,185,116,193,221,
         248,168,145,125,211,105,135>>,
       [{134,
         {leaf,<<"a">>,
               <<134,247,228,55,250,165,167,252,225,93,29,220,185,234,
                 234,234,55,118,103,184>>,
               <<57,246,156,39,143,70,22,84,71,243,13,16,172,245,66,
                 119,170,163,213,...>>}},
        {211,
         {leaf,<<131,107,0,1,99>>,
               <<211,29,241,167,251,188,205,162,69,235,0,101,90,83,57,
                 132,52,158,32,...>>,
               <<52,251,51,0,185,167,123,235,220,152,142,195,237,208,
                 212,166,164,42,...>>}},
        {233,
         {leaf,<<"b">>,
               <<233,215,31,94,231,201,45,109,201,233,47,253,173,23,184,
                 189,73,65,...>>,
               <<208,148,30,104,218,143,56,21,31,248,106,97,252,89,
                 247,197,207,...>>}}],
       0}
3> merklet:keys(T3).
[<<"a">>,<<"b">>,<<131,107,0,1,99>>]
4> merklet:keys(T4).
[<<"a">>,<<"b">>,<<131,107,0,1,99>>,<<131,107,0,2,119,102>>]
5> merklet:diff(T3,T4).
[<<131,107,0,2,119,102>>]
6> merklet:diff(T3,undefined).
[<<"a">>,<<"b">>,<<131,107,0,1,99>>
```

## Notes ##

The implementation is still incomplete and should not be used while this notice
is here.

More docs to come.
