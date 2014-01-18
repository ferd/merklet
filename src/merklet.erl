-module(merklet).

-record(leaf, {userkey :: binary(), % user submitted key
               hashkey :: binary(), % hash of the user submitted key
               hash :: binary()}).  % hash(hash(key), hash(value))
-record(inner, {hashchildren :: binary(), % hash of children's hashes
                children :: [{offset(), #inner{} | #leaf{}}, ...],
                offset :: non_neg_integer()}). % byte offset

-type offset() :: byte().
-type leaf() :: #leaf{}.
-type inner() :: #inner{}.

-type tree() :: leaf() | inner() | 'undefined'.
-type key() :: binary().
-type value() :: binary().
-type path() :: binary().
-type access_fun() :: fun((at | child_at | keys, path()) -> tree()).
-type serial_fun() :: fun((at | child_at | keys, path()) -> binary()).

-export_type([tree/0, key/0, value/0, path/0, access_fun/0, serial_fun/0]).
-export([insert/2, delete/2, keys/1, diff/2]).
-export([dist_diff/2, access_serialize/1, access_unserialize/1]).

-define(HASH, sha).
-define(HASHBYTES, 20).

-define(VSN, 1).
-define(UNDEFINED, 0).
-define(INNER, 1).
-define(LEAF, 2).
-define(OFFSETBYTE, 3).
-define(KEYS, 4).
-define(PATH, 5).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%

%% @doc Adds a key to the tree, or overwrites an exiting one.
-spec insert({key(), value()}, tree()) -> tree().
insert({Key, Value}, Tree) ->
    insert(0, to_leaf(Key, Value), Tree).

%% @doc Removes a key from a tree, if present.
-spec delete(key(), tree()) -> tree().
delete(Key, Tree) ->
    delete_leaf(to_leaf(Key, <<>>), Tree).

%% @doc Returns a sorted list of all the keys in the tree
-spec keys(tree()) -> [key()].
keys(Tree) ->
    lists:usort(raw_keys(Tree)).

%% @doc Takes two trees and returns the different keys between them.
-spec diff(tree(), tree()) -> [key()].
diff(Tree1, Tree2) ->
    %% We use the remote access for this local comparison. This is
    %% slower than a dedicated traversal algorithm, but less code
    %% means fewer chances of breaking stuff.
    Fun = access_local(Tree2),
    diff(Tree1, Fun, <<>>).

%% @doc Takes a local tree, and an access function to another tree,
%% and returns the keys associated with diverging parts of both trees.
%% The access fun takes an atom and a path and must retourn a flat tree
%% node or a subtree. (`fun(Verb, Path) -> Node | undefined').
%%
%% The Path is a sequence of bytes (in a `binary()') telling how to get to
%% a specific node:
%%
%% - `<<>>' means returning the current node, at whatever point we are in the
%%   tree's traversal.
%% - Whenever we hit a leaf node during traversal, it is returned.
%% - `<<Offset,...>>' means to return the node at the given offset for the
%%   current tree level. For example, a value of `<<0>>' means to return the
%%   leftmost child of the current node, whereas `<<3>>' should return the
%%   4th leftmost child. Any time the path is larger than the number of
%%   children, we return `undefined'.
%%   This is the case where we can recurse.
%%
%% The three terms required are:
%% - `at': Uses the path as above to traverse the tree and return a node.
%% - `keys': Returns all the keys held (recursively) by the node at a given
%%   path.
%% - `child_at': Special case of `at' used when comparing child nodes of two
%%   inner nodes. Basically the same as `at', but with one new rule:
%%
%%     Whenever we hit a path that is `<<N>>' and we are on an inner node,
%%     it means we only have a child to look at. Return that child along
%%     with its byte at the offset in the dictionary structure
%%     (`{ByteAtOffset, Node}').
%%
%% The values returned are all the keys that differ across both trees.
-spec dist_diff(tree(), access_fun()) -> [key()].
dist_diff(Tree, Fun) when is_function(Fun,2) ->
    diff(Tree, Fun, <<>>).

%% @doc Returns an `access_fun()' for the current tree. This function
%% can be put at the end of a connection to a remote node, and it
%% will return serialized tree nodes.
-spec access_serialize(tree()) -> serial_fun().
access_serialize(Tree) ->
    fun(at, Path) -> serialize(at(Path, Tree));
       (child_at, Path) -> serialize(child_at(Path, Tree));
       (keys, Path) -> serialize(raw_keys(at(Path, Tree)))
    end.

%% @doc Takes an {@link access_fun()} that fetches nodes serialized according
%% to the format used by {@link access_serialize/2}, and returns a new {@link
%% access_fun()} that will unserialized and can be used directly in
%% {@link dist_diff/2}
-spec access_unserialize(serial_fun()) -> access_fun().
access_unserialize(Fun) ->
    fun(Arg, Path) -> unserialize(Fun(Arg,Path)) end.

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

%% if the tree is empty, just use the leaf
insert(_Offset, Leaf, undefined) ->
    Leaf;
%% If the offset is at the max value for the hash, return the leaf --
%% We can't go deeper anyway.
insert(?HASHBYTES, Leaf, _) ->
    Leaf;
%% if the current node of the tree is a leaf and both keys are the same,
%% replace it.
insert(_Offset, Leaf=#leaf{hashkey=Key}, #leaf{hashkey=Key}) ->
    Leaf;
%% if the current node of the tree is a leaf, and keys are different, turn the
%% current leaf into an inner node, and insert the new one in it.
insert(Offset, NewLeaf, OldLeaf=#leaf{}) ->
    insert(Offset, NewLeaf, to_inner(Offset, OldLeaf));
%% Insert to an inner node!
insert(Offset, Leaf=#leaf{hashkey=Key}, Inner=#inner{children=Children}) ->
    Byte = binary:at(Key, Offset),
    NewChildren = case orddict:find(Byte, Children) of
        error ->
            orddict:store(Byte, Leaf, Children);
        {ok, Subtree} ->
            orddict:store(Byte, insert(Offset+1, Leaf, Subtree), Children)
    end,
    Inner#inner{hashchildren=children_hash(NewChildren), children=NewChildren}.

%% Not found or empty tree. Leave as is.
delete_leaf(_, undefined) ->
    undefined;
%% If we have the same leaf node we were looking for, kill it.
delete_leaf(#leaf{hashkey=K}, #leaf{hashkey=K}) ->
    undefined;
%% If it's a different leaf, the item to delete is already gone. Leave as is.
delete_leaf(#leaf{}, Leaf=#leaf{}) ->
    Leaf;
%% if it's an inner node, look inside
delete_leaf(Leaf=#leaf{hashkey=K}, Inner=#inner{offset=Offset, children=Children}) ->
    Byte = binary:at(K, Offset),
    case orddict:find(Byte, Children) of
        error -> % not found, leave as is
            Inner;
        {ok, Subtree} ->
            NewChildren = case maybe_shrink(delete_leaf(Leaf, Subtree)) of
                undefined -> % leaf gone
                    orddict:erase(Byte, Children);
                Node -> % replacement node
                    orddict:store(Byte, Node, Children)
            end,
            maybe_shrink(Inner#inner{hashchildren=children_hash(NewChildren),
                                     children=NewChildren})
    end.

raw_keys(undefined) ->
    [];
raw_keys(#leaf{userkey=Key}) ->
    [Key];
raw_keys(#inner{children=Children}) ->
    lists:append(orddict:fold(
        fun(_Byte, Node, Acc) -> [raw_keys(Node)|Acc] end,
        [],
        Children
    )).


-spec diff(tree(), access_fun(), path()) -> [key()].
diff(Tree, Fun, Path) ->
    lists:usort(raw_diff(Tree, Fun(at, Path), Fun, Path)).

%% Empty trees yield all keys of remaining trees
raw_diff(undefined, undefined, _, _) ->
    [];
raw_diff(undefined, _Tree2, Fun, Path) ->
    Fun(keys, Path);
raw_diff(Tree1, undefined, _, _) ->
    raw_keys(Tree1);
%% If hashes are the same, we're done.
raw_diff(#leaf{hash=Hash}, #leaf{hash=Hash}, _, _) ->
    [];
raw_diff(#leaf{hash=Hash}, #inner{hashchildren=Hash}, _, _) ->
    [];
raw_diff(#inner{hashchildren=Hash}, #leaf{hash=Hash}, _, _) ->
    [];
raw_diff(#inner{hashchildren=Hash}, #inner{hashchildren=Hash}, _, _) ->
    [];
%% if they differ and both nodes are leaf nodes, return both values
raw_diff(#leaf{userkey=Key1}, #leaf{userkey=Key2}, _, _) ->
    [Key1,Key2];
%% if both differ but one is an inner node, return everything
raw_diff(#leaf{userkey=Key}, #inner{}, Fun, Path) ->
    Fun(keys, Path) -- [Key];
raw_diff(Inner=#inner{}, #leaf{userkey=Key}, _, _) ->
    raw_keys(Inner) -- [Key];
%% if both nodes are inner and populated, compare them offset by offset.
raw_diff(#inner{children=Children}, #inner{}, Fun, Path) ->
    ChildPath = <<Path/binary, 0>>,
    diff_offsets(children_offsets(Children),
                 Fun(child_at, ChildPath),
                 Fun,
                 ChildPath).

%% Whatever is left alone is returned
diff_offsets([], undefined, _, _) ->
    [];
diff_offsets(List, undefined, _, _) ->
    lists:append([raw_keys(Child) || {_, Child} <- List]);
diff_offsets([], _, Fun, Path) ->
    NextPath = next_child_path(Path),
    Fun(keys, Path) ++ diff_offsets([], Fun(child_at, NextPath), Fun, NextPath);
%% If both offsets are the same, compare recursively.
diff_offsets(L=[{OffL, Child}|Rest], R={OffR,Node}, Fun, Path) ->
    NextPath = next_child_path(Path),
    if OffL =:= OffR ->
            raw_diff(Child, Node, Fun, Path) ++
            diff_offsets(Rest, Fun(child_at, NextPath), Fun, NextPath);
       OffL < OffR ->
            raw_keys(Child) ++ 
            diff_offsets(Rest, R, Fun, Path);
       OffL > OffR ->
            Fun(keys, Path) ++
            diff_offsets(L, Fun(child_at, NextPath), Fun, NextPath)
    end.

next_child_path(Path) ->
    ParentSize = byte_size(Path) - 1,
    <<ParentPath:ParentSize/binary, ChildByte>> = Path,
    <<ParentPath/binary, (ChildByte+1)>>.

%%% Basic Tree Management Functions

%% @doc Takes a Key and a Value and turns them to a leaf node.
-spec to_leaf(key(), value()) -> leaf().
to_leaf(Key, Value) when is_binary(Key) ->
    %% We use the hash of the value as part of the 'hash' entry,
    %% but not the 'hashkey'. This allows a tree where the structure
    %% is based on the keys, but we can still compare and use both
    %% the key and its value to do comparison when diffing.
    HashKey = crypto:hash(?HASH, Key),
    HashVal = crypto:hash(?HASH, Value),
    #leaf{userkey=Key,
          hashkey=HashKey,
          hash=crypto:hash(?HASH, <<HashKey/binary, HashVal/binary>>)}.

%% @doc We build a Key-Value list of the child nodes and their offset
%% to be used as a sparse K-ary tree.
-spec to_inner(offset(), leaf()) -> inner().
to_inner(Offset, Child=#leaf{hashkey=Hash}) ->
    Children = orddict:store(binary:at(Hash, Offset), Child, orddict:new()),
    #inner{hashchildren=children_hash(Children),
           children=Children,
           offset=Offset}.

%% @doc The hash for the node is put together in a somewhat portable way:
%% fetch the hashes for all the children, sort them by the value
%% they would yield for each byte being converted as an integer, and
%% then apply the ?HASH to the entire sequence in that order.
%%
%% We use the 'hash' value for leaf nodes so that comparison can be done
%% while caring about both keys and values. This has no impact on position
%% of inner nodes, because it is dictated by the children's keyhashes, and
%% not the inner node's own hashes.
%% @todo consider endianness for absolute portability
-spec children_hash([{offset(), leaf()}, ...]) -> binary().
children_hash(Children) ->
    Hashes = lists:sort(
        [case Child of
             #inner{hashchildren=HashChildren} -> HashChildren;
             #leaf{hash=Hash} -> Hash
         end || {_Offset, Child} <- Children]
    ),
    crypto:hash_final(lists:foldl(fun(K, H) -> crypto:hash_update(H, K) end,
                                  crypto:hash_init(?HASH),
                                  Hashes)).

%% @doc Checks if the node can be shrunken down to a single leaf it contains
%% or should just be returned as is.
%% This avoids a problem where a deleted subtree results in an inner node
%% with a single element, which wastes space and can slow down diffing.
maybe_shrink(Leaf = #leaf{}) ->
    Leaf;
maybe_shrink(undefined) ->
    undefined;
maybe_shrink(Inner = #inner{children=Children}) ->
    %% The trick for this one is that if we have *any* child set that
    %% is anything else than a single leaf node, we can't shrink. We use
    %% a fold with a quick try ... catch to quickly figure this out, in
    %% two iterations at most.
    try
        orddict:fold(fun(_Offset, Leaf=#leaf{}, 0) -> Leaf;
                        (_, _, _) -> throw(false)
                     end, 0, Children)
    catch
        throw:false -> Inner
    end.

%% @doc Returns the sorted offsets of a given child. Because we're using
%% orddicts, we can just straight up return the children as is, but another
%% data structure would need to transform them into a key/value list sorted
%% by the offset: [{Offet, ChildNode}].
children_offsets(Children) -> Children.

%% Wrapper for the diff function.
access_local(Node) ->
    fun(at, Path) -> at(Path, Node);
       (child_at, Path) -> child_at(Path, Node);
       (keys, Path) -> raw_keys(at(Path, Node))
    end.

%% Return the node at a given position in a tree.
at(Path, Tree) ->
    case child_at(Path, Tree) of
        {_Off, Node} -> Node;
        Node -> Node
    end.

%% Special case of at/2 that returns the node at a given position in a tree,
%% but if the resulting node is the child of an inner node, return it with
%% its indexed offset.
%% This allows to diff inner nodes without contextual info while in the
%% offset traversal.
child_at(<<>>, Node) ->
    %% End of path, return whatever
    Node;
child_at(<<N,Rest/binary>>, #inner{children=Children}) ->
    %% Depending on the path depth, the behavior changes. If the path depth
    %% left is of one (i.e. `<<N>> = <<N,Rest/binary>>') and that we are in
    %% an inner node, then we're looking for the child definition as
    %% `{ByteAtOffset, ChildNode}'.
    %% Otherwise, this is how we keep recursing by looking deeper in a
    %% inner node.
    %% If the path goes past what the node contains, we return `undefined'.
    try
        orddict:fold(fun(Off, Node, 0) when Rest =:= <<>> -> throw({Off,Node});
                        (_, Node, 0) -> throw(Node);
                        (_, _, X) -> X-1
                     end, N, Children),
        undefined
    catch
        throw:{Off,Node} -> {Off,Node};
        throw:Node -> child_at(Rest, Node)
    end;
%% Always stop at a leaf.
child_at(_, Node=#leaf{}) ->
    Node.

%% Serialize nodes flatly. All terms are self-contained and their
%% trailing value can be used as one blob. A protocol using this format
%% of serialization should therefore frame each binary before concatenating
%% them.
%%
%% Note that this format is sufficient for diffing, but not to rebuild entire
%% trees from scratch.
serialize(undefined) ->
    <<?VSN, ?UNDEFINED>>;
serialize(#leaf{userkey=Key, hashkey=HKey, hash=Hash}) ->
    <<?VSN, ?LEAF, ?HASHBYTES:32, HKey/binary, Hash/binary, Key/binary>>;
serialize(#inner{hashchildren=Hash}) ->
    <<?VSN, ?INNER, ?HASHBYTES:32, Hash/binary>>;
serialize({Offset, Node}) when is_record(Node, leaf); is_record(Node, inner) ->
    <<?VSN, ?OFFSETBYTE, Offset, (serialize(Node))/binary>>;
serialize(Keys) when is_list(Keys) ->
    Serialized = << <<(byte_size(Key)):16, Key/binary>> || Key <- Keys >>,
    <<?VSN, ?KEYS, (length(Keys)):16, Serialized/binary>>.

%% Deserialize nodes flatly. Assume self-contained binaries.
%%
%% Note that this format is sufficient for diffing, but not to rebuild entire
%% trees from scratch.
unserialize(<<?VSN, ?UNDEFINED>>) ->
    undefined;
unserialize(<<?VSN, ?LEAF, ?HASHBYTES:32, HKey:?HASHBYTES/binary, 
              Hash:?HASHBYTES/binary, Key/binary>>) ->
    #leaf{userkey=Key, hashkey=HKey, hash=Hash};
unserialize(<<?VSN, ?INNER, ?HASHBYTES:32, Hash:?HASHBYTES/binary>>) ->
    #inner{hashchildren=Hash};
unserialize(<<?VSN, ?OFFSETBYTE, Byte, Node/binary>>) ->
    {Byte, unserialize(Node)};
unserialize(<<?VSN, ?KEYS, NumKeys:16, Serialized/binary>>) ->
    Keys = [Key || <<Size:16, Key:Size/binary>> <= Serialized],
    NumKeys = length(Keys),
    Keys.

