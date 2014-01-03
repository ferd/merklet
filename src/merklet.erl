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
-type node() :: leaf() | inner().

-opaque tree() :: node() | 'undefined'.
-type key() :: binary().
-type value() :: binary().

-export_type([tree/0, key/0, value/0]).
-export([insert/2, delete/2, keys/1, diff/2]).

-define(HASH, sha).
-define(HASHBYTES, 19). % 0 included

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%

% insert
-spec insert({key(), value()}, tree()) -> tree().
insert({Key, Value}, Tree) ->
    insert(0, to_leaf(Key, Value), Tree).

% delete
-spec delete(key(), tree()) -> tree().
delete(Key, Tree) ->
    delete_leaf(to_leaf(Key, <<>>), Tree).

% keys
-spec keys(tree()) -> [key()].
keys(Tree) ->
    lists:usort(raw_keys(Tree)).

% diff
-spec diff(tree(), tree()) -> [key()].
diff(Tree1, Tree2) ->
    lists:usort(raw_diff(Tree1, Tree2)).
% serialize?

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

%% Empty trees yield all keys of remaining trees
raw_diff(undefined, undefined) -> [];
raw_diff(undefined, Tree2) -> raw_keys(Tree2);
raw_diff(Tree1, undefined) -> raw_keys(Tree1);
%% If hashes are the same, we're done.
raw_diff(#leaf{hash=Hash}, #leaf{hash=Hash}) -> [];
raw_diff(#leaf{hash=Hash}, #inner{hashchildren=Hash}) -> [];
raw_diff(#inner{hashchildren=Hash}, #leaf{hash=Hash}) -> [];
raw_diff(#inner{hashchildren=Hash}, #inner{hashchildren=Hash}) -> [];
%% if they differ and both nodes are leaf nodes, return both values
raw_diff(#leaf{userkey=Key1}, #leaf{userkey=Key2}) -> [Key1,Key2];
%% if both differ but one is an inner node, return everything
raw_diff(#leaf{userkey=Key}, Inner=#inner{}) -> raw_keys(Inner) -- [Key];
raw_diff(Inner=#inner{}, #leaf{userkey=Key}) -> raw_keys(Inner) -- [Key];
%% if both nodes are inner nodes at the same offset and one has no children,
%% return the other's ones (though this case shouldn't happen)
raw_diff(Inner=#inner{offset=Off}, #inner{offset=Off, children=[]}) ->
    raw_keys(Inner);
raw_diff(#inner{offset=Off, children=[]}, Inner=#inner{offset=Off}) ->
    raw_keys(Inner);
%% if both nodes are inner and populated, compare them offset by offset.
raw_diff(#inner{children=Children1}, #inner{children=Children2}) ->
    diff_offsets(children_offsets(Children1), children_offsets(Children2)).

%% Whatever is left alone is returned
diff_offsets([], []) ->
    [];
diff_offsets([{_, Child}|Rest], []) ->
    raw_keys(Child) ++ diff_offsets(Rest, []);
diff_offsets([], [{_, Child}|Rest]) ->
    raw_keys(Child) ++ diff_offsets([], Rest);
%% If both offsets are the same, compare recursively.
diff_offsets([{Offset, Child1}|Rest1], [{Offset, Child2}|Rest2]) ->
    raw_diff(Child1, Child2) ++ diff_offsets(Rest1, Rest2);
diff_offsets(L1=[{Off1, Child1}|Rest1], L2=[{Off2, Child2}|Rest2]) ->
    if Off1 < Off2 -> raw_keys(Child1) ++ diff_offsets(Rest1, L2);
       Off1 > Off2 -> raw_keys(Child2) ++ diff_offsets(L1, Rest2)
    end.

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
