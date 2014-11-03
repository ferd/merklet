%%% @doc Merkle Trees are a data structures devised especially to find
%%% conflicts or diverging pieces of data between two data sets.
%%%
%%% They're more or less a hybrid between a sparse K-ary tree and a
%%% trie of hash values.
%%%
%%% Each `{Key, Value}' pair gets two hashes: a hash of the key (Hkey), and
%%% a hash of the hashed key and the hashed value (Hash).
%%%
%%% The Hkey is used as the main index and to build a tree. If we have three
%%% hashes with the values `<<213,21,54,...>>', `<<213,33,98,...>>', and
%%% `<<11,45,101,...>>', the resulting tree/trie is:
%%%
%%%                (Root)
%%%                 Inner
%%%                /    \
%%%               /      \
%%%            (11)      (213)
%%%  <<11,45,101,...>>   Inner 
%%%                     /     \
%%%                    /       \
%%%                 (21)       (33)
%%%      <<213,21,54,...>>     <<213,33,98,...>>
%%%
%%% Each of the leaf nodes will contain both hashes, along with a non-hashed
%%% version of the key. Each Inner node contains a hash of all its children's
%%% hash values, and indexes them by the hash byte at the given depth.
%%%
%%% This structure allows to quickly compare for changes in values, missing
%%% nodes, and so on, but more importantly allows to quickly know if the data
%%% sets (or subsets of them) are identical.
%%%
%%% It also allows to do a level-order traversal node-per-node over the network
%%% allowing somewhat efficient diffing.
%%% @end
-module(merklet_zipper).

-define(HASH, sha).
-define(HASHBYTES, 20).

-define(VSN, 1).
-define(UNDEFINED, 0).
-define(INNER, 1).
-define(LEAF, 2).
-define(OFFSETBYTE, 3).
-define(KEYS, 4).
-define(PATH, 5).

-record(leaf, {userkey :: binary(), % user submitted key
               hashkey :: binary(), % hash of the user submitted key
               hash :: binary()}).  % hash(hash(key), hash(value))
-record(inner, {hashchildren :: binary(), % hash of children's hashes
                children :: children(),
                offset :: non_neg_integer()}). % byte the inner node is stored under
-record(zipper, {thread :: [znode()],
                 index :: binary(),
                 state :: clean | dirty, % marks if a re-hash is needed
                 current :: znode()}).

-type offset() :: byte().
-type znode() :: #inner{} | #leaf{}.
-type child() :: {offset(), znode()}.
-type children() :: {Prev::[child()], Next::[child()]}.
%-type zipper() :: #zipper{} | 'undefined'.

-export([insert/3, insert_many/2, delete/2, keys/1]).
-export([diff/2, access/3]).
-export([rewind/1, at/2]).
-export([prev/1,next/1,child/1,parent/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TREE-BUILDING API %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

insert(Key, Value, Tree) ->
    insert(to_leaf(Key, Value), rewind(Tree)).

insert_many([], Tree) -> Tree;
insert_many([{K,V}|T], Tree) -> insert_many(T, insert(K,V,Tree)).

delete(Key, Tree) ->
    delete_leaf(to_leaf(Key, <<>>), rewind(Tree)).

keys(undefined) ->
    [];
keys(Tree) ->
    %% It's easier to avoid the zipper structure for the full
    %% tree traversal there. We would technically not need the
    %% whole rewind operation to recalculate child hashes for
    %% this one, but we'll have to suffer it for simplicity's sake.
    #zipper{current=RootNode} = rewind(Tree),
    lists:usort(raw_keys(RootNode)).

%%%%%%%%%%%%%%%%%%%
%%% DIFFING API %%%
%%%%%%%%%%%%%%%%%%%
%% These navigation functions allow state updates
diff(Tree1, Tree2) ->
    diff(Tree1, fun access/3, Tree2).

access(at, Path, Zipper) ->
    case at(Path, Zipper) of
        undefined -> {undefined, Zipper};
        NewZipper -> {NewZipper#zipper.current, NewZipper}
    end;
access(child_at, Path, Zipper) ->
    case at(Path, Zipper) of
        undefined ->
            {undefined, Zipper};
        NewZipper ->
            {current_offset(NewZipper), NewZipper}
    end;
%% No state update for these!
access(keys, Path, Zipper) ->
    Node = case at(Path, Zipper) of
        undefined -> undefined;
        #zipper{current=Current} -> Current
    end,
    raw_keys(Node);
access({keys, Skip}, Path, Zipper) ->
    Node = case at(Path, Zipper) of
        undefined -> undefined;
        #zipper{current=Current} -> Current
    end,
    raw_keys(Node, Skip).

%access_serialize(at, Path, Zipper) ->
%    NewZipper = at(Path, Zipper),
%    serialize(NewZipper

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PATH-BASED NAVIGATION %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rewind(Zipper) -> at(<<>>, Zipper).

at(_, undefined) -> undefined;
at(Path, Z=#zipper{index=Index}) -> 
    PrefixSize = binary:longest_common_prefix([Path,Index]),
    IndexSize = byte_size(Index),
    ToUnwind = max(0, IndexSize-PrefixSize),
    <<Done:PrefixSize/binary, Todo/binary>> = Path,
    at_level(Done, Todo, at_unwind(ToUnwind, Z)).

at_unwind(0, Z) -> Z;
at_unwind(N, Z) -> at_unwind(N-1, parent(Z)).

at_level(_, _, undefined) -> undefined;
at_level(Done, <<>>, Z=#zipper{index=Done}) -> Z;
at_level(Done, <<ChildAt,Rest/binary>>, Z=#zipper{index = <<Done/binary>>}) ->
    at_level(<<Done/binary, ChildAt>>, Rest, at_sibling(ChildAt, child(Z))).

at_sibling(_, undefined) -> undefined;
at_sibling(N, Z=#zipper{index=Index}) ->
    case seek_direction(N, current_position(Index)) of
        current -> Z;
        next -> at_sibling(N, next(Z));
        prev -> at_sibling(N, prev(Z))
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ZIPPER MOVEMENT FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prev(#zipper{thread=[]}) -> undefined;
prev(#zipper{thread=[#inner{children= {[],_}}|_]}) -> undefined;
prev(Z=#zipper{thread=[I=#inner{children = Siblings}|Thread],
               index = Index, current = Current}) ->
    {[{_, NewCurrent}|Prev], Next} = Siblings,
    Offset = hash_offset(byte_size(Index)-1, Current),
    NewSiblings = {Prev, [{Offset, Current} | Next]},
    Z#zipper{thread = [I#inner{children=NewSiblings}|Thread],
             index = index_add(Index, -1), current = NewCurrent}.

next(#zipper{thread=[]}) -> undefined;
next(#zipper{thread=[#inner{children = {_,[]}}|_]}) -> undefined;
next(Z=#zipper{thread=[I=#inner{children = Siblings}|Thread],
               index = Index, current = Current}) ->
    {Prev, [{_, NewCurrent}|Next]} = Siblings,
    Offset = hash_offset(byte_size(Index)-1, Current),
    NewSiblings = {[{Offset, Current}|Prev], Next},
    Z#zipper{thread = [I#inner{children=NewSiblings}|Thread],
             index = index_add(Index, +1), current = NewCurrent}.

child(#zipper{current = #leaf{}}) -> undefined;
child(#zipper{current=#inner{children={_,[]}}}) -> undefined;
child(Z=#zipper{thread = Thread, index = Index,
                current = I=#inner{children=Children}}) ->
    {Prev, [{_, NewCurrent}|Next]} = Children,
    NewIndex = <<Index/binary, (length(Prev))>>,
    Z#zipper{thread = [I#inner{children={Prev,Next}}|Thread],
             index = NewIndex, current = NewCurrent}.

parent(#zipper{thread = []}) -> undefined;
parent(Z=#zipper{thread = [I=#inner{children = {Prev,Next}} | Thread],
                 state = State, index = Index, current = Node}) ->
    PrefixLength = byte_size(Index)-1,
    <<NewIndex:PrefixLength/binary, _>> = Index,
    Byte = hash_offset(PrefixLength, Node),
    Children = {Prev, [{Byte, Node}|Next]},
    HashChildren = case State of
        clean -> I#inner.hashchildren;
        dirty -> children_hash(Children)
    end,
    NewState = case Thread of
        [] -> clean;
        [_|_] -> State
    end,
    Z#zipper{thread = Thread, index=NewIndex, state=NewState,
             current = I#inner{children = Children, hashchildren=HashChildren}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PRIVATE TREE BUILDING %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% if the tree is empty, just use the leaf as a new tree.
insert(Leaf, undefined) ->
    root(Leaf);
%% If the node inserted already exists, do nothing. This case is important
%% because it may leave the tree in a clean state.
insert(Leaf, Z=#zipper{current=Leaf}) ->
    Z;
%% If the offset is at the max value for the hash, return the leaf --
%% We can't go deeper anyway.
insert(Leaf, Z=#zipper{index=Bin}) when byte_size(Bin) =:= ?HASHBYTES ->
    Z#zipper{current=Leaf, state=dirty};
%% if the current node of the tree is a leaf and both keys are the same,
%% replace it.
insert(Leaf=#leaf{hashkey=Key}, Z=#zipper{current=#leaf{hashkey=Key}}) ->
    Z#zipper{current=Leaf};
%% If the current node of the tree is a leaf, and the keys are different,
%% turn the current leaf into an inner node, and insert the new one in it
insert(NewLeaf=#leaf{}, Z=#zipper{current=OldLeaf=#leaf{}, index=Index}) ->
    Offset = byte_size(Index),
    insert(NewLeaf, Z#zipper{current=to_inner(Offset,OldLeaf), state=dirty});
%% Insert to an inner node!
insert(Leaf=#leaf{hashkey=Key},
       Z=#zipper{thread=Thread, index=Index, current=I=#inner{children=Children}}) ->
    ChildOffset = byte_size(Index),
    Byte = binary:at(Key, ChildOffset),
    case find_child(Byte, Children) of
        {not_found, {Prev,Next}} ->
            Z#zipper{thread = [I#inner{children={Prev,Next}}|Thread],
                     state = dirty,
                     index = <<Index/binary, (length(Prev))>>,
                     current = Leaf};
        {ok, {Prev, Next}, Child} ->
            insert(Leaf,
                   Z#zipper{thread = [I#inner{children={Prev,Next}}|Thread],
                            %state = dirty,
                            index = <<Index/binary, (length(Prev))>>,
                            current = Child})
    end.

%% Not found or empty tree. Leave as is.
delete_leaf(_, undefined) ->
    undefined;
%% The entire tree is the leaf. Undefine the tree
delete_leaf(#leaf{hashkey=K}, #zipper{current = #leaf{hashkey=K}, thread=[]}) ->
    undefined;
%% Different leaf at the root.
delete_leaf(#leaf{}, Z=#zipper{thread=[], current = #leaf{}}) ->
    Z;
%% If we got an inner node, dig in.
delete_leaf(Leaf=#leaf{hashkey = K},
            Z=#zipper{current = I=#inner{children=Children},
                      thread = Thread, index=Index}) ->
    Byte = binary:at(K, byte_size(Index)-1),
    case find_child(Byte, Children) of
        {not_found, _} -> % not found, leave as is
            Z;
        %% Can't be one of these if we properly shrink
        {ok, {[], []}, #leaf{}} ->
            error(bad_shrinking);  
        {ok, {[{_,#inner{}}], []}, #leaf{}} ->
            error(bad_shrinking);  
        {ok, {[], [{_,#inner{}}]}, #leaf{}} ->
            error(bad_shrinking);
        %% Shrinkable cases -- only one child leaf node is left after
        %% deleting another leaf. We promote that leaf one level and kill
        %% the current inner node.
        {ok, {[], [{_,Child=#leaf{}}]}, #leaf{}} ->
            Z#zipper{current = Child, state = dirty};
        {ok, {[{_,Child=#leaf{}}], []}, #leaf{}} ->
            Z#zipper{current = Child, state = dirty};
        %% Found the leaf, many children left. We can return the
        %% new set of children
        {ok, NewChildren, #leaf{}} ->
            Z#zipper{current = I#inner{children = NewChildren}, state = dirty};
        %% Explore recursively
        {ok, NewChildren, Inner} ->
            delete_leaf(Leaf,
                        Z#zipper{thread = [I#inner{children = NewChildren}|Thread],
                                 current = Inner})
    end.

raw_keys(undefined) ->
    [];
raw_keys(#leaf{userkey=Key}) ->
    [Key];
raw_keys(#inner{children={Prev,Next}}) ->
    lists:append(lists:foldl(
        fun({_,Node}, Acc) -> [raw_keys(Node)|Acc] end,
        [],
        Prev ++ Next
    )).

%% Same as raw_keys/1, but ignores a given hash
raw_keys(undefined, _) ->
    [];
raw_keys(#leaf{hash=Hash}, Hash) ->
    [];
raw_keys(#leaf{userkey=Key}, _) ->
    [Key];
raw_keys(#inner{children={Prev, Next}}, ToSkip) ->
    lists:append(lists:foldl(
        fun({_, Node}, Acc) -> [raw_keys(Node, ToSkip)|Acc] end,
        [],
        Prev ++ Next
    )).

root(Node) ->
    #zipper{thread = [],
            index = <<>>,
            state = clean,
            current = Node}.

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

to_inner(0, Child=#leaf{hashkey=Hash}) ->
    Children = {[],[{binary:at(Hash,0), Child}]},
    #inner{hashchildren=children_hash(Children),
           children=Children,
           offset=top};
to_inner(Offset, Child=#leaf{hashkey=Hash}) ->
    Children = {[],[{binary:at(Hash,Offset), Child}]},
    #inner{hashchildren=children_hash(Children),
           children=Children,
           offset=binary:at(Hash,Offset-1)}.

%% Empty child set
find_child(_, {[], []}) ->
    {not_found, {[], []}};
%% Found the right one
find_child(Byte, {Prev, [{Byte, Child}|Next]}) ->
    {ok, {Prev, Next}, Child};
%% Forward search
find_child(Byte, {[], [{NextByte, NextChild}|Next]=FullNext}) ->
    if Byte > NextByte ->
           find_child(Byte, {[{NextByte,NextChild}], Next});
       Byte < NextByte ->
           {not_found, {[],FullNext}}
    end;
%% Backwards Search
find_child(Byte, {[{PrevByte, PrevChild}|Prev]=FullPrev, []}) ->
    if Byte < PrevByte ->
           find_child(Byte, {Prev, [{PrevByte,PrevChild}]});
       Byte > PrevByte ->
           {not_found, {FullPrev,[]}}
    end;
%% We implement a two-way search, where we might be halfway
%% through a zipped dictionary and might be headed either way
find_child(Byte, {[{PrevByte,PrevChild}|Prev]=FullPrev,
                    [{NextByte,NextChild}|Next]=FullNext}) ->
    if Byte > PrevByte, Byte < NextByte ->
           {not_found, {FullPrev,FullNext}};
       Byte > NextByte ->
           find_child(Byte, {[{NextByte,NextChild}|FullPrev], Next});
       Byte =< PrevByte ->
           find_child(Byte, {Prev, [{PrevByte,PrevChild}|FullNext]})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PRIVATE PATH-BASED NAVIGATION %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
current_position(Index) ->
    binary:last(Index).

seek_direction(Target, Target) -> current;
seek_direction(Target, Current) when Target > Current -> next;
seek_direction(Target, Current) when Target < Current -> prev.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PRIVATE ZIPPER MOVEMENT %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
index_add(Index, Val) ->
    PrefixSize = byte_size(Index)-1,
    <<Prefix:PrefixSize/binary, Last>> = Index,
    <<Prefix/binary, (Last+Val)>>.

%%%%%%%%%%%%%%%%%%%%%%%
%%% PRIVATE DIFFING %%%
%%%%%%%%%%%%%%%%%%%%%%%
diff(Zipper, Fun, FunState) ->
    Root = case rewind(Zipper) of
        #zipper{current=Current} -> Current;
        undefined -> undefined
    end,
    {Res, NewState} = Fun(at, <<>>, FunState),
    lists:usort(raw_diff(Root, Res, Fun, <<>>, NewState)).

current_offset(#zipper{index=Index, current=Current}) ->
    {hash_offset(byte_size(Index)-1, Current), Current}.

raw_diff(undefined, undefined, _, _, _) ->
    [];
raw_diff(undefined, _Tree2, Fun, Path, FunState) ->
    Fun(keys, Path, FunState);
raw_diff(Tree1, undefined, _, _, _) ->
    raw_keys(Tree1);
%% if hashes are the same, we're done
raw_diff(#leaf{hash=Hash}, #leaf{hash=Hash}, _, _, _) ->
    [];
raw_diff(#leaf{hash=Hash}, #inner{hashchildren=Hash}, _, _, _) ->
    [];
raw_diff(#inner{hashchildren=Hash}, #leaf{hash=Hash}, _, _, _) ->
    [];
raw_diff(#inner{hashchildren=Hash}, #inner{hashchildren=Hash}, _, _, _) ->
    [];
%% if they differ and both nodes are leaf nodes, return both values
raw_diff(#leaf{userkey=Key1}, #leaf{userkey=Key2}, _, _, _) ->
    [Key1,Key2];
%% if both differ but one is an inner node, return everything
raw_diff(#leaf{hash=ToSkip}, #inner{}, Fun, Path, FunState) ->
    %% We can only get rid of the current Key if the hashes differ
    Fun({keys, ToSkip}, Path, FunState);
raw_diff(Inner=#inner{}, #leaf{hash=ToSkip}, _, _, _) ->
    %% We can only get rid of the current Key if the hashes differ
    raw_keys(Inner, ToSkip);
%% if both nodes are inner and populated, compare them offset by offset.
raw_diff(#inner{children=Children}, #inner{}, Fun, Path, FunState) ->
    ChildPath = <<Path/binary, 0>>,
    {Res, NewState} = Fun(child_at, ChildPath, FunState),
    diff_offsets(children_offsets(Children), Res, Fun, ChildPath, NewState).

%% Whatever is left alone is returned
diff_offsets([], undefined, _, _, _) ->
    [];
diff_offsets(List, undefined, _, _, _) ->
    io:format("2: ~p~n",[lists:append([raw_keys(Child) || {_, Child} <- List])]),
    lists:append([raw_keys(Child) || {_, Child} <- List]);
diff_offsets([], _, Fun, Path, FunState) ->
    NextPath = next_child_path(Path),
    Keys = Fun(keys, Path, FunState),
    io:format("3: ~p~n",[Keys]),
    {Res, NewState} = Fun(child_at, Path, FunState),
    Keys ++ diff_offsets([], Res, Fun, NextPath, NewState);
%% If both offsets are the same, compare recursively.
diff_offsets(L=[{OffL, Child}|Rest], R={OffR,Node}, Fun, Path, FunState) ->
    io:format("4- ~p~n",[{raw_keys(Child), Fun(keys,Path,FunState)}]),
    NextPath = next_child_path(Path),
    if OffL =:= OffR ->
            RawDiff = raw_diff(Child, Node, Fun, Path, FunState),
            {Res, NewState} = Fun(child_at, NextPath, FunState),
            RawDiff ++ diff_offsets(Rest, Res, Fun, NextPath, NewState);
       OffL < OffR ->
            io:format("4.2 ~p~n", [raw_keys(Child)]),
            raw_keys(Child) ++ 
            diff_offsets(Rest, R, Fun, Path, FunState);
       OffL > OffR ->
            Keys = Fun(keys, Path, FunState),
            io:format("4.3 ~p~n", [Keys]),
            {Res, NewState} = Fun(child_at, NextPath, FunState),
            Keys ++ diff_offsets(L, Res, Fun, NextPath, NewState)
    end.

next_child_path(Path) ->
    index_add(Path, 1).
%    ParentSize = byte_size(Path) - 1,
%    <<ParentPath:ParentSize/binary, ChildByte>> = Path,
%    <<ParentPath/binary, (ChildByte+1)>>.

children_offsets({Prev, Next}) -> lists:reverse(Prev, Next).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PRIVATE (GENERAL) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%
hash_offset(Pos, #leaf{hashkey=Hash}) -> binary:at(Hash, Pos);
hash_offset(_, #inner{offset=Byte}) -> Byte.

children_hash({Prev,Next}) ->
    Hashes = lists:sort(
        [case Child of
             #inner{hashchildren=HashChildren} -> HashChildren;
             #leaf{hash=Hash} -> Hash
         end || {_Offset, Child} <- Prev ++ Next]
    ),
    crypto:hash_final(lists:foldl(fun(K, H) -> crypto:hash_update(H, K) end,
                                  crypto:hash_init(?HASH),
                                  Hashes)).
