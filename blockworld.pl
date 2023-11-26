% member(X, L) holds when X is a member of the list L.
member(X, [X|_]).
member(X, [_|T]):- member(X, T),!.

% notmember(X, L) holds when X is not a member of the list L.
notmember(_, []).
notmember(X, [H|T]):- X \= H, notmember(X, T),!.

% append(L1, L2, L3) holds when L3 is the result of appending L1 and L2.
append([],L,L).
append([H|T],L2,[H|L3]):- append(T,L2,L3).

% remove(L1, E, L3) holds when L3 is the result of removing E from L1.
remove([], _, []).
remove([H|T], E, [H|L3]):- H \= E, remove(T, E, L3).
remove([H|T], E, L3):- H = E, L3 = T.

% printList([H|T]) holds when head of H|T, H, is printed and printList is recursively called on T

% printList(L) prints the list L.
printList([]):-nl.
printList([H|T]):- write(H), printList(T).

% notequal(X1, X2) holds true when X1 equals X2 and fails when X1 is not equal to X2

% notequal(X1, X2) takes two arguments and holds true when these arguments are not equal.  In other words, it fails when the arguments are equal and otherwise succeeds.
notequal(X, X):-!, fail. % fail, if equal.
notequal(_, _).          % otherwise, succeed.

% substitue(X, Y, [X|T], [Y|T]) holds when the head X of X|T is substiuted with Y to produce list Y|T where Y is the new head

% substitute (E, E1, OLD, NEW) holds when NEW is the list OLD in which E is substituted by E1.  There are no duplicates in OLD or NEW.
substitute(X, Y, [X|T], [Y|T]).  % Here, the head X of [X|T] is substituted by Y to yield the list with the head Y to produce the list [Y|T].  The tails of OLD and NEW are the same, because we seek to substitute only one occurrence.

% substitue(X, Y, T, T1) holds when an element X of H|T is substiuted with Y to produce the list H|T1 where X and Y are elements of T and T1

substitute(X, Y, [H|T], [H|T1]):- 
    substitute(X, Y, T, T1).  % In this clause the element to be substituted is NOT the head of the list, so the head H of the list [H|T] carries over to the head H of the new list, but the tail of the new list is obtained from the tail T of the old list where the element X was substituted by Y, producing the new list [H|T1].


% blocks(X) holds when X is a list of blocks

% defining a predicate which lists all the blocks in your world
% blocks(L) holds when L is a list of blocks.
blocks([a, b, c, d, e]).
% blocks([a, b, c]).

% block(X) holds when X is a member in the list of blocks

% Then a generic block, say X, is a member of the list of blocks
block(X):-
    blocks(BLOCKS),  % this extracts the list BLOCKS
    member(X, BLOCKS).


% moveblockblock(X, Y, Z, S1, S2) holds when a clear block X on block Y in state S1 is moved onto another clear block Z
% state S2 results in block X being clear and on block Z, block Y becoming clear, and block Z becoming no longer clear

% move(X, Y, Z, S1, S2) holds when the state S2 is obtained from the state S1 
% by moving the block X from the block Y onto the block Z.
moveblockblock(X, Y, Z, S1, S2):-
	member([clear, X], S1), %find a clear block X in S1
	member([on, X, Y], S1), block(Y), %find a block on which X sits
	member([clear, Z], S1), X \= Z, %find another clear block, Z
	substitute([on, X, Y], [on, X, Z], S1, INT),  %remove X from Y, place it on Z
	substitute([clear, Z], [clear, Y], INT, S2). % Z is no longer clear; Y is now clear
	
% You must write two more rules for the blocks’ world: 

% moveblocktable(X, Y, S1, S2) holds when clear block X on block Y in state S1 is moved onto the "table"
% state S2 results in block X being clear and on "table" and block Y becoming clear

% (i)	move from a block X onto the table, and 
moveblocktable(X, Y, S1, S2):- %move(X, Y, "table", S1, S2)
	member([clear, X], S1), %find a clear block X in S1
	member([on, X, Y], S1), block(Y), %find a block on which X sits
	substitute([on, X, Y], [on, X, "table"], S1, INT),  %remove X from Y, place it on "table"
	append([[clear, Y]], INT, S2). % Y is now clear too

% movetableblock(X, Z, S1, S2) holds when a clear block X on the "table" in state S1 is moved onto another clear block Z
% state S2 results in block X being clear and on block Z and block Z becoming no longer clear

% (ii)	move X from the table onto a block
movetableblock(X, Z, S1, S2):- %move(X, "table", Z, S1, S2)
	member([clear, X], S1), %find a clear block X in S1
	member([on, X, "table"], S1), % X should be on table
	member([clear, Z], S1), X \= Z, %find another clear block, Z
	substitute([on, X, "table"], [on, X, Z], S1, INT),  %add X on z
	remove(INT, [clear, Z], S2). % Z is no longer clear


% path(S1, S2) holds when state S2 can be found from state S1 using one of moveblockblock, moveblocktable, movetableblock

% there is a path from state S1 to state S2 when there is a move from S1 to S2.
path(S1, S2):-
	moveblockblock(X, Y, Z, S1, S2);moveblocktable(X, Y, S1, S2);movetableblock(X, Z, S1, S2).


% connect holds when there is a path between states S1 and S2 exists in one direction resulting in the symmetric version of the path

% connect is the symmetric version of path: states S1 and S2 are connected if there is a path from S1 to S2 or a path from S2 to S1.
connect(S1, S2) :- 
	path(S1, S2), path(S2, S1).


% notYetVisited holds when a state State is not a member of PathSoFar, the current path from the start position to the goal position,

notYetVisited(State, PathSoFar):-
	permutation(State, PermuteState),
	notmember(PermuteState, PathSoFar).


% start is the start position exsisting as a list of blocks in their initial states

% starting position 
start([[on, a, "table"], [on, b, "table"], [on, c, a], [on, d, b], [on, e, c], [clear, e], [clear, d]]).
% start([[on, a, "table"], [on, b, "table"], [on, c, a], [clear, c], [clear, b]]).

% goal is the finish position exsisting as a list of blocks in their final states

% goal 
goal([[on, e, "table"], [on, d, e], [on, c, d], [on, b, c], [on, a, b], [clear, a]]).
% goal([[on, a, "table"], [on, c, a], [on, b, c], [clear, b]]).


% dfs(Start, Goal, Path) calls dfs_util, the utility function, with the additional parameter of a list of states starting with [Start]

dfs(Start, Goal, Path) :-
    dfs_util(Start, Goal, [Start], Path).


% dfs_util(Goal, Goal, _, [Goal]) holds when the starting position and final position are the same  resulting in a path with only the final state
% dfs_util(Current, Goal, Visited, [Current|Path]) holds when the current position is different from the goal position  and results in dfs_util being called on the next possible notYetVisited state

% Internal DFS predicate with loop avoidance
dfs_util(Goal, Goal, _, [Goal]) :- !.
dfs_util(Current, Goal, Visited, [Current | Path]) :-
    connect(Current, Next), % find a state with a path in either direction between it and the current state
	%printList(Visited),
    notYetVisited(Next, Visited), % check that the new state has not been visited yet
    dfs_util(Next, Goal, [Next | Visited], Path). % run dfs_util again adding new state to visited and setting  new state as current state


% run dfs on the start, S, and goal, G, state then print path, P

% start(S),goal(G), dfs(S,P,[S]).
%new  start(S),goal(G), dfs(S,G,P), printList(P).

