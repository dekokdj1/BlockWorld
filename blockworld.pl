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
remove([], _, [], _).
remove([H|T], E, [H|L3], L4):- H \= E, remove(T, E, L3, L4).
remove([H|T], E, L3, L4):- H = E, append(L4, T, L3).

% printList(L) prints the list L.
printList([]).
printList([H|T]):- write(H), nl, printList(T).

% notequal(X1, X2) takes two arguments and holds true when these arguments are not equal.  In other words, it fails when the arguments are equal and otherwise succeeds.
notequal(X, X):-!, fail. % fail, if equal.
notequal(_, _).          % otherwise, succeed.

% substitute (E, E1, OLD, NEW) holds when NEW is the list OLD in which E is substituted by E1.  There are no duplicates in OLD or NEW.
substitute(X, Y, [X|T], [Y|T]).  % Here, the head X of [X|T] is substituted by Y to yield the list with the head Y to produce the list [Y|T].  The tails of OLD and NEW are the same, because we seek to substitute only one occurrence.

substitute(X, Y, [H|T], [H|T1]):- 
    substitute(X, Y, T, T1).  % In this clause the element to be substituted is NOT the head of the list, so the head H of the list [H|T] carries over to the head H of the new list, but the tail of the new list is obtained from the tail T of the old list where the element X was substituted by Y, producing the new list [H|T1].

% blocks(L) holds when L is a list of blocks.
blocks([a, b, c, d, e]).

block(X):-
    block(BLOCKS),  % this extracts the list BLOCKS
    member(X, BLOCKS).

% move(X, Y, Z, S1, S2) holds when the state S2 is obtained from the state S1 
% by moving the block X from the block Y onto the block Z.
move(X, Y, Z, S1, S2):-
	member([clear, X], S1), %find a clear block X in S1
	member([on, X, Y], S1), block(Y), %find a block on which X sits
	member([clear, Z], S1), notequal(X, Z), %find another clear block, Z
	substitute([on, X, Y], [on, X, Z], S1, INT),  %remove X from Y, place it on Z
	substitute([clear, Z], [clear, Y], INT, S2). % Z is no longer clear; Y is now clear
	
% You must write two more rules for the blocks’ world: 
% (i)	move from a block X onto the table, and 
move(X, Y, _, S1, S2):- %move(X, Y, "table", S1, S2)
	member([clear, X], S1), %find a clear block X in S1
	member([on, X, Y], S1), block(Y), %find a block on which X sits
	substitute([on, X, Y], [on, X, "table"], S1, INT),  %remove X from Y, place it on "table"
	append([clear, Y], INT, S2). % Y is now clear too

% (ii)	move X from the table onto a block
move(X, _, Z, S1, S2):- %move(X, "table", Z, S1, S2)
	member([clear, X], S1), %find a clear block X in S1
	member([on, X, "table"], S1), % X should be on table
	member([clear, Z], S1), notequal(X, Z), %find another clear block, Z
	substitute([on, X, "table"], [on, X, Z], S1, INT),  %add X on z
	remove(INT, [clear, Z], S2). % Z is no longer clear

% there is a path from state S1 to state S2 when there is a move from S1 to S2.
path(S1, S2):-
	move(_, _, _, S1, S2).

% connect is the symmetric version of path: states S1 and S2 are connected if there is a path from S1 to S2 or a path from S2 to S1.
connect(S1, S2) :- path(S1, S2).
connect(S1, S2) :- path(S2, S1).


notYetVisited(State, PathSoFar):-
	permutation(State, PermuteState),
	notmember(PermuteState, PathSoFar).

% dfs(Start, Path, PathSoFar): returns the Path from the start to the goal state given the path so far.
dfs(Start, Path, PathSoFar):-
	%todo

% Trivial: if X is the goal return X as the path from X to X.
dfs(X, [X], _):- goal(X).
% else expand X by Y and find path from Y
dfs(X, [X|Ypath], VISITED):-
 	connect(X, Y),
	%negmember(Y, VISITED), % replace negmember by notYetVisited when using on the block world
  	notYetVisited(Y, VISITED),
	dfs(Y, Ypath, [Y|VISITED]).


% starting position 
start([[on, a, "table"], [on, b, "table"], [on, c, a], [on, d, b], [on, e, c], [clear, e], [clear, d]]).
% goal 
goal([[on, e, "table"], [on, d, e], [on, c, d], [on, b, c], [on, a, b], [clear, a]]).

% print the path from start to goal, if it exists
% printPath(Start, Goal, Path):-
% 	dfs(Start, Path, [Start]),!,
% 	write('Path from '), write(Start), write(' to '), write(Goal), write(':'), nl,
% 	printList(Path).




