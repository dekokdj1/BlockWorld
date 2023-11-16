blocks([a, b, c, d, e, f]). 

block(X):- 
    block(BLOCKS),  % this extracts the list BLOCKS 
    member(X, BLOCKS). 

    % move(X, Y, Z, S1, S2) holds when the state S2 is obtained from the state S1 by moving the block X from the block Y onto the block Z. 

move(X, Y, Z, S1, S2):- 
    member([clear, X], S1), %find a clear block X in S1 
    member([on, X, Y], S1), block(Y), %find a block on which X sits 
    member([clear, Z], S1), notequal(X, Z), %find another clear block, Z 
    substitute([on, X, Y], [on, X, Z], S1, INT),  %remove X from Y, place it on Z 
    substitute([clear, Z], [clear, Y], INT, S2). % Z is no longer clear; Y is now clear 

% notequal(X1, X2) takes two arguments and holds true when these arguments are not equal.  In other words, it fails when the arguments are equal and otherwise succeeds. 

notequal(X, X):-!, fail. % fail, if equal. 
notequal(_, _).          % otherwise, succeed. 

% substitute (E, E1, OLD, NEW) holds when NEW is the list OLD in which E is substituted by E1.  There are no duplicates in OLD or NEW. 

 

substitute(X, Y, [X|T], [Y|T]).  % Here, the head X of [X|T] is substituted by Y to yield the list with the head Y to produce the list [Y|T].  The tails of OLD and NEW are the same, because we seek to substitute only one occurrence. 

 

substitute(X, Y, [H|T], [H|T1]):-  
    substitute(X, Y, T, T1).  % In this clause the element to be substituted is NOT the head of the list, so the head H of the list [H|T] carries over to the head H of the new list, but the tail of the new list is obtained from the tail T of the old list where the element X was substituted by Y, producing the new list [H|T1]. 

path(S1, S2):- 
    move(X, Y, Z, S1, S2). 

 

% connect is the symmetric version of path: states S1 and S2 are connected if there is a path from S1 to S2 or a path from S2 to S1. 

connect(S1, S2) :- path(S1, S2). 
connect(S1, S2) :- path(S2, S1). 

notYetVisited(State, PathSoFar):- 
    permutation(State, PermuteState), 
    notmember(PermuteState, PathSoFar). 

dfs(X, [X],_):- goal(X). 

 
% else expand X by Y and find path from Y 
dfs(X, [X|Ypath], VISITED):- 
 	connect(X, Y), 
  	negmember(Y, VISITED), % replace negmember by notYetVisited when using on the block world 
  	dfs(Y, FILLIN, [Y|VISITED]). 