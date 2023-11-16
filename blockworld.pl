block(X):- 
    block(BLOCKS),  % this extracts the list BLOCKS 
    member(X, BLOCKS). 