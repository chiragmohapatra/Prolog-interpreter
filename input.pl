father(jack, susan).                             
father(jack, ray).                               
father(david, liza).                             
father(david, john).                             
father(john, peter).                             
father(john, mary).                              
mother(karen, susan).                            
mother(karen, ray).                              
mother(amy, liza).                               
mother(amy, john).                               
mother(susan, peter).                            
mother(susan, mary).    
plays(jack,ray,golf).                        

parent(X, Y) :- father(X, Y).                    
parent(X, Y) :- mother(X, Y).                    
grandfather(X, Y) :- father(X, Z), parent(Z, Y). 
grandmother(X, Y) :- mother(X, Z), parent(Z, Y). 
grandparent(X, Y) :- parent(X, Z), parent(Z, Y). 
yeye(X, Y) :- father(X, Z), father(Z, Y).        
mama(X, Y) :- mother(X, Z), father(Z, Y).        
gunggung(X, Y) :- father(X, Z), mother(Z, Y).    
popo(X, Y) :- mother(X, Z), mother(Z, Y).

edge(a1,a2).
edge(a2,a3).
edge(a3,a4).
edge(a3,a5). 
tedge(X,A) :- edge(X,A).
path(X,X).
path(X,Y) :- tedge(X,Z) , path(Z,Y).
