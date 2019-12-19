sameelts([],[]).
sameelts([A|X],Y) :- append(U,[A|V],Y), append(U,V,Z), sameelts(X,Z).

less1(X,Z) :- X is Z-1.
