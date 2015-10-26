father(anton,bart).
father(anton,daan).
father(anton,elisa).
father(fabian,anton).

mother(celine,bart).
mother(celine,daan).
mother(celine,gerda).
mother(gerda,hendrik).

sibling(X,Y):-
        father(Z,X),
        father(Z,Y),
        mother(Q,X),
        mother(Q,Y),
        X \== Y.

ancestor(X,Y):-
        father(Z,Y),
        ancestor(X,Z).

ancestor(X,Y):- father(X,Y).

peano_plus(zero,X,X).
peano_plus(s(X),Y,s(Z)):- peano_plus(X,Y,Z).

min(X,zero,X).
min(X,s(Y),Z):- min(X,Y,s(Z)).

greater_than(s(_),zero).
greater_than(s(X),s(Y)):- greater_than(X,Y).

maximum(X,zero,X).
maximum(zero,Y,Y).
maximum(s(X),s(Y),s(Z)):- maximum(X,Y,Z). 

%EXTRA

min_plus(X,Y,Z):- peano_plus(Z,Y,X).

div(X,X,s(zero),zero).

div(X,_,zero,X).

div(X,Y,_,_):-
        greater_than(Y,X),
        div(X,Y,zero,X).

div(X,Y,D,R):-
        min(X,Y,Z),
        div(Z,Y,s(D),R).
