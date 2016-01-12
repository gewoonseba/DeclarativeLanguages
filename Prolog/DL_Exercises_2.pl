
listlength(L,R):-
  listlength(L,R,0).
listlength([],R,R).
listlength([_|Rest],R,A):-
  NewA is A + 1,
  listlength(Rest,R,NewA).

last([X],X).
last([_|Rest],R):- last(Rest,R).

next_to([X,Y|_],X,Y).
next_to([_|Rest],X,Y):- next_to(Rest,X,Y).

vector_sum([],[],[]).
vector_sum([X|R1],[Y|R2],[R|R3]):-
  R is X + Y,
  vector_sum(R1,R2,R3).

look_up([pair(Name,Grade)|_],Name,Grade).
look_up([_|Rest],Name,Grade):-
  look_up(Rest,Name,Grade).

%Graph
node(a).
node(b).
node(c).
node(d).
node(e).

edge(a,b).
edge(b,c).
edge(c,d).
edge(d,b).

neighbor(X,Y):- edge(X,Y).
neighbor(X,Y):- edge(Y,X).

path(X,Y):- path(X,Y,[X]).
path(X,Y,L):-
  neighbor(X,Y),
  \+ member(Y,L).
path(X,Y,L):-
  neighbor(X,Z),
  \member(Z,L),
  path(Z,Y,[Z|L]).

%Fibonacci
%ineffictient version
fib(1,0).
fib(2,1).
fib(N,F):-
  N2 is N - 2,
  N1 is N - 1,
  fib(N2,F2),
  fib(N1,F1),
  F is F1 + F2.

%more efficiently (only one (tail) recursive call)
fib2(1,0).
fib2(2,1).
fib2(N,F):-
  fib2(N,2,1,0,F).
fib2(N,I,F2,F1,F):-
  In is I + 1,
  Fn is F2 + F1,
  (
    In = N
  ->
    Fn = F
  ;
    fib2(N,In,Fn,F2,F)
  ).

%trees
max_one_difference(X,X).
max_one_difference(A,B):-
  A is B + 1.
max_one_difference(A,B):-
  B is A + 1.

depth(empty,0).
depth(node(L,_,R),D):-
  depth(L,Ld),
  depth(R,Rd),
  D is max(Ld,Rd) + 1.

balanced(empty).
balanced(node(L,_,R)):-
  depth(L,Ld),
  depth(R,Rd),
  max_one_difference(Ld,Rd),
  balanced(L),
  balanced(R).

shalowest_tree(T1,T2,S):-
  depth(T1,D1),
  depth(T2,D2),
  (
    D1 < D2
  ->
    S = T1
  ;
    S = T2
  ).

add_to(node(empty,V,R),X,node(X,V,R)).
add_to(node(L,V,empty),X,node(L,V,X)).
add_to(node(L,_,R),X,Res):-
  shalowest_tree(L,R,T),
  add_to(T,X,Res).

%Expressive
eval(int(X),_,X).
eval(var(Var),List,Res):-
  look_up(List,Var,Res).
eval(plus(X,Y),List,Res):-
  eval(X,List,Res1),
  eval(Y,List,Res2),
  Res is Res1 + Res2.
eval(times(X,Y),List,Res):-
  eval(X,List,Res1),
  eval(Y,List,Res2),
  Res is Res1 * Res2.
eval(pow(X,Y),List,Res):-
  eval(X,List,Res1),
  eval(Y,List,Res2),
  Res is Res1 ** Res2.
eval(min(X),List,Res):-
  eval(X,List,Res1),
  Res is 0 - Res1.

%primes
remove_multiples([],_,[]).
remove_multiples([X|Rest],Val,Result):-
  (
    0 is X mod Val
  ->
    NResult = Result
  ;
    NResult = [X|Result]
  ),
  remove_multiples(Rest,Val,NResult).
