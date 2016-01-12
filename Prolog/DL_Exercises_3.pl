
teaches(berbers,bs).
teaches(berbers,iw).
teaches(demoen,ab).
teaches(demoen,cc).
teaches(holvoet,bvp).
teaches(moens,bvp).
teaches(danny,ai).
teaches(maurice,ai).
teaches(dedecker,socs).

takes(tom,bs).
takes(tom,bvp).
takes(tom,socs).
takes(maarten,socs).
takes(maarten,bs).
takes(pieter,bvp).

takes_same_course(X,Y) :-
	findall(paar(X,Z),(takes(X,W), takes(Z,W), X \= Z),L),
	list_to_set(L,S),
	member(paar(X,Y),S).

teach_same_course(X,Y) :-
  findall(paar(X,Z),(teaches(X,W), teaches(Z,W), X \= Z),L),
  list_to_set(L,S),
  member(paar(X,Y),S).

teaches_multiple_courses(X):-
  findall(X,(teaches(X,Z), teaches(X,Y), Y \= Z), L),
  list_to_set(L,S),
  member(X,S).

%prelude to functional programming

map(_,[],[]).
map(P,[X|Xs],[Y|Ys]) :-
  Call =.. [P,X,Y],
  call(Call),
  map(P,Xs,Ys).

%interpreter

%turing tape
% The internal representation of a tape is tape(Left,Current,Right), where
% Left is the reversed list of symbols to the left of the current symbol,
% Right is the list with symbols to the right of the current symbol, and
% Current is the symbol currently located under the read head.
% It is possible to perform every operation in constant time.
move(left,tape([],Symbol,Right),tape([],#,[Symbol|Right])).
move(left,tape([FirstLeft|RestLeft],Symbol,Right),tape(RestLeft,FirstLeft,[Symbol|Right])).
move(right,tape(Left,Symbol,[]),tape([Symbol|Left],#,[])).
move(right,tape(Left,Symbol,[FirstRight|Right]),tape([Symbol|Left],FirstRight,Right)).

read_tape(tape(_,Symbol,_),Symbol).
write_tape(Symbol,tape(L,_,R),tape(L,Symbol,R)).

%NQueens
queens(N,L):-
  numlist(1,N,Orig),
  permutate(Orig,L),
  diag1(L),
  diag2(L).

count_queens(N,Int):-
  findall(L,queens(N,L),Bag),
  length(Bag,Int).

%Horizontal direction is ok if no two values are the same
horiz(L):-
  list_to_set(L,S),
  length(L,LL),
  length(S,SL),
  SL == LL.

permutate([],[]).
permutate(Orig,[Elem|Perm]):-
  select(Elem,Orig,Rest),
  permutate(Rest,Perm).

%First diag (top left -> bottom right)
diag1(L):-
  index_diff(L,Idiff),
  list_to_set(Idiff,Is),
  length(Idiff,LL),
  length(Is,SL),
  SL == LL.

index_diff(L,Idiff):-
  length(L,N),
  numlist(1,N,I),
  vector_diff(L,I,Idiff).

vector_diff([],[],[]).
vector_diff([X|Xs],[Y|Ys],[D|Ds]):-
  D is X - Y,
  vector_diff(Xs,Ys,Ds).

%Second diag (bottom left -> top right)
diag2(L):-
  index_sum(L,Idiff),
  list_to_set(Idiff,Is),
  length(Idiff,LL),
  length(Is,SL),
  SL == LL.

index_sum(L,Idiff):-
  length(L,N),
  numlist(1,N,I),
  vector_sum(L,I,Idiff).

vector_sum([],[],[]).
vector_sum([X|Xs],[Y|Ys],[D|Ds]):-
  D is X + Y,
  vector_sum(Xs,Ys,Ds).
