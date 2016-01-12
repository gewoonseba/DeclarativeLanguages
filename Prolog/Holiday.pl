%Holiday Lights

highway(1,2,yellow).
highway(2,3,blue).
highway(1,3,yellow).

node(X):- highway(X,_,_).
node(X):- highway(_,X,_).

all_nodes(Nodes):-
        findall(X,node(X),Nl),
        list_to_set(Nl,Nodes).

check_even([]).
check_even([N|Nodes]):-
        findall(Y,(highway(N,Y,_) ; highway(Y,N,_)),Dest),
        list_to_set(Dest,UDest),
        length(UDest,L),
        0 is L mod 2,
        check_even(Nodes).

check_colour([]).
check_colour([N|Nodes]):-
        findall(C,(highway(N,_,C) ; highway(_,N,C)),Col),
        list_to_set(Col,UCol),
        check_colour_node(N,UCol),
        check_colour(Nodes).

check_colour_node(1,_).
check_colour_node(_,[]).
check_colour_node(N,[Col|Cols]):-
        findall(X,((highway(N,X,NotC) ; highway(X,N,NotC)), NotC \= Col),NotCConn),
        list_to_set(NotCConn,NotSet),
        findall(X,(highway(X,N,Col) ; highway(N,X,Col)),CConn),
        list_to_set(CConn,Set),
        length(NotSet,NotL),
        length(Set,L),
        NotL >= L,
        check_colour_node(N,Cols).
        
check:-
        all_nodes(Nodes),
        check_even(Nodes),
        check_colour(Nodes).

smaller_tour([],[]).
smaller_tour([T1|T1s],[T2|T2s]):-
        T1 @< T2,
        smaller_tour(T1s,T2s).

find_smallest_tour([T|Ts],Res):- find_smallest_tour(Ts,Res,T).
find_smallest_tour([],Cursm,Cursm).
find_smallest_tour([T|Ts],Res,Cursm):-
        (
                smaller_tour(T,Cursm)
        ->
                find_smallest_tour(Ts,Res,T)
        ;
                find_smallest_tour(Ts,Res,Cursm)
        ).

create_tour(P,[],_,[N-Col]):-
        all_nodes(Nodes),
        min_member(N,Nodes),
        (highway(P,N,Col) ; highway(N,P,Col)).
        
create_tour(N,Nodes,PrevCol,[Ndest-Ncol|Rtour]):-
        findall(X-Col,((highway(N,X,Col) ; highway(X,N,Col)) , Col \= PrevCol),Opt),
        select(Ndest-Ncol,Opt,_),
        select(Ndest,Nodes,NNodes),
        create_tour(Ndest,NNodes,Ncol,Rtour).
        
tour(T):-
        check,
        all_nodes(Nodes),
        sort(Nodes,[N|Ns]),
        findall(TOpt,create_tour(N,Ns,null,TOpt),Tours),
        find_smallest_tour(Tours,T).
        






































