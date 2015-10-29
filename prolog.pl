%Sebastian Stoelen
% r0381219
% master cw

%Assignment one

%get all relevant numbers
relevant_number(Numbers,Max,Solution):-
        min_list(Numbers,Min),
        numlist(Min,Max,Cand),
        remove_excess_candidates(Numbers,Cand,SolList),
        member(Solution,SolList).

%max is the largest number in a list
max_list(List,Max):-
        max_list(List,Max,0).
max_list([],Max,Max).
max_list([New|Rest],Max,CurMax):-
        NCurMax is max(CurMax,New),
        max_list(Rest,Max,NCurMax).

%Min is the smallest number in a list
min_list(List,Min):-
        max_list(List,Max),
        min_list(List,Min,Max).
min_list([],Min,Min).
min_list([New|Rest],Min,CurMin):-
        NCurMin is min(CurMin,New),
        min_list(Rest,Min,NCurMin).

%remove entries from a list that are not formed by the given list of numbers        
remove_excess_candidates(Numbers,Cand,Sol):-
        remove_excess_candidates(Numbers,Cand,Sol,[]).
remove_excess_candidates(_,[],Sol,Sol).
remove_excess_candidates(Numbers,[NextC|RestC],Sol,CurSol):-
        (
                formed_by_numbers(Numbers,NextC)
        ->
                remove_excess_candidates(Numbers,RestC,Sol,[NextC|CurSol])
        ;
                remove_excess_candidates(Numbers,RestC,Sol,CurSol)
        ).

%check wheter an int is formed by the numbers in a list
formed_by_numbers(_,0).
formed_by_numbers(Numbers,Int):-
        Check is Int mod 10,
        member(Check,Numbers),
        Interm is (Int - Check),
        NewInt is div(Interm,10),
        formed_by_numbers(Numbers,NewInt).
                
%Assignment two

%calculate the operations needed to reach goal
calcul(Numbers,Goal,Curr,Ops):-
        calcul(Numbers,Goal,Curr,Ops,[]).
calcul(_,Goal,Goal,Ops,ROps):-
        reverse(Ops,ROps).
calcul(Numbers,Goal,Curr,Ops,ROps):-
        (
                Curr = 0
        ->
                relevant_number(Numbers,Goal,RelNb),
                is_valid(+,RelNb),
                eval(Curr,[Op,RelNb],NCurr),
                NCurr =< Goal,
                calcul(Numbers,Goal,NCurr,Ops,[RelNb,Op|ROps])
        ;   
                member(Op,[+,*]),
                relevant_number(Numbers,Goal,RelNb),
                is_valid(Op,RelNb),
                eval(Curr,[Op,RelNb],NCurr),
                NCurr =< Goal,
                calcul(Numbers,Goal,NCurr,Ops,[RelNb,Op|ROps])
        ).

%lists the operations that are not valid
not_val(*,0).
not_val(*,1).
not_val(+,0).

is_valid(Op,Int):-
        \+ not_val(Op,Int).

%evaluate a list of operations        
eval(Res,[],Res).
eval(IntRes,[+,X|Rest],Res):-
        NewIntRes is IntRes + X,
        eval(NewIntRes,Rest,Res).
eval(IntRes,[*,X|Rest],Res):-
        NewIntRes is IntRes * X,
        eval(NewIntRes,Rest,Res).

%Assignment 3
%find all the solutions that are a solution tot calcul from the previous, select the ones with the given cost
calculCost(Numbers,Goal,Curr,NbOps,Ops):-
        findall(Op,(calcul(Numbers,Goal,Curr,Op),calculate_cost(Op,NbOps)),Ops).
        
calculate_cost(Ops,Cost):-
       calculate_cost(Ops,Cost,0).
calculate_cost([],Cost,Cost).
caculate_cost([NOp|Ops],Cost,CurrCost):-
        calculate_cost_op(NOp,AddCost),
        NCurrCost is AddCost + CurrCost,
        calculate_cost(Ops,Cost,NCurrCost).
        
%Calculate the cost for different operations
calculate_cost_op(+,1).
calculate_cost_op(*,1).
calculate_cost_op(X,Cost):-
        calculate_cost_int(X,Cost,1).
calculate_cost_int(0,Cost,Cost).
%To calculate the cost of a number, we count the number of cifers in the number
calculate_cost_int(X,Cost,CurrCost):-
        Mod is X mod 10,
        Interm is (X - Mod),
        (
                Interm = 0
        ->
                calculate_cost_int(Interm,Cost,CurrCost)
        ;
                NewInt is div(Interm,10),
                NCurrCost is CurrCost + 1,
                calculate_cost_int(NewInt,Cost,NCurrCost)
        ).
         

%Assignment 4
%create numlist(2,Max,List). This represents the number of steps Stijn could take. Call calculCost with increasing numbers from the numlist, add to the front of a Allsolutions list, reverse the list at the end, use member(Sol,Allsolutios) to first get the smallest number of steps.

%Assignment

