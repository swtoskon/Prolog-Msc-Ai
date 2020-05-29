%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Serpiko_PRoblem
%Athour:Konstantakos_Sotirios
%Arithmos_Mitrwou:mtn1905


place_snake(List,K,Cols,Rows,Box):-
        length(List,M),
         N is M*K,
        length(List1,N),N=<Rows*Cols,
        snake2(List,List1),!,
        length(L2,Cols),
        length(Box,Rows),
        take_parts(List1,L2,Box,Cols,Rows).

snake2(_,[]):-!.

snake2(List,List1):- 
         append(List,List2,List1),
         snake2(List,List2),!.

take_parts(_,_,[],_,_):-!.
take_parts(List1,L2,[L2|T],Cols,Rows):-
              length(List1,K),length(L2,M),K>=M,
             1 is Rows mod 2,
             append(L2,L3,List1),
             length(NL2,Cols),
              Rows1 is Rows -1,
              take_parts(L3,NL2,T,Cols,Rows1),!.

take_parts(List1,L2,[L2|T],Cols,Rows):-
             0 is Rows mod 2,
             length(List1,K),length(L2,M),K>=M,
             append(L4,L3,List1),
             reverse(L4,L2),
             length(NL2,Cols),
              Rows1 is Rows -1,
              take_parts(L3,NL2,T,Cols,Rows1),!.

take_parts(List1,L2,[NL2|T],Cols,Rows):-
              length(List1,K),length(L2,M),K<M,
               N is M-K,length(List0,N),
                length(List3,K),
               create(List0),
               length(NL2,Cols),
              append(List1,List0,NL2),
              append(List3,List4,List1),
              take_parts(List4,NL2,T,Cols,Rows).

create([]).
create(['o'|T]):-
       create(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%COLORING_MAP

template_map([],[]).
template_map([adjacent(X,_)|T],[color(X,_)|T1]):-
         template_map(T,T1).


color_map(Graph,C,Map):-
         template_map(Graph,Map),
         solve_map(Graph,C,Map).

solve_map([],_,_).
solve_map([adjacent(X,[])|Rest],C,Map):-solve_map(Rest,C,Map).
solve_map([adjacent(X,[H|T])|Rest],C,Map):-
           swtos(X,H,C,Map),
           solve_map([adjacent(X,T)|Rest],C,Map).
             
swtos(X,H,C,Map):-
         member(Colour,C),
          member(Colour1,C),
          Colour \= Colour1,
         member(color(X,Colour),Map),
         member(color(H,Colour1),Map).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Maze_Problem

move1((X/Y),(X/NY)):- NY is Y + 1.
move1((X/Y),(X/NY)):- NY is Y - 1. 
move1((X/Y),(NX/Y)):- NX is X + 1.
move1((X/Y),(NX/Y)):- NX is X - 1.  

solve_maze(M,N,Barriers,From,To,Path) :-
       path(From,To,M,N,Barriers,Path),!.

move_cyclefree1(Visited, Node, NextNode) :-
  move1(Node, NextNode),
  \+ member(NextNode, Visited).


valid1(M,N,(X/Y),Barriers):- 
 X>0,X=<M,Y>0,Y=<N,
\+(member((X/Y),Barriers)).

path(From,From,_,_,_,[From]).

path(Node, LastNode,M,N,Barriers, Path) :-
  path(Node, PenultimateNode,M,N,Barriers, PenultimatePath),
  move_cyclefree1(PenultimatePath, PenultimateNode, LastNode),
  valid1(M,N,LastNode,Barriers),
  append(PenultimatePath, [LastNode], Path).
  


















