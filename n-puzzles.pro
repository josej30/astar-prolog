/************************************************
*
* Problema de n-puzzles
* 
*
* inicial: Estado inicial del acertijo
* objective: Estado al que deseamos llegar
*
*************************************************/

inicial(state([X|XS])) :-
   length([X|XS],Y),
   aux_estado([X|XS],Y),
   Y1 is Y*Y,
   aux_inicio([X|XS],1,Y1).

aux_estado([],_).
aux_estado([X|XS],Y) :-
   length(X,Y),
   aux_estado(XS,Y).

aux_inicio(L,Y,Y)  :- member(L1,L), member(empty,L1).
aux_inicio(L,Y,Y1) :- Y < Y1, Y2 is Y+1, member(L1,L), member(Y,L1), aux_inicio(L,Y2,Y1).

objective(state([X|XS])) :- 
   length([X|XS],Y),
   aux_estado([X|XS],Y),
   Y1 is Y*Y,
   aux_fin([X|XS],1,Y1).

aux_fin(L,Y,Y)            :- member(L1,L), member(empty,L1).
aux_fin([[]|XS],Y,Y1)     :- aux_fin(XS,Y,Y1).
aux_fin([[Y|YS]|XS],Y,Y1) :- Y \= Y1, Y2 is Y+1, aux_fin([YS|XS],Y2,Y1).