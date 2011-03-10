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

/**************************************
* action({up|down|left|right)
**************************************/

action(state([X|XS]),up,state(R)) :-
	\+ member(empty,X),
	cambiar_empty_up([X|XS],R).
action(state(L),down,state(R)) :-
	last(L,L1),
	\+ member(empty,L1),
	cambiar_empty_down(L,R).
action(state(L),left,state(R)) :-
	member([X|XS],L),
	member(empty,[X|XS]),
	X \= empty,
	cambiar_empty_left(L,R).
action(state(L),right,state(R)) :-
	last(L,L1),
	\+ member(empty,L1),
	cambiar_empty_right(L,R).

cambiar_empty_up([X1,X2|XS],[X1|L]) :-
	\+ member(empty,X2),
	cambiar_empty_up([X2|XS],L).
cambiar_empty_up([X1,X2|XS],[E1,E2|XS]) :-
	member(empty,X2),
	swap_columna(X2,X1,E2,E1).

cambiar_empty_down([X1,X2|XS],[X1,L]) :-
	\+ member(empty,X1),
	cambiar_empty_down([X2|XS],L).
cambiar_empty_down([X1,X2|XS],[E1,E2|XS]) :-
	member(empty,X1),
	swap_columna(X1,X2,E1,E2).

swap_columna([X|XS],[Y|YS],[Y|XS],[X|YS]) :- X == empty.
swap_columna([X|XS],[Y|YS],[X|L1],[Y|L2]) :-
	X \= empty,
	swap_columna(XS,YS,L1,L2).

cambiar_empty_left([X|XS],[X|L]) :-
	\+ member(empty,X),
	cambiar_empty_left(XS,L).
cambiar_empty_left([X|XS],[E|XS]) :-
	member(empty,X),
	swap_left(X,E).

cambiar_empty_right([X|XS],[X|L]) :-
	\+ member(empty,X),
	cambiar_empty_right(XS,L).
cambiar_empty_right([X|XS],[E|XS]) :-
	member(empty,X),
	swap_right(X,E).

swap_left([X1,X2|XS],[X2,X1|XS]) :- X2 == empty.
swap_left([X1,X2|XS],[X1,L]) :-
	X2 \= empty,
	swap_left([X2|XS],L).

swap_right([X1,X2|XS],[X2,X1|XS]):- X1 == empty.
swap_right([X1,X2|XS],[X1|L]) :-
	X1 \= empty,
	swap_right([X2|XS],L).

/*
* Costo :D no necesita auxiliares :D
*/
cost(state([X|_]),up,1)      :- \+ member(empty,X).
cost(state(L),down,1)         :- 
	last(L,L1),
	\+ member(empty,L1).
cost(state(L),left,1)         :- 
	member([X|XS],L),
	member(empty,[X|XS]),
	X \= empty.
cost(state(L),right,1)        :-
	last(L,L1),
	\+ member(empty,L1).

/*
* showmoves
*/

showmoves(state(L),_) :- 
	show_estado(state(L)), !.
showmoves(state(L),[A|AS]) :-
	show_estado(state(L)),
	action(state(L),A,E1),
	showmoves(E1,AS), !.
show_estado(state(L)) :- 
	length(L,N),
	show_estado(L),
	show_linea(0,N), nl.
show_estado([]).
show_estado([X|XS]) :-
	length(X,N),
	show_linea(0,N),
	show_elementos(X),
	show_estado(XS).

show_linea(X,X) :- write(+), nl, !.
show_linea(X,Y) :-
	write(+--),
	X1 is X+1,
	show_linea(X1,Y), !.

show_elementos([]) :- write('|'), nl, !.
show_elementos([empty|XS]) :-
	write('|  '),
	show_elementos(XS). 
show_elementos([X|XS])     :- 
	format("| ~w",[X]),
	show_elementos(XS).