/************************************************
*
* Problema del puente, los 4 chicos y la linterna con 60mint de bateria
* los 4 exploradores: Curly (20min), Larry (10min), Moe (5min) y Shemp (25min)
*
* inicial: Estado inicial del acertijo
* objective: Estado al que deseamos llegar
*
*************************************************/
inicial(state([curly,larry,moe,shemp],[],left)).

objective(state([],D,right)) :- 
   length(D,4),
   member(curly,D),
   member(larry,D),
   member(moe,D),
   member(shemp,D).

/************************************************
* action(Start,Action,End) 
*************************************************/

action(state(I,D,X),move(X1,Y),state(I1,D1,X1)) :-
   cambio_linterna(X,X1),
   cambio_lado(I,I1,D,D1,X,Y).

cambio_linterna(left,right).
cambio_linterna(right,left).

cambio_lado(I,I1,D,D1,left,[N1]) :-
   select(N1,I,I1),
   insert(N1,D,D1).

cambio_lado(I,I1,D,D1,right,[N1]) :-
   select(N1,D,D1),
   insert(N1,I,I1).

cambio_lado(I,I1,D,D1,left,[N1,N2]) :-
   select(N1,I,L1),
   select(N2,L1,I1),
   insert(N1,D,L2),
   insert(N2,L2,D1).

cambio_lado(I,I1,D,D1,right,[N1,N2]) :-
   select(N1,D,L1),
   select(N2,L1,D1),
   insert(N1,I,L2),
   insert(N2,L2,I1).

insert(X,[Y|Ys],[X,Y|Ys]) :-
    precede(X,Y).
insert(X,[Y|Ys],[Y|Zs]) :-
    precede(Y,X), insert(X,Ys,Zs).
insert(X,[],[X]).

precede(curly,_).
precede(larry,moe).
precede(_,shemp).

/************************************************
* Costo del tiempo y sus funciones auxiliares 
*************************************************/

cost(state(I,_,left),move(right,[N1]),Costo) :-
   member(N1,I),
   costo(N1,Costo).
cost(state(_,D,right),move(left,[N1]),Costo) :-
   member(N1,D),
   costo(N1,Costo).

cost(state(I,_,left),move(right,[N1,N2]),Costo) :-
   N1 \= N2,
   member(N1,I),
   member(N2,I),
   costo(N1,N2,Costo).
cost(state(_,D,right),move(left,[N1,N2]),Costo) :-
   N1 \= N2,
   member(N1,D),
   member(N2,D),
   costo(N1,N2,Costo).

costo(curly,C) :- C is 20.
costo(larry,C) :- C is 10.
costo(moe,C) :- C is 5.
costo(shemp,C) :- C is 25.

costo(N1,N2,R) :- costo(N1,C1), costo(N2,C2), max(C1,C2,R).

max(X,Y,Y) :- X =< Y.
max(X,Y,X) :- Y < X.


/************************************************
* showmoves\2 y todos sus auxiliares
*************************************************/

showmoves(state(I,D,X),L)     :-
	write('*** Estado Inicial ***'), nl,
	showmoves_aux(state(I,D,X),L,0).

showmoves_aux(state(I,D,X),[],Costo)     :-
	write('*** Resultado ***'), nl,
	show_estado(state(I,D,X)), !.
showmoves_aux(state(I,D,X),[A|AS],Costo) :-
	show_estado(state(I,D,X)),
	action(state(I,D,X),A,E1),
        cost(state(I,D,X),A,Nuevo),
        Costo1 is Costo + Nuevo,
	show_movimiento(A,Costo1),  
	showmoves_aux(E1,AS,Costo1),!.

show_estado(state(I,D,X)) :-
   show_personas(I),
   show_lin(X),
   show_personas(D), nl, nl.

show_personas(X) :- 
   show(curly,X),
   show(larry,X),
   show(moe,X),
   show(shemp,X).

show(X,Y)   :- member(X,Y), format("~a ",[X]).
show(moe,Y) :- \+ member(moe,Y), write('    ').
show(X,Y)   :- X \= moe, \+ member(X,Y), write('      ').

show_lin(left) :- show_pue("@ ","   ").
show_lin(right) :- show_pue("  "," @ ").

show_pue(X,Y) :-
   format("~s|_______|~s",[X,Y]).

show_movimiento(move(left,[N1]),Costo) :- 
	show_mov("derecha","izquierda",N1,Costo), !.
show_movimiento(move(right,[N1]),Costo) :- 
	show_mov("izquierda","derecha",N1,Costo), !.   
show_movimiento(move(left,[N1,N2]),Costo) :- 
	show_mov("derecha","izquierda",N1,N2,Costo), !.
show_movimiento(move(right,[N1,N2]),Costo) :- 
	show_mov("izquierda","derecha",N1,N2,Costo), !.

show_mov(X,Y,N1,Costo) :-
	format("Con ~w minutos de uso de linterna (Termina el Movimiento): Desplazamiento de ~s a ~s de ~a",[Costo,X,Y,N1]),  nl.
show_mov(X,Y,N1,N2,Costo) :-
	format("Con ~w minutos de uso de linterna (Termina el Movimiento): Desplazamiento de ~s a ~s de ~a y ~a",[Costo,X,Y,N1,N2]), nl.

/*******************************x*****************
*      showmoves(state([curly,larry,moe,shemp],[],left),[move(right,[larry,moe]),move(left,[moe]),move(right,[curly,shemp]),move(left,[larry]),move(right,[larry,moe])]).
************************************************/

heuristic(_,1).
