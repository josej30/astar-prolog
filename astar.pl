/* Definicion de A* */

/* Que necesita?
* 
* -> Nombre del problema a resolver
* -> Lista de caminos explorados hasta el momento 
*    con su respectivo costo final
* <- Costo y estado final
*/

/* 
Tengo que revisar si el ultimo camino expandido queda al principio. 
Esto para no tener que revisar toooodos los caminos y mapearle la funcion objective
*/
astar(Problema,[[Costo,Estado|Estados]|Caminos],Solucion) :- 
    objective(Problema,Estado),
    append([Costo],[Estado],Solucion).
astar(Problema,[[Costo,Estado|Estados]|Caminos],Solucion) :-
    procesar(Problema,[[Costo,Estado|Estados]|Caminos],Nuevos),
    astar(Problema,Nuevos,Solucion).

procesar(Problema,[[Costo,Estado|Estados]|Caminos],Resultado) :-
    findall(
	[CostoEstrella,CostoReal,EstadoNuevo,Costo,Estado|Estados],
	( 
	  movimiento(Problema,Estado,Movimiento),
	  cost(Problema,Estado,Movimiento,CostoMoverse),
	  action(Problema,Estado,Movimiento,EstadoNuevo),
	  heuristic(Problema,EstadoNuevo,CostoHeu),
	  CostoEstrella is Costo + CostoHeu + CostoMoverse,
          CostoReal is CostoMoverse + Costo
	),
	Posibles
    ),!,
    renovar(Posibles,[[Costo,Estado|Estados]|Caminos],Resultado).
procesar(_,_,[]).

renovar([[CostoEstrella,CostoReal,EstadoNuevo|R1]|Ns],[[Costo,Estado|Estados]|Caminos],Resultado) :-
    caminoMinimo([[CostoEstrella,CostoReal,EstadoNuevo|R1]|Ns],CM),
    caminoEscogido([[CostoEstrella,CostoReal,EstadoNuevo|R1]|Ns],CM,CamE),
    renovarCaminos([[Costo,Estado|Estados]|Caminos],CamE,Resultado).

/* Se saca el costo minimo de todos los posibles */
caminoMinimo([[CostoEstrella,_,_|_]|Ns],CostoMinimo) :-
    caminoMinimo(Ns,CM), CostoMinimo is min(CM,CostoEstrella).
caminoMinimo([[CostoEstrella,_,_|_]],CostoEstrella).

/* Se escoge un camino que coincida con el costo minimo */
caminoEscogido([],_,[]).
caminoEscogido([[CM,CostoReal,EstadoNuevo|R1]|Ns],CM,[CostoReal,EstadoNuevo|R1]).
caminoEscogido([[CostoEstrella,CostoReal,EstadoNuevo|R1]|Ns],CM,RestoCaminos) :- 
    CostoEstrella \== CM, caminoEscogido(Ns,CM,RestoCaminos).

/* Se actualiza la lista de caminos sustituyendo el viejo camino por el nuevo expandido y los demas se dejan igual */
renovarCaminos([],_,[]).
renovarCaminos([[Costo,Estado|Estados]|Caminos],[CostoE,EstadoE|[Costo,Estado|Estados]],[[CostoE,EstadoE|[Costo,Estado|Estados]]|Resultado]) :-
    renovarCaminos(Caminos,[CostoE,EstadoE|[Costo,Estado|Estados]],Resultado).
renovarCaminos([[Costo,Estado|Estados]|Caminos],[CostoE,EstadoE|EstadosE],[[Costo,Estado|Estados]|Resultado]) :-
    EstadosE \== [Costo,Estado|Estados], renovarCaminos(Caminos,[CostoE,EstadoE|EstadosE],Resultado).

/* Para resolver problemas con esta implementacion de A* */
resolver(Problema,Solucion) :-
    inicial(Problema,Estado),
    astar(Problema,[[0,Estado]],Solucion).

/* Ejemplo de Wikipedia */

/* Represento un estado como (nodo_actual,[nodos_posibles]) */

inicial(w,w(i,[a,d])).

objective(w,w(z,_)).

movimiento(w,w(i,[a,d]),M) :- member(M,[a,d]).
movimiento(w,w(a,[b]),M) :- member(M,[b]).
movimiento(w,w(b,[c]),M) :- member(M,[c]).
movimiento(w,w(c,[z]),M) :- member(M,[z]).
movimiento(w,w(d,[e]),M) :- member(M,[d]).
movimiento(w,w(e,[z]),M) :- member(M,[z]).

cost(w,w(i,[a,d]),a,1.5).
cost(w,w(i,[a,d]),d,2).
cost(w,w(a,[b]),b,2).
cost(w,w(b,[c]),c,3).
cost(w,w(c,[z]),z,4).
cost(w,w(d,[e]),e,3).
cost(w,w(e,[z]),z,2).

action(w,w(i,[a,d]),a,w(a,[b])).
action(w,w(i,[a,d]),d,w(d,[e])).
action(w,w(a,[b]),b,w(b,[c])).
action(w,w(b,[c]),c,w(c,[z])).
action(w,w(c,[z]),z,w(z,[])).
action(w,w(d,[e]),e,w(e,[z])).
action(w,w(e,[z]),z,w(z,[])).

heuristic(w,w(a,[b]),4).
heuristic(w,w(b,[c]),2).
heuristic(w,w(c,[z]),4).
heuristic(w,w(d,[e]),4.5).
heuristic(w,w(e,[z]),2).
