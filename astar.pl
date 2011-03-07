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
astar(Problema,[Caminos],Solucion) :- 
    comprobar(Problema,[Caminos],Solucion).
astar(Problema,[Caminos],Solucion) :-
    procesar(Problema,[Caminos],Nuevos),
    astar(Problema,Nuevos,Solucion).

comprobar(Problema,[[Costo,Estado|Estados]|Resto],Solucion) :-
    objective(Problema,Estado),
    append([Costo],[Estado],Solucion).
comprobar(Problema,[[Costo,Estado|Estados]|Resto],Solucion) :-
    comprobar(Problema,Resto,Solucion).
comprobar(Problema,[],[]).


/* Revisar que el findall lo este logrando bien */
procesar(Problema,[[Costo,Estado|Estados]|Caminos],Resultado) :-
    findall(
	[CostoNuevo,EstadoNuevo,Costo,Estado|Estados],
	( 
	  movimiento(Problema,Estado,Movimiento),
	  cost(Problema,Estado,Movimiento,CostoMoverse),
	  action(Problema,Estado,Movimiento,EstadoNuevo),
	  heuristic(Problema,EstadoNuevo,CostoHeu),
	  CostoNuevo is CostoHeu + CostoMoverse
	),
	Posibles
    ),
    renovar(Posibles,[[Costo,Estado|Estados]|Caminos],Resultado).
procesar(_,_,[]).

renovar([[CostoNuevo,EstadoNuevo|R1]|Ns],Originales,Resultado) :-
    caminoMinimo([[CostoNuevo,EstadoNuevo|R1]|Ns],CM),
    escogerCamino([[CostoNuevo,EstadoNuevo|R1]|Ns],CM,CaminoEscogido),
    renovarCaminos(Originales,CaminoEscogido,Resultado).

caminoMinimo([[CostoNuevo,_|_]|Ns],CostoMinimo) :-
    caminoMinimo(Ns,CM), CostoMinimo is min(CM,CostoNuevo).
caminoMinimo([[CostoMinimo,_|_]],CostoMinimo).

escogerCamino([],_,[]).
escogerCamino([[CM,EstadoNuevo|R1]|Ns],CM,[CM,EstadoNuevo|R1]) :- 
    escogerCamino(Ns,CM,RestoCaminos).
escogerCamino([[CostoNuevo,EstadoNuevo|R1]|Ns],CM,RestoCaminos) :- 
    CostoNuevo \== CM, escogerCamino(Ns,CM,RestoCaminos).

renovarCaminos([],_,[]).
renovarCaminos([Camino|CaminosO],[CostoE,EstadoE|Camino],[[CostoE,EstadoE|Camino]|Resultado]) :-
    renovarCaminos(CaminosO,[CostoE,EstadoE|Camino],Resultado).
renovarCaminos([CaminoO|CaminosO],[CostoE,EstadoE|CaminoE],[CaminoO|Resultado]) :-
    CaminoO \== CaminoE, renovarCaminos(CaminosO,[CostoE,EstadoE|CaminoE],Resultado).

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
