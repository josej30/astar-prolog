/* Definición de A* */

/* Que necesita?
* 
* -> Nombre del problema a resolver
* -> Lista de caminos explorados hasta el momento 
*    con su respectivo costo final
* <- Costo y estado final
*/

astar(Problema,[[Costo,Estado|Estados]|_],Solucion) :- 
    objective(Problema,Estado),
    append([Costo],[Estado],Solucion).

astar(Problema,[Caminos],Solucion) :-
    procesar(Problema,[Caminos],Nuevos),
    astar(Problema,Nuevos,Solucion).

procesar(Problema,[[Costo,Estado|Estados]|Caminos],Resultado) :-
    renovar(Posibles,[[Costo,Estado|Estados]|Caminos],Resultado),
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
    ).
procesar(_,_,[]).

renovar([[CostoNuevo,EstadoNuevo|R1]|Ns],[[Costo,Estado|Estados]|Caminos],Resultado) :-
    caminoMinimo([[CostoNuevo,EstadoNuevo|R1]|Ns],CM),
    limpiarCaminos([[CostoNuevo,EstadoNuevo|R1]|Ns],CM,Resultado).

caminoMinimo([[CostoNuevo,_|_]|Ns],CostoMinimo) :-
    caminoMinimo(Ns,CM), CostoMinimo is min(CM,CostoNuevo).
caminoMinimo([[CostoMinimo,_|_]],CostoMinimo).
























limpiarCaminos([],_,_).

limpiarCaminos([[CM,EstadoNuevo|R1]|Ns],CM,Resultado) :- 
    append([[CM,EstadoNuevo|R1]],[],Resultado), limpiarCaminos(Ns,CM,RestoCaminos).

limpiarCaminos([[CostoNuevo,EstadoNuevo|R1]|Ns],CM,Resultado) :- 
    append([R1],[],Resultado), limpiarCaminos(Ns,CM,RestoCaminos).
