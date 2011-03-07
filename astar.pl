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
astar(Problema,[[Costo,Estado|Estados]|_],Solucion) :- 
    objective(Problema,Estado),
    append([Costo],[Estado],Solucion).


astar(Problema,[Caminos],Solucion) :-
    procesar(Problema,[Caminos],Nuevos),
    astar(Problema,Nuevos,Solucion).


/* Revisar que el findall lo este logrando bien */
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
