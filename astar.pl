/* Implementacion de A* */

/* 
*
* astar
* 
* -> Nombre del problema a resolver
* -> Lista de caminos explorados hasta el momento 
*    con su respectivo costo final
* -> Lista de nodos visitados
* <- Costo final y camino recorrido
*
*/

astar(Problema,[[Costo,Estado|Estados]|Caminos],Visitados,[Costo|Solucion]) :- 
    objective(Problema,Estado),
    salida([Costo,Estado|Estados],Solucion).
astar(Problema,[[Costo,Estado|Estados]|Caminos],Visitados,Solucion) :-
    procesar(Problema,[[Costo,Estado|Estados]|Caminos],Visitados,Posibles),
    renovar(Posibles,[[Costo,Estado|Estados]|Caminos],Visitados,VisitadosNuevos,Nuevos),
    astar(Problema,Nuevos,VisitadosNuevos,Solucion).

salida([],[]).
salida([Costo,Estado|Estados],[Estado|Solucion]) :-
    salida(Estados,Solucion).

/* 
*
* procesar
*
* -> Nombre del problema 
* -> Lista de caminos abiertos hasta el momento
* -> Lista de nodos visitados
* <- Lista con los nuevos posibles estados que se van a expandir.
*    
*    Nota: 
*    Dentro de esa lista están todos los nuevos (posibles) caminos expandidos
*    sin tomar en cuenta caminos que lleguen a un nodo al que ya se
*    habia llegado previamente, ya que al ser A*, si llegaste antes
*    al nodo fue por un camino de menor costo.
*
*/
procesar(Problema,[[Costo,Estado|Estados]|Caminos],Visitados,P) :-
    findall(
	[CostoEstrella,CostoReal,EstadoNuevo,Costo,Estado|Estados],
	( 
	  movimiento(Problema,Estado,Movimiento),          
	  cost(Problema,Estado,Movimiento,CostoMoverse),
	  action(Problema,Estado,Movimiento,EstadoNuevo),
          \+ member(EstadoNuevo,Visitados),
	  heuristic(Problema,EstadoNuevo,CostoHeu),
	  CostoEstrella is Costo + CostoHeu + CostoMoverse,
          CostoReal is CostoMoverse + Costo
	),
	Posibles
    ),!,
    procesar(Problema,Caminos,Visitados,Posibles2),
    append(Posibles,Posibles2,P).
procesar(_,_,_,[]).


/* 
*
* renovar
*
* -> Lista de caminos expandidos posibles para escoger
* -> Lista de caminos que se encuentran abiertos
* -> Lista con los estados visitados originales
* <- Lista con los estados visitados luego de escoger el nuevo camino
* <- Lista con los caminos originales masa el camino escogido para expandir
*    
*    Nota: 
*    Notese que el nuevo estado que se expande siempre se coloca de primero en
*    la lista resultante. Esto nos asegura que podemos buscar el nodo final 
*    a salvo siempre en el primer elemento de la lista de caminos resultantes.
*
*/
renovar([[CostoEstrella,CostoReal,EstadoNuevo|R1]|Ns],[[Costo,Estado|Estados]|Caminos],VV,[EstadoN|VV],
        [[CostoN,EstadoN|EstadosN]|[[Costo,Estado|Estados]|Caminos]]) :-
    caminoMinimo([[CostoEstrella,CostoReal,EstadoNuevo|R1]|Ns],CM),
    renovarCaminos([[CostoEstrella,CostoReal,EstadoNuevo|R1]|Ns],CM,[CostoN,EstadoN|EstadosN]).


/* 
*
* caminoMinimo
*
* -> Lista de posibles caminos para ser expandidos
* <- Costo minimo que se escoge en este paso de A*
*    
*    Resumen: 
*    Basicamente se busca en todos los caminos posibles para
*    expandir el que tenga el costo menor de g(x); recordando que
*    g(x) = f(x) + h(x)
*
*/
caminoMinimo([[CostoEstrella,_,_|_]|Ns],CostoMinimo) :-
    caminoMinimo(Ns,CM), CostoMinimo is min(CM,CostoEstrella).
caminoMinimo([[CostoEstrella,_,_|_]],CostoEstrella).


/* 
*
* renovarCaminos
*
* -> Lista con todos los caminos posibles ya expandidos
* -> Costo minimo que se escoge en este paso de A*
* <- Camino de costo minimo que se escoge en este paso de A*
*   
*    Resumen: 
*    Se revisan todos los posibles caminos hasta encontrar el del
*    costo minimo.
*
*/
renovarCaminos([],_,[]).
renovarCaminos([[CM,CostoReal,EstadoNuevo|R1]|Ns],CM,[CostoReal,EstadoNuevo|R1]).
renovarCaminos([[CostoEstrella,CostoReal,EstadoNuevo|R1]|Ns],CM,RestoCaminos) :- 
    CostoEstrella \== CM, renovarCaminos(Ns,CM,RestoCaminos).

/*---------- Hasta aqui es A* ------------*/


/* Predicado para resolver comodamente los problemas en A* */

resolver(Problema,Solucion) :-
    inicial(Problema,Estado),
    astar(Problema,[[0,Estado]],[Estado],Solucion).

/************************/
/* Ejemplo de Wikipedia */
/************************/

/* Represento un estado como (nodo_actual,[nodos_posibles]) */

inicial(w,w(i,[a,d])).

objective(w,w(z,_)).

movimiento(w,w(i,[a,d]),M) :- member(M,[a,d]).
movimiento(w,w(a,[b]),M) :- member(M,[b]).
movimiento(w,w(b,[c]),M) :- member(M,[c]).
movimiento(w,w(c,[z]),M) :- member(M,[z]).
movimiento(w,w(d,[e]),M) :- member(M,[e]).
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

/* Importante! */ 
heuristic(w,w(z,[]),0).      /* La heuristica del nodo final siempre es cero (por eso es final) */
