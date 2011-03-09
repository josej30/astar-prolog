/* Implementacion de A* */

/* 
*
* astar
* 
* -> Lista de caminos explorados hasta el momento 
*    con su respectivo costo final
* -> Lista de nodos visitados
* <- Costo final y camino recorrido
*
*/

astar([[Costo,Estado|Estados]|Caminos],Visitados,[Costo|Solucion]) :- 
    objective(Estado),
    salida([Costo,Estado|Estados],Solucion).
astar([[Costo,Estado|Estados]|Caminos],Visitados,Solucion) :-
    procesar([[Costo,Estado|Estados]|Caminos],Visitados,Posibles),
    renovar(Posibles,[[Costo,Estado|Estados]|Caminos],Visitados,VisitadosNuevos,Nuevos),
    astar(Nuevos,VisitadosNuevos,Solucion).

salida([],[]).
salida([Costo,Estado|Estados],[Estado|Solucion]) :-
    salida(Estados,Solucion).

/* 
*
* procesar
*
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
procesar([[Costo,Estado|Estados]|Caminos],Visitados,P) :-
    findall(
	[CostoEstrella,CostoReal,EstadoNuevo,Costo,Estado|Estados],
	( 
	  movimiento(Estado,Movimiento),          
	  cost(Estado,Movimiento,CostoMoverse),
	  action(Estado,Movimiento,EstadoNuevo),
          \+ member(EstadoNuevo,Visitados),
	  heuristic(EstadoNuevo,CostoHeu),
	  CostoEstrella is Costo + CostoHeu + CostoMoverse,
          CostoReal is CostoMoverse + Costo
	),
	Posibles
    ),!,
    procesar(Caminos,Visitados,Posibles2),
    append(Posibles,Posibles2,P).
procesar(_,_,[]).


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


/* Resolver un problema */

a_star(Start,Actions) :-
    astar([[0,Start]],[Start],Actions).
