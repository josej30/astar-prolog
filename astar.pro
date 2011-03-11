/* Implementacion de A* */

astar([[Costo,Estado|Estados]|Caminos],Resultado):-
    final([[Costo,Estado|Estados]|Caminos],Resp),
    salida(Resp,Invertida),
    reverse(Invertida,Resultado).
astar([[Costo,Estado|Estados]|Caminos],Resultado) :-
    procesar([[Costo,Estado|Estados]|Caminos],Procesados),
    astar(Procesados,Resultado),!.

salida([Costo,Estado,Accion|Estados],[Accion|Solucion]) :-
    salida(Estados,Solucion).
salida(_,[]).

final([[Costo,Estado|Estados]|Caminos],[Costo,Estado|Estados]) :- 
    objective(Estado),!.
final([[Costo,Estado|Estados]|Caminos],Resultado) :- 
    final(Caminos,Resultado).

procesar(CaminosViejos,Procesados) :-
    buscarProximo(CaminosViejos,Proximo),
    delete(CaminosViejos,Proximo,NuevosCaminos),
    expandirProximo(Proximo,Proximos),
    insertIntel(Proximos,NuevosCaminos,Procesados).

insertIntel([P|PS],C,Rest) :-
    insertI(P,C,C2),!,
    insertIntel(PS,C2,Rest).
insertIntel([],X,X).

insertI([],[],[]).

insertI(X,[],[X]).

insertI([],[[Costo2|Estados2]|Caminos],Ret2) :-
    append([[Costo2|Estados2]],Ret,Ret2),
    insertI([],Caminos,Ret).

insertI([Costo|Estados],[[Costo2|Estados2]|Caminos],Ret2) :-
    Costo2 < Costo,
    append([[Costo2|Estados2]],Ret,Ret2),
    insertI([Costo|Estados],Caminos,Ret).

insertI([Costo|Estados],[[Costo2|Estados2]|Caminos],Ret2) :-
    Costo2 >= Costo,
    append([[Costo|Estados]],[[Costo2|Estados2]],X),
    append(X,Ret,Ret2),
    insertI([],Caminos,Ret).

expandirProximo([Costo,Estado|Estados],Proximos) :- 
     findall(
	[CostoNuevo,EstadoNuevo,X,Costo,Estado|Estados],
	( 
	  action(Estado,X,EstadoNuevo),
	  cost(Estado,X,CostoMoverse),
          CostoNuevo is CostoMoverse + Costo
	),
	Proximos
    ).

buscarProximo([[Costo,Estado|Estados]|Caminos],Proximo) :-
    ponerHeuristicas([[Costo,Estado|Estados]|Caminos],Hs,Hm),
    seleccionarProximo(Hs,Hm,Proximo).

seleccionarProximo([[N,Costo,Estado|Estados]|R],N,[Costo,Estado|Estados]):-!.
seleccionarProximo([[N,Costo,Estado|Estados]|R],Hm,Resultado) :-
    seleccionarProximo(R,Hm,Resultado).

ponerHeuristicas([[Costo,Estado|Estados]],[[N,Costo,Estado|Estados]],N) :- 
    heuristic(Estado,H),
    N is H + Costo.
ponerHeuristicas([[Costo,Estado|Estados]|Caminos],[[N,Costo,Estado|Estados]|R],MinH) :-
    ponerHeuristicas(Caminos,R,Min),
    heuristic(Estado,H),
    N is H + Costo,
    MinH is min(Min,N),!.

a_star(Start,Actions) :-
    astar([[0,Start]],Actions).

min(X,Y,X) :- X=<Y,!.
min(X,Y,Y) :- Y<X,!.

/*

Correlo asi:

a_star(state([shemp,larry,moe,curly],[],left),X), showmoves(state([shemp,larry,moe,curly],[],left),X). 

*/

