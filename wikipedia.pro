/************************/
/* Ejemplo de Wikipedia */
/************************/

/* Represento un estado como (nodo_actual,[nodos_posibles]) */

objective((z,_)).

movimiento((i,[a,d]),M) :- member(M,[a,d]).
movimiento((a,[b]),M) :- member(M,[b]).
movimiento((b,[c]),M) :- member(M,[c]).
movimiento((c,[z]),M) :- member(M,[z]).
movimiento((d,[e]),M) :- member(M,[e]).
movimiento((e,[z]),M) :- member(M,[z]).

cost((i,[a,d]),a,1.5).
cost((i,[a,d]),d,2).
cost((a,[b]),b,2).
cost((b,[c]),c,3).
cost((c,[z]),z,4).
cost((d,[e]),e,3).
cost((e,[z]),z,2).

action((i,[a,d]),a,(a,[b])).
action((i,[a,d]),d,(d,[e])).
action((a,[b]),b,(b,[c])).
action((b,[c]),c,(c,[z])).
action((c,[z]),z,(z,[])).
action((d,[e]),e,(e,[z])).
action((e,[z]),z,(z,[])).

heuristic((a,[b]),4).
heuristic((b,[c]),2).
heuristic((c,[z]),4).
heuristic((d,[e]),4.5).
heuristic((e,[z]),2).

/* Importante! */ 
heuristic((z,[]),0).      /* La heuristica del nodo final siempre es cero (por eso es final) */
