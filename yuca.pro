
/* Yuca Novichsiana :D */


/* Represento un estado como (nodo_actual,[nodos_posibles]) */

objective((z,_)).

movimiento((a,[b,c]),M) :- member(M,[b,c]).
movimiento((b,[d,e]),M) :- member(M,[d,e]).
movimiento((c,[f]),M) :- member(M,[f]).
movimiento((d,[i]),M) :- member(M,[i]).
movimiento((e,[i]),M) :- member(M,[i]).
movimiento((i,[j]),M) :- member(M,[j]).
movimiento((j,[z]),M) :- member(M,[z]).
movimiento((f,[g]),M) :- member(M,[g]).
movimiento((g,[k]),M) :- member(M,[k]).
movimiento((k,[z]),M) :- member(M,[z]).

cost((a,[b,c]),b,1).
cost((a,[b,c]),c,3).
cost((b,[d,e]),d,2).
cost((b,[d,e]),e,14).
cost((d,[i]),i,3).
cost((e,[i]),i,17).
cost((i,[j]),j,5).
cost((j,[z]),z,50).
cost((c,[f]),f,7).
cost((f,[g]),g,4).
cost((g,[k]),k,20).
cost((k,[z]),z,2).

action((a,[b,c]),b,(b,[d])).
action((a,[b,c]),c,(c,[f])).
action((b,[d,e]),d,(d,[i])).
action((b,[d,e]),e,(e,[i])).
action((d,[i]),i,(i,[j])).
action((e,[i]),i,(i,[j])).
action((i,[j]),j,(j,[z])).
action((j,[z]),z,(z,[])).
action((c,[f]),f,(f,[g])).
action((f,[g]),g,(g,[k])).
action((g,[k]),k,(k,[z])).
action((k,[z]),z,(z,[])).

heuristic((b,[d,e]),3).
heuristic((d,[i]),4).
heuristic((e,[i]),2).
heuristic((i,[j]),2).
heuristic((j,[z]),2).
heuristic((c,[f]),7).
heuristic((f,[g]),1).
heuristic((g,[k]),2).
heuristic((k,[z]),1).
heuristic((z,[]),0).
