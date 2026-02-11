% Final 12/12/25

generarKPiezas(L, 2, [L1, L2]) :- length(L, LL),
                                  LL >= 2,
                                  append(Pre, Suf, L),
                                  last(Pre, E),
                                  L1 = Pre,
                                  append([E], Suf, L2),
                                  length(L1, LL1),
                                  LL1 >= 2,
                                  length(L2, LL2),
                                  LL2 >= 2.
generarKPiezas(L, K, [H | T]) :- K > 2, Kmm is K-1,
                                 append(H, R, L),
                                 last(H, E),
                                 append([E], R, Resto),
                                 length(H, LH),
                                 LH >= 2,
                                 generarKPiezas(Resto, Kmm, T).

permutacion([], []).
permutacion(L, [X | XS]) :- select(X, L, R),
                            permutacion(R, XS).

generarRompecabezas(S, R) :- length(S, LS), LS >= 2,
                             between(2, LS, K),
                             generarKPiezas(S, K, R).


% arboles
desde(X, X).
desde(X, Y) :- Xpp is X+1, desde(Xpp, Y).


arbolGeneral(A) :- desde(1, N), arbolSize(N, A).

arbolSize(1, x).
arbolSize(1, []).
arbolSize(N, L) :-
    N > 1,
    N1 is N - 1,
    listaArbolesSize(N1, L).

listaArbolesSize(0, []).
listaArbolesSize(N, [H | T]) :-
    N > 0,
    between(1, N, SH),
    arbolSize(SH, H),
    ST is N - SH,
    listaArbolesSize(ST, T).


%
vars(prop(N), [N]).
vars(neg(F), Vs) :- vars(F, Vs).
vars(and(F1,F2), Vs) :- vars(F1, V1), vars(F2, V2), union(V1, V2, Vs).
vars(or(F1,F2), Vs) :- vars(F1, V1), vars(F2, V2), union(V1, V2, Vs).
vars(imp(F1,F2), Vs) :- vars(F1, V1), vars(F2, V2), union(V1, V2, Vs).

asignar([], []).
asignar([V|Vs], [(V,true)|R]) :- asignar(Vs, R).
asignar([V|Vs], [(V,false)|R]) :- asignar(Vs, R).

ev(prop(N), Val, R) :- member((N,R), Val).

ev(neg(F), Val, true) :- ev(F, Val, false).
ev(neg(F), Val, false) :- ev(F, Val, true).

ev(and(F1,F2), Val, true) :- ev(F1, Val, true), ev(F2, Val, true).
ev(and(F1,F2), Val, false) :- ev(F1, Val, false).
ev(and(F1,F2), Val, false) :- ev(F2, Val, false).

ev(or(F1,F2), Val, true) :- ev(F1, Val, true).
ev(or(F1,F2), Val, true) :- ev(F2, Val, true).
ev(or(F1,F2), Val, false) :- ev(F1, Val, false), ev(F2, Val, false).

ev(imp(F1,F2), Val, true) :- ev(F1, Val, false).
ev(imp(F1,F2), Val, true) :- ev(F1, Val, true), ev(F2, Val, true).
ev(imp(F1,F2), Val, false) :- ev(F1, Val, true), ev(F2, Val, false).

esTautologia(F) :- vars(F, Vars), not((asignar(Vars, Val), ev(F, Val, false))).


%

unir([], Ys, Ys).
unir([X | Xs], Ys, [X | Zs]) :- unir(Xs, Ys, Zs).

prefijo(P, L) :- unir(P, _, L).