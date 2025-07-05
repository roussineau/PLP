
%% Programación Lógica - Parte 1 - Turno noche

%! entre(+X, +Y, -Z)
entre(X, Y, X) :- X =< Y.
entre(X, Y, Z) :- X < Y, N is X+1, entre(N, Y, Z). 

%! long(+L, ?N)
long([], 0).
long([_|T], N) :- long(T, M), N is 1 + M.

%! sacar(+X, +XS, -YS)
sacar(_, [], []).
sacar(X, [X|T], YS) :- sacar(X, T, YS).
sacar(X, [N|T], [N|YS]) :- X \= N, sacar(X, T, YS).

%! sinConsecRep(+XS, -YS)
sinConsecRep([], []).
sinConsecRep([X], [X]).
sinConsecRep([X, X|XS], YS) :- sinConsecRep([X|XS], YS).
sinConsecRep([X, Y|XS], [X|YS]) :- X \= Y, sinConsecRep([Y|XS], YS).

%! prefijo(+L, ?P)
prefijo(L, P) :- append(P, _, L).

sufijo(L, S) :- append(_, S, L).

%! sublista(+L, ?SL)
sublista(_, []).
sublista(L, SL) :- prefijo(L, P), sufijo(P, SL), SL \= [].

%! insertar(?X, +L, ?LX)
insertar(X, L, LX) :- append(P,S, L), append(P, [X|S], LX).

%! permutacion(+L, ?PS)
permutacion([], []).
permutacion([L|LS], PS) :- permutacion(LS, P), insertar(L, P, PS).