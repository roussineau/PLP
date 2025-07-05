
%! entre(+X,+Y,-Z)
% sea verdadero cuando el número entero Z esté ...
entre(X, Y, X) :-
	X =< Y.
entre(X, Y, Z) :-
	X =< Y,
	Xp1 is X + 1,
	entre(Xp1, Y, Z).


%! long(+L, -N).
long([], 0).
long([_H|T], N) :-
	long(T, Nm1),
	N is Nm1 + 1.

% sacar(X, XS, YS).
sacar(_, [], []).
sacar(X, [X|XS], YS) :-
	sacar(X, XS, YS).
sacar(X, [A|XS], [A|RES]) :-
	X \= A,
	sacar(X, XS, RES).

% prefijo(_, []).
% prefijo([H|L], [H|T]) :-
% 	prefijo(L, T).

prefijo(L, P) :-
	append(P, _, L).

sufijo(L, P) :-
	append(_, P, L).

sublista(_, []).
sublista(L, SL) :-
	append(_, SLYAlgoMas, L),
	append(SL, _, SLYAlgoMas),
	SL \= [].

insertar(X, L, LX) :-
	append(A, B, L),
	append(A, [X|B], LX).

permutacion([], []).
permutacion([H|T], P) :-
	permutacion(T, Q),
	 insertar(H, Q, P).