% Ejercicio 23:

% i) arbol(-A)

desde(X, X).
desde(X, Y) :- var(Y), X1 is X+1, desde(X1, Y).
desde(X, Y) :- nonvar(Y), X =< Y.

% arbol(A) :- desde(0, N), arbolNivel(A, N).

% arbolNivel(nil, 0).
% arbolNivel(bin(AI, _, AD), N) :-
%     N > 0,
%     Nm1 is N - 1,
%     between(0, Nm1, NI),
%     ND is Nm1 - NI,
%     arbolNivel(AI, NI),
%     arbolNivel(AD, ND).


% nodosEn(nil, _).
% nodosEn(bin(I, V, D), L) :- member(V, L), nodosEn(I, L), nodosEn(D, L).

% arbolSinRepEn(A, L) :- arbolSinRepEn1(A,L).
% arbolSinRepEn(A, L) :- arbolSinRepEn2(A,L).

% arbolSinRepEn1(nil, _).
% arbolSinRepEn1(bin(AI, V, AD), L) :-
%     append(LI, [V | LD], L),
%     arbolSinRepEn(AI, LI),
%     arbolSinRepEn(AD, LD).

% arbolSinRepEn2(bin(AI, V, AD), L) :-
%     append(LI, [V | LD], L),
%     arbolSinRepEn(AI, LD),
%     arbolSinRepEn(AD, LI),
%     not(arbolSinRepEn1(bin(AI, V, AD), L)).

% arbolSinRepEn(nil, _).
% arbolSinRepEn(bin(AI, V, AD), L) :-
%     member(V, L),
%     delete(L, V, LP),!,
%     append(LI, LD, LP),
%     arbolSinRepEn(AI, LI),
%     arbolSinRepEn(AD, LD).


% Ejercicio 20)

% proximoNumeroPoderoso(+X, -Y)
proximoNumeroPoderoso(X, Y) :-
    Xpp is X+1,
    Cota is X*X,
    between(Xpp, Cota, Y),
    todosFactoresAlMenosCuadrados(Y),
    not((between(Xpp, Y, Z), todosFactoresAlMenosCuadrados(Z), Z<Y)).

todosFactoresAlMenosCuadrados(1).
todosFactoresAlMenosCuadrados(X) :-
    primoMenorFactor(X, P),
    0 is X mod (P*P),
    sacarFactores(X, P, Rec),
    todosFactoresAlMenosCuadrados(Rec).

factoresPrimos(X, P) :-
    between(2, X, P),
    P is gcd(X, P),
    not((Pm1 is P-1, between(2, Pm1, Factor), 0 is P mod Factor)).

primoMenorFactor(X, P) :-
    between(2, X, P),
    P is gcd(X, P),
    not((between(2, X, Cand), 0 is X mod Cand, Cand < P)). 

sacarFactores(X, P, X) :- not(0 is X mod P).
sacarFactores(X, P, Res) :-
    factoresPrimos(X, P),
    Rec is X / P,
    sacarFactores(Rec, P, Res).


% miembro(X, [X | _]).
% miembro(X, [_ | XS]) :- miembro(X, XS).

esSublista(_, []).
esSublista(L, [X | XS]) :- member(X, L), esSublista(L, XS).




unico([X | XS], X) :- not(member(X, XS)).
unico([Y | XS], X) :- unico(XS, X), Y \= X.


%! sublista(+L, +S).
% sublista(_, []).
% sublista([X | LS], [X | XS]) :- append(XS, _, LS).
% sublista([Y | LS], [X | XS]) :- Y \= X, sublista(LS, [X | XS]).

%! esRotacion(+L, +R).
esRotacion(L, R) :- 
    append(L, L, LL),
    sublista(LL, R).


tokenizar(_, [], []).
tokenizar(D, L, [H | T]) :- member(H, D), append(H, Resto, L), tokenizar(D, Resto, T).




subsecuenciaCreciente(L, S) :- subsecuencia(L, S), creciente(S).

subsecuencia([], []).
subsecuencia([X | L], [X | S]) :- subsecuencia(L, S).
subsecuencia([_ | L], S) :- subsecuencia(L, S).

creciente(L) :- not((length(L, N), Nm1 is N-1, between(1, Nm1, I), J is I+1, nth1(I, L, EI), nth1(J, L, EJ), EJ < EI)).

subsecuenciaCrecienteMasLarga(L, S) :- subsecuenciaCreciente(L,S),
    not((subsecuenciaCreciente(L, P), length(S, LS), length(P, LP), LP > LS)),
    not((length(S, N), between(1, N, I), between(1, I, J), J \= I, nth1(I, S, EI), nth1(J, S, EJ), EI == EJ)).

fibonacci(X) :- desde(1, P), fiboPasos(P, X).

fiboPasos(1, 1).
fiboPasos(2, 1).
fiboPasos(N, X) :- N > 2, Nm1 is N-1, Nm2 is N-2, fiboPasos(Nm1, A), fiboPasos(Nm2, B), X is A+B.

miembro(X, [X | _]).
miembro(X, [_ | T]) :- miembro(X, T).

seFormaCon([], _).
seFormaCon([A | AS], B) :- miembro(A, B), seFormaCon(AS, B).




caminoDesde(Inicio, C) :- desde(0, N), caminoConNPasos(N, Inicio, C).

caminoConNPasos(0, I, [I]).
caminoConNPasos(N, I, [I | Resto]) :- N > 0, Nmm is N-1, caminar(I, Next), caminoConNPasos(Nmm, Next, Resto).

caminar((X, Y), (X, Ypp)) :- Ypp is Y+1.
caminar((X, Y), (X, Ymm)) :- Ymm is Y-1.
caminar((X, Y), (Xpp, Y)) :- Xpp is X+1.
caminar((X, Y), (Xmm, Y)) :- Xmm is X-1.



objeto(1, 50, 10).
objeto(2, 75, 15).
objeto(3, 60, 5).
objeto(4, 10, 1).


listaIDs([]).
listaIDs([X]) :- objeto(X, _, _).
listaIDs([X, Y | XS]) :- objeto(X, _, _), objeto(Y, _, _), X \= Y, listaIDs(XS), not((member(X, XS), member(Y, XS))).

% mochila(C, L) :- juntarObjetos(OS), listaPesos(OS, PS), sum_list(PS, SP), SP =< C, listaIDs(OS, L).


mochila(C, []) :- C >= 0.
mochila(C, [I | IS]) :- C > 0, objeto(I, P, _), NC is C - P, mochila(NC, IS), not(member(I, IS)), sort([I | IS], [I | IS]).

sumaValor([], 0).
sumaValor([I | IS], T) :- objeto(I, _, V), sumaValor(IS, VS), T is V + VS.

mejorMochila(C, L) :- mochila(C, L), sumaValor(L, S), not((mochila(C, M), sumaValor(M, SM), SM > S)).




sublista(_, []).
sublista([X | XS], [X | YS]) :- append(YS, _, XS).
sublista([_ | XS], YS) :- sublista(XS, YS).

noEsPrimo(X) :- Xmm is X-1, between(2, Xmm, D), 0 is X mod D.

sublistaDePrimos(L, P) :- sublista(L, P), not((member(X, P), noEsPrimo(X))).

sublistaMasLargaDePrimos(L, P) :- sublistaDePrimos(L, P), not((sublistaDePrimos(L, M), length(M, LM), length(P, LP), LM > LP)).


simbolo(a).
simbolo(b).

palabraLong(0, []).
palabraLong(N, [X | XS]) :- N > 0, simbolo(X), Nmm is N-1, palabraLong(Nmm, XS).

clausura(L) :- desde(0, N), palabraLong(N, L).

palabra(_, 0, []).
palabra(A, N, [X | XS]) :- N > 0, member(X, A), Nmm is N-1, palabra(A, Nmm, XS).

frase(A, F) :- desde(0, N), generarFraseDeNLetras(N, A, F).

generarFraseDeNLetras(0, _, []).
generarFraseDeNLetras(N, A, [P | PS]) :-
    between(1, N, LP),
    palabra(A, LP, P),
    R is N - LP,
    generarFraseDeNLetras(R, A, PS).


arbol(bin(bin(bin(nil,4,nil),2,bin(nil,5,bin(nil,6,nil))),1,bin(bin(nil,7,nil),3,nil))).


camino(bin(nil, R, nil), [R]).
camino(bin(I, R, _), [R | C]) :- camino(I, C).
camino(bin(_, R, D), [R | C]) :- camino(D, C).