

desdeReversible(X,X).
desdeReversible(X, Y) :- var(Y), N is X+1, desdeReversible(N, Y).
desdeReversible(X, Y) :- nonvar(Y), X =< Y.


% NEGACIÓN POR FALLA:

%! primo(+N).
primo(N) :-
    N>1,
    Nm1 is N-1,
    not((between(2, Nm1, D),
    N mod D =:= 0)).

%! siguientePrimo(+N, ?P).
siguientePrimo(P, P) :-
    primo(P).
siguientePrimo(N, P) :-
    not(primo(N)), N1 is N+1, siguientePrimo(N1, P).

%! proximoPrimo(+N, ?P)
proximoPrimo(N, P) :-
    N1 is N+1,
    siguientePrimo(N1, P).


% METAPREDICADOS:

esTriangulo(tri(A, B, C)) :-
    A > 0, B > 0, C > 0,
    A < B + C, B <A + C, C < A + B.


%! perimetro(?T, ?P).
perimetro(tri(A, B, C), P) :-
    ground(tri(A, B, C)),
    esTriangulo(tri(A, B, C)),
    P is tri(A, B, C).

perimetro(tri(A, B, C), P) :-
    not(ground(tri(A, B, C))),
    desdeReversible(3, P),
    generarTriplas(P, A, B, C),
    esTriangulo(tri(A, B, C)).

generarTriplas(P, A, B, C) :-
    between(1, P, A),
    LimiteB is P - A,
    between(1, LimiteB, B),
    C is P - A - B.