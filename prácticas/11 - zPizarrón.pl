%iesimo(+I, +L, -X)
iesimo(0, [X|_], X).
iesimo(I, [X|XS], Y) :- I2 is I-1, iesimo(I2, XS, Y).


%iesimo(?I, +L, -X)
iesimo2(I, L, X) :- nonvar(I), I >= 0, length(L1, I), append(L1, [X|_], L).
iesimo2(I, L, X) :- var(I), append(L1, [X|_], L), length(L1, I).



%desde(+X, -Y)
desde(X, X).
desde(X, Y) :- N is X+1, desde(N, Y).


%desdeReversible(+X, ?Y)
desdeReversible(X,Y) :- var(Y), desde(X,Y).
desdeReversible(X,Y) :- nonvar(Y), X =< Y.


%pmq(+X, -Y)
pmq(X, Y) :- between(0, X, Y),  par(Y).
            % generamos los y, testeamos que cumplan

%par(+Y)
par(Y) :- 0 =:= Y mod 2.


%%%NO HACER ESTO!!!!!!!!!!!!!!!!!!!!!!!!!
%pmq2(+X, -Y)
pmq2(X,Y) :- pares(Y), between(0, X, Y).
%pares(-Y)
pares(Y) :- desdeReversible(0, Y), par(Y).


%coprimos(-X, -Y)
coprimos(X, Y) :- generarPares(X,Y), X > 0, Y > 0, 1 =:= gcd(X,Y).

%generarPares(-X, -Y)
generarPares(X,Y) :- desde(0, N), paresQueSuman(N, X, Y).

%paresQueSuman(+N, -X, -Y)
paresQueSuman(N, X, Y) :- between(0, N, X), Y is N-X.


progenitorx(yocasta,edipo).
progenitorx(yocasta,antigona).
progenitorx(edipo,antigona).

abuela(X,Y) :- progenitorx(X,Z), progenitorx(Z,Y).

pariente(X,Y) :- progenitorx(X,Y), not(abuela(X,Y)).
pariente(X,Y) :- abuela(X,Y).

%EL NOT NO INSTANCIA 


%corteMásParejo(+L,-L1,-L2)
corteMasParejo(L, I, D) :- append(I, D, L), not(hayUnCorteMasParejo(I,D,L)).

hayUnCorteMasParejo(I,D,L) :- append(I2, D2, L), esMasParejo(I2, D2, I, D).

esMasParejo(I2, D2, I, D) :- sum_list(I2, SI2), sum_list(D2, SD2), 
                             sum_list(I, SI), sum_list(D, SD), 
                             abs(SI - SD) >  abs(SI2 - SD2).




%proximoPrimo(+N,-P) --> instancia P en el menor primo >= N
proximoPrimo(N, N2) :- N2 is N+1, esPrimo(N2).
proximoPrimo(N, P)  :- N2 is N+1, not(esPrimo(N2)), proximoPrimo(N2, P).


% esPrimo(+N)
esPrimo(N) :- N > 1, not(tieneUnDivisorNoTrivial(N)).
tieneUnDivisorNoTrivial(N) :- N1 is N-1, between(2, N1, D), 0 =:= N mod D.













% esTriangulo(+T)
esTriangulo(tri(A,B,C)) :- valido(A,B,C), valido(B,C,A),valido(C,A,B).
% valido(+A, +B, +C)
valido(A, B, C) :- A < B + C.



%perimetro(?T, ?P)
perimetro(tri(A,B,C), P) :- ground(tri(A,B,C)), esTriangulo(tri(A,B,C)), P is A + B + C.
perimetro(tri(A,B,C), P) :- not(ground(tri(A,B,C))), triplasQueSuman(P, A, B, C), esTriangulo(tri(A,B,C)).

%triplasQueSuman(?P, -A, -B, -C)
triplasQueSuman(P, A, B, C) :- 
    desdeReversible(0,P), 
    between(1,P,A), between(1,P,B), 
    C is P - A - B,  C > 0.


%triangulo(-T)
triangulo(T) :- perimetro(T, _).