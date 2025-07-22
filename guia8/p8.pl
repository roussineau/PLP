% El motor de búsqueda de Prolog:

% Ejercicio 1)
padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).
abuelo(X, Y) :- padre(X, Z), padre(Z, Y).
% i. juan.
% ii.
hijo(X, Y) :- padre(Y, X).
hermano(X, Y) :- padre(Z, X), padre(Z, Y), X \= Y.
descendiente(X, Y) :- padre(Y, X).
descendiente(X, Y) :- padre(Y, Z), descendiente(X, Z).
% iv. abuelo(juan, X).
% v. hermano(pablo, X).
% vi.
ancestro(X, X).
ancestro(X, Y) :- ancestro(Z, Y), padre(X, Z).
/* vii.
    La recursión se vuelve infinita si para resolver P(X) con X sin instanciar necesito
    resolver P(Y) con Y sin instanciar. El problema que se intenta resolver es el mismo
    en ambos casos, y entonces el programa se cuelga.
*/
% viii.
ancestroCorregido(X, Y) :- padre(X, Y).
ancestroCorregido(X, Y) :- padre(X, Z), ancestroCorregido(Z, Y).


% Ejercicio 2)
vecino(X, Y, [X | [Y | _]]).
vecino(X, Y, [_ | Ls]) :- vecino(X, Y, Ls).
% Son vecinos si y sólo si X está la izquierda de Y.
/* i. Mostrar el árbol de búsqueda en Prolog de:
    ?- vecino(5, Y, [5, 6, 5, 3]).
        true. {Y := 6}
        ?- vecino(5, Y, [6, 5, 3]).
            false.
            ?- vecino(5, Y, [5, 3]).
                true. {Y := 3}
                ?- vecino(5, Y, [3]).
                    false.
                    ?- vecino(5, Y, []).
                        false.
    ii. Si se invierte el orden de las reglas:
    vecino(X, Y, [W | Ls]) :- vecino(X, Y, Ls).
    vecino(X, Y, [X | [Y | Ls]]).
    ?- vecino(5, Y, [5, 6, 5, 3]).
        ?- vecino(5, Y, [6, 5, 3]).
            ?- vecino(5, Y, [5, 3]).
                ?- vecino(5, Y, [3]).
                    ?- vecino(5, Y, []).
                        false.
                    false.
                true. {Y := 3}
            false.
        true. {Y := 6}
*/


% Ejercicio 3)
natural(0).
natural(suc(X)) :- natural(X).
menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).
menorOIgual(X, X) :- natural(X).
/*
    i. Explicar qué sucede al consultar menorOIgual(0, X).
    ?- menorOIgual(0, X).
        ?- menorOIgual(0, Y). {X := suc(Y)}
            ?- menorOIgual(0, Z). {Z := suc(Y)}
                ... Hasta stack overflow.
    Lo que está pasando es lo mismo que en el punto 1.vii), para resolver
    P(X) con X sin instanciar estoy necesitando resolver P(Y) para Y sin
    instanciar, causando una recursión infinita porque son en sí el mismo
    problema.

    ii. Describir las circunstancias en las que puede colgarse un programa
    en Prolog, es decir, ejecutarse infinitamente sin arrojar soluciones.

    Esto puede pasar cuando el problema que intentamos resolver requiere que
    resolvamos el mismo problema por dentro. Pasa cuando trabajamos con 
    variables no instanciadas sobre todo.
*/
% iii.
menorOIgualCorregido(X, X) :- natural(X).
menorOIgualCorregido(X, suc(Y)) :- natural(Y), menorOIgualCorregido(X, Y).
/*
    Básicamente estamos instanciando el Y antes de hacer el llamado recursivo.
*/


% Operaciones sobre listas:

% Ejercicio 4)
juntar([], Ys, Ys).
juntar([X | Xs], Ys, [X | Ls]) :- juntar(Xs, Ys, Ls).


% Ejercicio 5) 
% Definir los siguientes predicados USANDO APPEND.

% i.
last(Xs, U) :- append(_, [U], Xs).

% ii.
reversa([], []).                                  
reversa([X | Xs], Ys) :- reversa(Xs, Zs), append(Zs, [X], Ys).
/*
    Observación: ¿por qué es necesario el caso base?
    Seguimiento de consulta:
    ?- reversa([a, b, c], Q).
        ?- reversa([b, c], Zs), append(Zs, [a], Q). {X := a, Xs := [b, c], Ys := Q}
            ?- reversa([c], As), append(As, [b], Zs), append(Zs, [a], Q).
                ?- reversa([], Bs), append(Bs, [c], As), append(As, [b], Zs), append(Zs, [a], Q).
    En este punto, sin caso base cortamos con false. Con caso base, podemos segui así:
                    ?- append([], [c], As), append(As, [b], Zs), append(Zs, [a], Q). {Bs := []}
                        ?- append([c], [b], Zs), append(Zs, [a], Q). {As := [c]}
                            ?- append([c, b], [a], Q). {Zs := [c, b]}
                                true. {Q := [c, b, a]}
*/
% iii.
% prefijo([], _).
% prefijo([X | Xs], [X | Ls]) :- prefijo(Xs, Ls).
prefijo(P, L) :- append(P, _, L).
% iv.
sufijo(S, L) :- append(_, S, L).
% v.
sublista([], _).
sublista(S, L) :- append(_, P, L), append(S, _, P), S \= [].
% vi.
pertenece(X, L) :- sublista([X], L).


% Ejercicio 6)
aplanar([], []).
aplanar([X | Xs], [X | Ys]) :- not(is_list(X)), aplanar(Xs, Ys).
aplanar([X | Xs], Ys) :- is_list(X), aplanar(X, Y), aplanar(Xs, Z), append(Y, Z, Ys).


% Ejercicio 7)
% Definir los siguientes predicados usando MEMBER o APPEND según sea conveniente.
% i.

% interseccion(+L1, +L2, -L3)
interseccion([], _, []).
interseccion([X | Xs], Ys, [X | L]):- member(X, Ys), sacarApariciones(X, Xs, M), interseccion(M, Ys, L).
interseccion([X | Xs], Ys, L):- not(member(X, Ys)), interseccion(Xs, Ys, L).

sacarApariciones(_, [], []).
sacarApariciones(X, [X | Xs], L):- sacarApariciones(X, Xs, L).
sacarApariciones(X, [Y | Xs], [Y | L]):- X \= Y, sacarApariciones(X, Xs, L).

% partir(N, L, L1, L2) (Preguntar sobre cuán reversible es el predicado).
partir(N, L, L1, L2) :- prefijo(L1, L), sufijo(L2, L), length(L1, N), append(L1, L2, L).

% ii.
% borrar(+ListaOriginal, +X, -ListaSinXs)
borrar([], _, []).
borrar([E | Xs], E, Zs) :- member(E, Xs), borrar(Xs, E, Zs).
borrar([E | Xs], E, Zs) :- not(member(E, Xs)), append([], Zs, Xs).
borrar([X | Xs], E, [X | Zs]) :- X \= E, borrar(Xs, E, Zs).

% iii.
% sacarDuplicados(+L1, -L2).
sacarDuplicados([], []).
sacarDuplicados([X | L1], [X | L2]) :- borrar(L1, X, L3), sacarDuplicados(L3, L2).

% iv.
% permutacion(+L1, ?L2).
permutacion([],[]).
permutacion([X | XS], L) :- permutacion(XS, Y), borrarUno(X, L, Y), member(X, L).

borrarUno(_, [], []).
borrarUno(X, [X | XS], XS).
borrarUno(X, [Y | YS], [Y | L]) :- X \= Y, borrarUno(X, YS, L).

% v. reparto(+L, +N, -LListas).
% LListas debe ser una lista de N >= 1 listas de cualquier longitud tales que al concatenarlas se obtiene L.
reparto(X, 1, [X]).
reparto(L, N, [X | Ls]) :-
    N > 1,
    Nm1 is N-1,
    append(X, Xs, L),
    reparto(Xs, Nm1, Ls).

% vi.
% repartoSinVacias(+L, -LListas).
repartoSinVacias(X, [X]) :- length(X, LX), LX > 0.
repartoSinVacias(Original, [X | Xs]) :-
    append(X, Ys, Original),
    length(X, LX),
    LX > 0,
    repartoSinVacias(Ys, Xs).


% Ejercicio 8)
% parteQueSuma(+L, +S, -P).
parteQueSuma(_, 0, []).
parteQueSuma([X | Ls], S, [X | Xs]) :-
    Resto is S-X,
    Resto >= 0,
    parteQueSuma(Ls, Resto, Xs).
parteQueSuma([_ | Ls], S, P) :-
    S >= 0,
    parteQueSuma(Ls, S, P).



% Instanciación y reversibilidad:

% Ejercicio 9)
desde(X, X).
desde(X, Y) :- N is X+1, desde(N, Y).
/*
    i) Si instanciamos solo el primer parámetro, podemos tener los números desde
    X hasta el infinito, pero es la idea de programa así que no está colgado.
    
    Si instanciamos solo el segundo parámetro, unifica solo con la primera ecuación,
    pues la segunda requiere que el primero esté instanciado. Luego da error.
    Si instanciamos ambos, nos da una lista desde X hasta Y, pero para que no se
    cuelgue requerimos que X sea menor o igual a Y.

    ii) Dar una versión del predicado que funcione con la instanciación:
*/
% desdeReversible(+X, ?Y). Preguntar como hacer sin el corte.
desdeReversible(X,X).
desdeReversible(X,Y) :- nonvar(Y), X =< Y.
desdeReversible(X,Y) :- var(Y), N is X+1, desde(N,Y).


% Ejercicio 10)
% intercalar(L1, L2, L3).
intercalar(X, [], X).
intercalar([], X, X).
intercalar([X | Xs], [Y | Ys], [X | [Y | Zs]]) :- intercalar(Xs, Ys, Zs).
/*
    Es reversible, aunque hay que tener ojo con la implementación. Usando
    append se puede colgar al final.
*/


% Ejercicio 11)
vacio(nil).

raiz(bin(_, V, _), V).

altura(nil, 0).
altura(bin(Izq, _V, Der), H) :-
    altura(Izq, HI),
    altura(Der, HD),
    Max is max(HI, HD),
    H is 1+Max.

cantidadDeNodos(nil, 0).
cantidadDeNodos(bin(Izq, _V, Der), N) :-
    cantidadDeNodos(Izq, NI),
    cantidadDeNodos(Der, ND),
    N is 1+NI+ND.


% Ejercicio 12)
% i. inorder(+AB, -Lista)
inorder(nil, []).
inorder(bin(Izq, V, Der), Ls) :-
    inorder(Izq, OI),
    inorder(Der, OD),
    append(OI, [V | OD], Ls).

% ii. arbolConInorder(+Lista, -AB).
arbolConInorder([], nil).
arbolConInorder(L, bin(Izq, V, Der)) :-
    append(LI, [V | LD], L),
    arbolConInorder(LI, Izq),
    arbolConInorder(LD, Der).

% iii.
% aBB(+T).
aBB(nil).
aBB(bin(Izq, V, Der)) :-
    inorder(Izq, LI),
    inorder(Der, LD),
    append(LI, [V | LD], L),
    msort(L, L).

% iv.
% aBBInsertar(+X, +T1, -T2).
aBBInsertar(V, nil, bin(nil, V, nil)).
aBBInsertar(V, bin(Izq, R, Der), bin(Izq, R, DerI)) :-
    V > R,
    aBBInsertar(V, Der, DerI).
aBBInsertar(V, bin(Izq, R, Der), bin(IzqI, R, Der)) :-
    V =< R,
    aBBInsertar(V, Izq, IzqI).
/*
    El predicado es reversible para los dos árboles pero no para el valor a agregar,
    porque Prolog necesita que ambos valores de la comparación en la primer cláusula
    de los dos predicados estén instanciados.
*/



% Generate & Test:

% Ejercicio 13)
% coprimos(-X, -Y).
coprimos(X, Y) :- generate(X, Y), gcd(X,Y) =:= 1.

generate(X, Y) :- desdeReversible(0, Z), between(0, Z, X), Y is Z-X, Y>=X .
/*
    No genera conmutados, pero es importante que no estén instanciados los
    dos parámetros a la vez, pues en ese caso se puede llegar a colgar al
    pedir más soluciones o que no sean coprimos los instanciados.
*/


% Ejercicio 14)
% i. cuadradoSemiMagico(+N, -XS).
cuadradoSemiMagico(0, []).
cuadradoSemiMagico(N, L) :-
    desde(0, X),
    generarNListasDeMElemsQueSumanS(N, N, X, L).

generarNListasDeMElemsQueSumanS(0, _, _, []).
generarNListasDeMElemsQueSumanS(N, M, S, [L | Ls]) :-
    generarNElemsQueSumanS(M, S, L),
    Nm1 is N-1,
    length(Ls, Nm1),
    generarNListasDeMElemsQueSumanS(Nm1, M, S, Ls).

generarNElemsQueSumanS(0, 0, []).
generarNElemsQueSumanS(N, S, [E | Resto]) :-
    N > 0,
    S >= 0,
    between(0, S, E),
    SmE is S - E,
    Nm1 is N - 1,
    generarNElemsQueSumanS(Nm1, SmE, Resto).


% ii. cuadradoMagico(+N, -XS).
cuadradoMagico(0, []).
cuadradoMagico(N, L) :-
    desde(0, X),
    generarNListasDeMElemsQueSumanS(N, N, X, L),
    todasColumnasSuman(X, L).

todasColumnasSuman(Suma, Matriz) :-
    length(Matriz, N),
    sumasColumnas(Suma, N, Matriz).

sumasColumnas(_, 0, _).
sumasColumnas(Suma, Columna, Matriz) :-
    sumaColumna(Suma, Columna, Matriz),
    Cm1 is Columna-1,
    sumasColumnas(Suma, Cm1, Matriz).

% sumaColumna(?Columna, +Suma, +Matriz).
sumaColumna(0, _, []).
sumaColumna(Suma, Columna, [L | Ls]) :-
    Suma >= 0,
    nth1(Columna, L, N),
    SmN is Suma-N,
    sumaColumna(SmN, Columna, Ls).

% Ejercicio de la guía de resolución:
r(alan).
j(alan).
i(X) :- res(X, Y), pl(Y).
res(X, Y) :- j(X), r(X), pr(Y).
pl(X) :- pr(X).
pr(c).

% Dejado en el ejercicio 14 hecho.

% Ejercicios pendientes: 15 en adelante.





% REPASO PARA EL SEGUNDO RECU:

peano(zero).
peano(suc(X)) :- peano(X).

% peanoANativo(+P, ?X),
peanoANativo(zero, 0).
peanoANativo(suc(N), X) :- peano(N), peanoANativo(N, Y), X is Y+1.

% nativoAPeano(?P, +X).
nativoAPeano(zero, 0).
nativoAPeano(suc(M), X) :- Y is X-1, X>0, nativoAPeano(M, Y), peano(M).

% traducir(?P, ?N)
traducir(P, N) :- nonvar(N), nativoAPeano(P, N).
traducir(P, N) :- var(N), peanoANativo(P, N).

% esParPeano(?P)
esParPeano(zero).
esParPeano(suc(suc(N))) :- esParPeano(N).

% esParNativo(?N) para testear rápido esParPeano
esParNativo(N) :- traducir(P,N), esParPeano(P).

% menorPeano(?X, ?Y)
menorPeano(zero, suc(M)) :- peano(M).
menorPeano(suc(N), suc(suc(M))) :- menorPeano(N, M).

% menorNativo(?X, ?Y)
menorNativo(X, Y) :- traducir(Py, Y), menorPeano(Px, Py), traducir(Px, X).


% iesimo(+I, +L, -X)
iesimo(1, [X | _], X).
iesimo(N, [_ | T], X) :- Nm1 is N-1, iesimo(Nm1, T, X).



% Ejercicio 15)

% i.
% esTriangulo(+T)
esTriangulo(tri(A,B,C)) :- A < B+C, B < A+C, C < A+B.

% ii.
% perimetro(?T, ?P)
perimetro(tri(A,B,C), P) :- ground(tri(A,B,C)), esTriangulo(tri(A,B,C)), P is A+B+C.
perimetro(tri(A,B,C), P) :-
    not(ground(tri(A,B,C))),
    armarTriplas(P,A,B,C),
    esTriangulo(tri(A,B,C)).

armarTriplas(P,A,B,C) :-
    desdeReversible(3,P),
    between(0,P,A),
    S is P-A,
    between(0,S,B),
    C is S-B.

triangulo(T) :- perimetro(T, _).


% Ejercicio 16)
frutal(frutilla).
frutal(banana).
frutal(manzana).
cremoso(banana).
cremoso(americana).
cremoso(frutilla).
cremoso(dulceDeLeche).
leGusta(X) :- frutal(X), cremoso(X).
cucurucho(X,Y) :- leGusta(X), leGusta(Y).

% Ejercicio 17: en hoja.

% Ejercicio 18)
% corteMasParejo(+L, -L1, -L2)
corteMasParejo(L, L1, L2) :-
    % sum_list(L, Suma),
    % between(0, Suma, Diferencia),
    corteConDif(L, L1, L2, Diferencia),
    not((corteConDif(L, _, _, Mayor), Mayor<Diferencia)).

corteConDif(L, L1, L2, Dif) :-
    append(L1, L2, L),
    sum_list(L1, S1),
    sum_list(L2, S2),
    D is S1-S2,
    abs(D, Dif).


% Ejercicio 19) En hoja


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


% Ejercicio 21) conjuntoDeNaturales(X) :- not((pertenece(E, X), not(natural(E)))).


% Ejercicio 22) En hoja.


% Ejercicio 23)
% i) 
% arbol(-A)
arbol(A) :- desdeReversible(0, N), generarNodos(N, A).

generarNodos(0, nil).
generarNodos(N, bin(AI, _, AD)) :-
    N > 0,
    K is N-1,
    between(0, K, NI), 
    ND is K-NI,
    generarNodos(NI,AI),
    generarNodos(ND, AD).

% ii)
% nodosEn(?A, +L)
nodosEn(nil, _).
nodosEn(bin(AI, R, AD), L) :-
    member(R, L),
    nodosEn(AI, L),
    nodosEn(AD, L).

% iii)
% sinRepEn(-A, +L)
sinRepEn(nil, _).
sinRepEn(bin(AI, R, AD), L) :-
    append(L1, [R | L2], L),
    sinRepEn(AI, L1),
    sinRepEn(AD, L2).