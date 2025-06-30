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
    Se llena el stack de memoria.
    Seguimiento del árbol de búsqueda: 
    ?- ancestro(juan, X).
        true. {X := juan}
        ?- ancestro(Z, X), padre(juan, Z).
            ?- padre(juan, X). {Z := X}
                true. {X := carlos}
                true. {X := luis}
            ?- ancestro(A, X), padre(Z, A), padre(juan, Z).
                ?- padre(Z, X), padre(juan, Z). {A := X}
                    ?- padre(juan, juan). {Z := juan, X := carlos}
                        false.
                    ?- padre(juan, juan). {Z := juan, X := luis}
                        false.
                    ?- padre(juan, carlos). {Z := carlos, X := daniel}
                        true. {X := daniel}
                    ?- padre(juan, carlos). {Z := carlos, X := diego}
                        true. {X := diego}
                    ?- padre(juan, luis). {Z := luis, X := pablo}
                        true. {X := pablo}
                    ?- padre (juan, luis). {Z := luis, X := manuel}
                        true. {X := manuel}
                    ?- padre (juan, luis). {Z := luis, X := ramiro}
                        true. {X := ramiro}
                ?- ancestro(B, X), padre(A, B), padre(Z, A), padre(juan, Z).
                    ?- padre(A, X), padre(Z, A), padre(juan, Z). {B := X}
                        ?- padre(Z, juan), padre(juan, Z). {A := juan, X := carlos}
                            false.
                        ?- padre(Z, juan), padre(juan, Z). {A := juan, X := luis}
                            false.
                        ?- padre(Z, carlos), padre(juan, Z). {A := carlos, X := daniel}
                            ?- padre(juan, juan). {Z := juan}
                                false.
                        ?- padre(Z, carlos), padre(juan, Z). {A := carlos, X := diego}
                            ?- padre(juan, juan). {Z := juan}
                                false.
                        ?- padre(Z, luis), padre(juan, Z). {A := luis, X := pablo}
                            ?- padre(juan, juan). {Z := juan}
                                false.
                        ?- padre(Z, luis), padre(juan, Z). {A := luis, X := manuel}
                            ?- padre(juan, juan). {Z := juan}
                                false.
                        ?- padre(Z, luis), padre(juan, Z). {A := luis, X := ramiro}
                            ?- padre(juan, juan). {Z := juan}
                                false.
                    ?- ancestro(C, X), padre(B, C), padre(A, B), padre(Z, A), padre(juan, Z).
                        ... Hasta llenar la memoria.

    La justificación a nivel más alto es que la recursión se vuelve infinita si para resolver
    P(X) con X sin instanciar necesitás resolver P(Y) con Y sin instanciar. El problema que
    intentás resolver es el mismo en ambos casos, y entonces el programa se cuelga.
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
menorOIgualCorregido(X, suc(Y)) :- natural(Y), menorOIgual(X, Y).



% Operaciones sobre listas:

% Ejercicio 4)
juntar([], Ys, Ys).
juntar([X | Xs], Ys, [X | Ls]) :- juntar(Xs, Ys, Ls).


% Ejercicio 5) 
% Definir los siguientes predicados USANDO APPEND.
% i.
% last([U], U).
% last([X | Xs], U) :- last(Xs, U).
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
aplanar([X | Xs], Ys) :- not(is_list(X)), aplanar(Xs, Z), append([X], Z, Ys).
aplanar([X | Xs], Ys) :- is_list(X), aplanar(X, Y), aplanar(Xs, Z), append(Y, Z, Ys).


% Ejercicio 7)
% Definir los siguientes predicados usando MEMBER o APPEND según sea conveniente.
% i.
/* Versión de ChatGPT:

interseccion(L1, L2, Res) :-
    % Inicializamos un acumulador vacío
    interseccion_aux(L1, L2, [], Res).
% Caso base: ya recorrimos toda la primera lista
interseccion_aux([], _, Acc, Res) :-
    reverse(Acc, Res).  % invertimos para mantener el orden original
% Caso 1: X está en L2 y no está en el acumulador → lo agregamos
interseccion_aux([X|Xs], L2, Acc, Res) :-
    member(X, L2),
    not(member(X, Acc)),
    interseccion_aux(Xs, L2, [X|Acc], Res).
% Caso 2: en cualquier otro caso, lo descartamos
interseccion_aux([_|Xs], L2, Acc, Res) :-
    interseccion_aux(Xs, L2, Acc, Res).
*/
sacarApariciones(_, [], []).
sacarApariciones(X, [X | Xs], L):- sacarApariciones(X, Xs, L).
sacarApariciones(X, [Y | Xs], [Y | L]):- X \= Y, sacarApariciones(X, Xs, L).
% interseccion(+L1, +L2, -L3)
interseccion([], _, []).
interseccion([X | Xs], Ys, [X | L]):- member(X, Ys), sacarApariciones(X, Xs, M), interseccion(M, Ys, L).
interseccion([X | Xs], Ys, L):- not(member(X, Ys)), interseccion(Xs, Ys, L).
% partir(N, L, L1, L2) (Preguntar sobre cuán reversible es el predicado).
partir(_, [], [], []).
partir(0, [X | Xs], [], [X | Xs]).
partir(N, [X | Xs], [X | Xs], []) :- length([X | Xs], N). 
partir(N, [X | Xs], [X | Ls1], L2) :- N > 0, M is N-1, partir(M, Xs, Ls1, L2), append(Ls1, L2, Xs).
% ii.
% borrar(+ListaOriginal, +X, -ListaSinXs)
% borrar([], _, []).
% borrar([X|Xs], X, L) :- borrar(Xs, X, L).
% borrar([X|Xs], Y, [X|Ls]) :- X \= Y, borrar(Xs, Y, Ls).
borrar([], _, []).
borrar([E | Xs], E, Zs) :- member(E, Xs), borrar(Xs, E, Zs).
borrar([E | Xs], E, Zs) :- not(member(E, Xs)), append([], Zs, Xs).
borrar([X | Xs], E, [X | Zs]) :- X \= E, borrar(Xs, E, Zs).
% iii.
% sacarDuplicados(+L1, -L2).
sacarDuplicados([], []).
sacarDuplicados([X | L1], [X | L2]) :- borrar(L1, X, L3), sacarDuplicados(L3, L2).
% iv. Este anda mal, solo funciona cuando están los dos parámetros instanciados.
% permutacion(+L1, ?L2).
% permutacion([], []).
% permutacion([X | Xs], [X | Ls]) :- permutacion(Xs, Ls).
% permutacion([X | Xs], [Y | Ys]) :-
%     member(X, Ys),
%     member(Y, Xs),
%     X \= Y,
%     borrarUno(Ys, X, M),
%     borrarUno(Xs, Y, N),
%     permutacion(M, N).
borrarUno(_, [], []).
borrarUno(X, [X | XS], XS).
borrarUno(X, [Y | YS], [Y | L]) :- X \= Y, borrarUno(X, YS, L).

permutacion([],[]).
permutacion([X | XS], L) :- permutacion(XS, Y), borrarUno(X, L, Y), member(X, L).  
% v. reparto(+L, +N, -LListas).
% LListas debe ser una lista de N >= 1 listas de cualquier longitud tales que al concatenarlas se obtiene L.
reparto(X, 1, [X]).
reparto(L, N, [X | Ls]) :-
    N > 1,
    M is N-1,
    append(X, Xs, L),
    reparto(Xs, M, Ls).
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
    X hasta el infinito, sin detenernos, por lo que tendría que preguntar si se
    considera como colgado o no.
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
/*
    Observación: esta implementación ordena los elementos no por recorrido,
    sino por su valor en sí (había malinterpretado la consigna).
    inorder(A , Ls) :-
        listaDeAB(A, Ms),    
        msort(Ms, Ls).

    listaDeAB(nil, []).
    listaDeAB(bin(Izq, V, Der), [V | Resto]) :-
        listaDeAB(Izq, LI),
        listaDeAB(Der, LD),
        append(LI, LD, Resto).
*/
inorder(nil, []).
inorder(bin(Izq, V, Der), Ls) :-
    inorder(Izq, OI),
    inorder(Der, OD),
    append(OI, [V | OD], Ls).
% Recorrido inorder de ABs: Todo izquierda a todo derecha.

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

generate(X, Y) :- desde(0, Z), between(0, Z, X), Y is Z-X, Y>=X .
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

generarNElemsQueSumanS(0, 0, []).
generarNElemsQueSumanS(N, S, [E | Resto]) :-
    N > 0,
    S >= 0,
    between(0, S, E),
    SmE is S - E,
    Nm1 is N - 1,
    generarNElemsQueSumanS(Nm1, SmE, Resto).

generarNListasDeMElemsQueSumanS(0, _, _, []).
generarNListasDeMElemsQueSumanS(N, M, S, [L | Ls]) :-
    generarNElemsQueSumanS(M, S, L),
    Nm1 is N-1,
    length(Ls, Nm1),
    generarNListasDeMElemsQueSumanS(Nm1, M, S, Ls).

/*
todasSuman([], _).
todasSuman([L | Ls], S) :-
    sum_list(L, S),
    todasSuman(Ls, S).
*/

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

% Dejado en el ejercicio 14 hecho.

% LO SIGUIENTE ES LO DE OCTOKERBS:

%-------------------------------------------------
% esTriángulo(+T)
esTriangulo(tri(A, B, C)) :- A < B+C, B < C+A, C < A+B.

% perímetro(?T,?P)
perimetro(tri(A,B,C),P) :- ground(tri(A,B,C)), esTriangulo(tri(A,B,C)), P is A+B+C.
perimetro(tri(A,B,C),P) :- not(ground(tri(A,B,C))), armarTriplas(P,A,B,C), esTriangulo(tri(A,B,C)).

armarTriplas(P,A,B,C) :- desdeReversible(3,P), between(0,P,A), S is P-A, between(0,S,B), C is S-B.

triangulos(T) :- perimetro(T,_).
%-------------------------------------------------
frutal(frutilla).
frutal(banana).
frutal(manzana).
cremoso(banana).
cremoso(americana).
cremoso(frutilla).
cremoso(dulceDeLeche).

% leGusta(X)
leGusta(X) :- frutal(X), cremoso(X).

% cucurucho(X,Y)
cucurucho(X,Y) :- leGusta(X), leGusta(Y).

% ?- cucurucho(X,Y)
% Se unifica con las variables de leGusta(X) y leGusta(Y).
% en leGusta(X), X unifica con la variable de frutal y la de cremoso
% X de frutal es instanciado como frutilla.
% X de cremoso es resolvente con cremoso(frutilla).
% X de leGusta se instancia como frutilla.
% X de cucurucho se instancia con frutilla,
% se puede instanciar lo mismo con Y.
% Se empieza a hacer backracking con respecto a Y, dejamos de instanciar la Y como frutilla e instaanciamos un nuevo
% frutal, el sigueinte es banana. Despues de banana , como es resolvente con cremoso(banana), se instancia en cucurucho.
% como no existe otro frutal que sea cremoso, se hace backtracking de y se prueba una nueva instanciacion de X.

% 2. 

% cucurucho(X,Y) :- leGusta(X), !, leGusta(Y).
% Si leGusta(X) encuentra alguna instancia para X entonces se sigue para adelante y no se buscan mas instancias.
% es como una pared que separa la instanciacion actual de mas instanciaciones.

% cucurucho(X,Y) :- leGusta(X), leGusta(Y), !.
% Si leGusta(X) encuentra alguna instancia para X y leGusta(Y) encuentra una para Y entonces listo, terminamos.

% leGusta(X) :- frutal(X), !, cremoso(X).
% despues de un frutal solo vemos si es cremoso.

% leGusta(X) :- frutal(X), cremoso(X), !.
% despues de un frutal y cremoso terminamos, solo queremos uno.

%% Literalmente copie lo que hice cuando rendi la materia, ni ganas de pensar en este ejercicio repetitivo.
%-------------------------------------------------
% P(?X) 

% Q(?X)

% ?- P(Y), not(Q(Y))

% 1. Significa que queremos un Y tal que P la instancie pero Q no pueda instanciarla.

% 2. Si se invierte el orden prolog va a buscar la NO instanciacion de Y con respecto a Q. Esto solo es verdadero si no existe base de conocimientos de Q. Pero 
% si la base de conocimiento no existe entonces prolog falla porque no se pudo instanciar a Y. EL NOT NO SIRVE PARA INSTANCIAR.

% 3. P(Y), not((P(X), X \= Y))
%-------------------------------------------------
% corteMásParejo(+L,-L1,-L2)
corteMásParejo(L, L1, L2) :- corte(L, L1, L2, D1), not((corte(L, _, _, D2), D2 < D1)).

corte(L, L1, L2, D) :- append(L1, L2, L), sum_list(L1, S1), sum_list(L2, S2), D is abs(S1-S2).
%-------------------------------------------------
p(X) :- mod(X,2) =:= 0.

minimoNatural(X) :- desdeReversible(1, X), p(X), not((between(1,X,Y), p(Y), Y < X)),!.
%-------------------------------------------------
% divisores(N1, N2, L)
divisores(X, N2, [X | XS]) :- X =< N2, mod(N2,X) =:= 0, N3 is X + 1, divisores(N3, N2, XS).
divisores(N1, N2, XS) :- N1 =< N2, mod(N2,N1) =\= 0, N3 is N1 + 1, divisores(N3, N2, XS).
divisores(N, N, [N]).

% esPrimo(N).
esPrimo(N) :- divisores(1, N, L), length(L, X), X =:= 2, !. % Los numeros primos solo tienen 2 divisores. 1 y si mismos.

% próximoNumPoderoso(+X,-)
proximoNumPoderoso(X,M) :- X2 is X + 1, desdeReversible(X2, M), divisores(1, M, D), not((member(P, D), esPrimo(P), not((0 is mod(M, P), 0 is mod(M, P*P))))), !.

% Para algun M mayor a X existe una lista de divisores D tal que ninguno de sus primos no divida a M y su cuadrado tampoco.
%-------------------------------------------------
% natural(N).
natural(cero).
natural(suc(X)) :- natural(X).

% pertenece(?Elemento, +Conjunto)
pertenece(X, [X | _]).
pertenece(Y, [X | XS]) :- Y =\= X, pertenece(Y, XS).

% conjuntoDeNaturales(X)        
conjuntoDeNaturales(X) :- not( (pertenece(N,X), not( natural(N) )) ).

% No uses numeros. Ejemplo: conjuntoDeNaturales([suc(cero),suc(suc(cero)),suc(suc(suc(cero)))]).
%-------------------------------------------------
:- ensure_loaded('Ejercicio8.pl').      % Importamos el predicado 'borrar' para eliminar los nodos que ya se usaron en el grafo.

% Con getGrafo(conexo, G) obtenemos instanciado en G el grafo. Sirve para testear las consultas. 
% Ejemplo. La consulta 'getGrafo(conexo, G), esNodo(G, X)' instancia en G el grafo de abajo e instancia en X cada nodo.
getGrafo(conexo, grafo([1, 2, 3, 4, 5],[arista(1, 2), arista(2, 3), arista(2, 4), arista(3, 5),arista(4, 5)])).
getGrafo(disconexo, grafo([1, 2, 3, 4, 5, 6, 7],[arista(1, 2), arista(2, 3), arista(2, 4), arista(3, 5),arista(4, 5), arista(6,7)])).
getGrafo(estrella, grafo([1, 2, 3, 4, 5, 6, 7],[arista(1, 2), arista(1, 3), arista(1, 4), arista(1, 5),arista(1, 6), arista(1,7)])).

% --- Ejercicios ---

% nodos
nodosDe(grafo(V, _), V).

% aristas
aristasDe(grafo(_,E), E).

% esNodo(+G,?X)
esNodo(G, X) :- ground(G), nodosDe(G, V), member(X, V).

% esArista(+G,?X,?Y)
esArista(G, X, Y) :- ground(G), nonvar(X), nonvar(Y), aristasDe(G, E), member(arista(X,Y), E).    % Caso arista instanciada.
esArista(G, X, Y) :- ground(G), nonvar(X), nonvar(Y), aristasDe(G, E), member(arista(Y,X), E).    % Caso arista instanciada invertida.
esArista(G, X, Y) :- ground(G), nonvar(X), var(Y), aristasDe(G, E), member(arista(X,Y), E).       % Caso solo X instanciado.
esArista(G, X, Y) :- ground(G), nonvar(X), var(Y), aristasDe(G, E), member(arista(Y,X), E).       % Caso solo X instanciado invertido.
esArista(G, X, Y) :- ground(G), var(X), nonvar(Y), aristasDe(G, E), member(arista(X,Y), E).       % Caso solo Y instanciado.
esArista(G, X, Y) :- ground(G), var(X), nonvar(Y), aristasDe(G, E), member(arista(Y,X), E).       % Caso solo Y instanciado invertido.
esArista(G, X, Y) :- ground(G), var(X), var(Y), aristasDe(G, E), member(arista(X,Y), E).          % Caso ningún nodo instanciado. Se instancian todas las aristas.

% caminoSimple(+G,+D,+H,?L)
caminoSimple(_, H, H, [H]).
caminoSimple(G, D, H, [D | L]) :- esArista(G, D, N), esNodo(G, N), nodosDe(G, V), borrar(V, D, V2), aristasDe(G, E), caminoSimple(grafo(V2, E), N, H, L).

% caminoHamiltoniano(+G,?L)
caminoHamiltoniano(G, L) :- caminoSimple(G, _, _, L), length(L, N1), nodosDe(G, V), length(V, N2), N1 =:= N2. 

% esConexo(+G)
esConexo(G) :- not((esNodo(G, X), esNodo(G, Y), X \= Y, not((caminoSimple(G, X, Y, _))))). % No existen dos nodos tal que no exista camino simple entre ambos.

% esEstrella(+G)
esEstrella(G) :- esConexo(G), esNodo(G, X), not((esNodo(G, Y), X \= Y, not((esArista(G,X,Y))))). % Existe un nodo tal que no existe otro nodo que no sea arista de este.
%-------------------------------------------------
% generarNodos(+N, -A)
generarNodos(0, nil).
generarNodos(N, bin(AI, _, AD)) :- N > 0, K is N-1, between(0, K, NI), ND is K-NI, generarNodos(NI,AI), generarNodos(ND, AD). 

% arbol(-A)
arbol(A) :- desde(0, X), generarNodos(X, A).

% nodosEn(?A, +L)
nodosEn(nil, _).
nodosEn(bin(AI, R, AD), L) :- member(R, L), nodosEn(AI, L), nodosEn(AD, L).

% sinRepEn(-A, +L)
sinRepEn(nil, _).
sinRepEn(bin(AI, R, AD), L) :- append(L1, [R | L2], L), sinRepEn(AI, L1), sinRepEn(AD, L2).