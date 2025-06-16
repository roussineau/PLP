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
    append(X, Xs, L), % Esta es la línea clave que no ví. Confiar en la lógica para asumir que Xs es el resto del problema.
    reparto(Xs, M, Ls).
% vi.
% repartoSinVacias(+L, -LListas).
