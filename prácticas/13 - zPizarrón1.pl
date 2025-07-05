% rama(+A, -R).
rama(bin(nil, R, nil), [R]).
rama(bin(I,R,_), [R | XS]) :- rama(I, XS).
rama(bin(_,R,D), [R | XS]) :- rama(D, XS).


arbol(bin(bin(bin(nil,4,nil),2,bin(nil,5,bin(nil,6,nil))),1,bin(bin(nil,7,nil),3,nil))).
arbol2(bin(nil, 2, nil)).

arbol3(bin(bin(nil, 2, nil),1,bin(nil, 2, nil))).


% rama(+A, +R)


% Si R unifica con [] la consulta va a fallar pues [] no unifica con ninguna regla.

% Si R es de longitud 1 entonces va a ser una rama de A sii A es un árbol-hoja, es decir si A unifica con bin(nil, Raiz, nil).

% Por lo tanto la consulta rama(A, R) unifica con la primer regla y por ende la consulta va a ser verdadera si el único elemento de R unifica con la raiz de A.

% La consulta va a fallar al unificar con la segunda o tercera regla pues estas solo tendrían éxito si rama(A', []) tuviera éxito, lo cual ya vimos que no es posible.

% Dado que la primera regla es la única candidata a tener éxito,y además es un hecho, entonces una vez que llega a la clausula vacía, el programa termina.



% Si R es una lista de longitud mayor a 1:
%   Si el primer elemento de R no unifica con la raiz del árbol entonces no va a unificar con ninguna regla y la consulta va a fallar. Lo cual es deseado pues todas las ramas inician en la raíz del árbol.

%   Si el primer elemento de R unifica con la raíz del árbol (dado que |R| > 1) se puede unificar únicamente con las dos últimas reglas.
%   En ambas reglas se va a tener éxito únicamente si la cola de R es una rama del subarbol izquierdo y derecho respectivamente. Lo cual funciona inductivamente.




%ramaUnicaDeLong(+A,+N,-C)
ramaUnicaDeLong(A, N, C) :- length(C, N), rama(A, C), 
                              not(hayOtraRamaDeLong(A,N,C)).

%hayOtraRamaDeLong(+A,+N,+C) 
hayOtraRamaDeLong(A,N,C) :- length(C2, N), rama(A, C2), C2 \= C.



