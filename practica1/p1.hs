{-
    Currificación y tipos

    Ejercicio 1: considerar las siguientes definiciones de funciones.

    i) ¿Cuál es el tipo de cada función? (suponer que todos los números son Float)
    ii) Indicar cuáles de las funciones anteriores no están currificadas. Para cada una de ellas, definir la función currificada correspondiente. Recordar dar el tipo de la función.
-}

max2 :: (Float, Float) -> Float
max2 (x, y)
    | x >= y = x
    | otherwise = y
-- No está currificada. Currificada:
max2currified :: Float -> Float -> Float
max2currified = max


normalVectorial :: (Float, Float) -> Float
normalVectorial (x, y) = sqrt (x^2 + y^2)
-- No está currificada. Currificada:
normalVectorialCurrified :: Float -> Float -> Float
normalVectorialCurrified x y = sqrt (x^2 + y^2)


subtract' :: Float -> Float -> Float
subtract' = flip (-)
{-
    flip :: (a -> b -> c) -> b -> a -> c
    flip f x y = f y x

    flip no anida tipos, solamente toma una función de dos parámetros y devuelve otra función de dos parámetros

    subtract 2 5 = flip (-) 2 5
                 = (-) 5 2
                 = 3
-}


predecesor :: Float -> Float
predecesor = subtract' 1
{-
    subtract' toma un valor y devuelve una función que toma otro valor
    En este caso, el tipo es subtract 1 :: Float -> Float porque ya toma ese primer valor (1).
-}


evaluarEnCero :: (Float -> t) -> t
evaluarEnCero f = f 0
{-
    Sí está currificada, porque seguimos teniendo una secuencia de
    funciones unarias. Recordemos que las expresiones son secuencias
    de símbolos que representan datos, funciones y funciones aplicadas
    a datos. PERO COMO LAS FUNCIONES TAMBIÉN SON DATOS COMO CUALQUIER
    OTRO, AL FINAL SEGUIMOS TENIENDO UNA FUNCIÓN UNARIA.
-}

dosVeces :: (a -> a) -> a -> a
dosVeces f = f . f
{-
    La definición de (.) es: (.) f g x = f (g x). Luego:

    dosVeces (*2) 1 = (*2) . (*2) 1
                    = (*2) ((*2) 1)
                    = (*2) 2
                    = 4
-}

{-
    Ejercicio de TDA, implementación de pseudocódigo de la guía de Backtracking punto 1
    Toma un multiconjunto, su longitud-1, y el número que queremos sumar entre los elementos.
    Devuelve si existe o no esa suma
-}
subsetSum :: [Int] -> Int -> Int -> Bool
subsetSum _ 0 j = j == 0
subsetSum cs i j = subsetSum cs (i-1) j || subsetSum cs (i-1) (j - cs !! i)

{-
    Ejercicio 2:
    i) Definir la función curry, que dada una función de dos argumentos, define su equivalente currificada
    ii) Definir la función uncurry, que dada una función currificada de dos argumentos, devuelve ibidem
    iii) ¿Se podría definir una función curryN, que tome una función de un número n de argumentos y devuelva su versión currificada?
        Sugerencia: pensar cuál sería el tipo de la función

        A priori yo contestaría que no es posible, no al menos con las condiciones planteadas por la consigna.
        Para empezar, al tratar de pensar su tipo nos encontramos con que necesitamos saber cuántos parámetros
        tiene la función. Quizás sí se podría pasando la cantidad de parámetros de la función como parámetro,
        además de la función. O sea, pasando dos parámetros.
-}

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f =  (\a b -> f (a,b)) -- La escribo como función lambda porque así se entiende mejor

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f = (\(a,b) -> f a b)


data Nat = Zero | Succ Nat deriving Show

sumar :: Nat -> Nat -> Nat
sumar Zero n     = n
sumar (Succ n) m = Succ (sumar n m)


{-
    Ejercicio 3:
    i) Redefinir usando foldr la función sum, elem, (++), filter y map
    ii) Definir la función mejorSegún :: (a -> a -> Bool) -> [a] -> a, que devuelve el máximo elemento
        de la lista según una función de comparación, utilizando foldr1. Por ejemplo, maximum = mejorSegún(>).
    iii) Definir la función sumasParciales :: Num a => [a] -> [a], que dada una lista de números devuelve
         otra de la misma longitud, que tiene en cada posición la suma parcial de los elementos de la lista original
         desde la cabeza hasta la posición actual. Por ejemplo, sumasParciales [1,4,-1,0,5] ; [1,5,4,4,9].
    iv) Definir la función sumaAlt, que realiza la suma alternada de los elementos de una lista. Es decir, da como
        resultado: el primer elemento, menos el segundo, más el tercero, menos el cuarto, etc. Usar foldr.
    v) Hacer lo mismo que en el punto anterior, pero en sentido inverso (el último elemento menos el anteúltimo,
       etc.). Pensar qué esquema de recursión conviene usar en este caso.
-}

sum' :: [Int] -> Int
sum' = foldr (+) 0

elem' :: Eq a => a -> [a] -> Bool
elem' x xs = foldr (\y rec -> y == x || rec) False xs

unir' :: [a] -> [a] -> [a]
unir' = flip (foldr (\x rec -> x : rec)) -- el flip porque queda facha

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x rec-> if f x then x:rec else rec) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x -> (:) (f x)) []

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x rec -> if f x rec then x else rec)

maximo :: Ord a => [a] -> a
maximo = mejorSegun (>)

sumasParciales :: Num a => [a] -> [a]
sumasParciales [] = []
sumasParciales xs = sumasParciales(init xs) ++ [sum xs]

sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0
{-
    Recordatorio:
    foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr f cb [] = cb
    foldr f cb (x:xs) = f x (foldr f cb xs)

    Ejemplo en este ejercicio:
    sumaAlt [1,2,3]
    foldr (-) 0 [1,2,3]
    (-) 1 (foldr (-) 0 [2,3])
    (-) 1 ((-) 2 (foldr (-) 0 [3]))
    (-) 1 ((-) 2 ((-) 3 (foldr (-) 0 [])))
    (-) 1 ((-) 2 ((-) 3 0))
    (-) 1 ((-) 2 3)
    (-) 1 (-1)
    2
    O sea, resta en las posiciones pares y suma en las posiciones impares
-}

sumaAltInverso :: Num a => [a] -> a
sumaAltInverso = foldl (flip (-)) 0

{-
    Recordatorio:
    foldl :: (b -> a -> b) -> b -> [a] -> b
    foldl f ac [] = ac
    foldl f ac (x:xs) = foldl f (f ac x) xs

    Ejemplo en este ejercicio:
    sumaAltInverso [1,2,3,4]
    foldl (flip (-)) 0 [1,2,3,4]
    foldl (flip (-)) (flip (-) 0 1) [2,3,4]
    foldl (flip (-)) (flip (-) (flip (-) 0 1) 2) [3,4]
    foldl (flip (-)) (flip (-) (flip (-) (flip (-) 0 1) 2) 3) [4]
    foldl (flip (-)) (flip (-) (flip (-) (flip (-) (flip (-) 0 1) 2) 3) 4) []
    flip (-) (flip (-) (flip (-) (flip (-) 0 1) 2) 3) 4
    (-) 4 (flip (-) (flip (-) (flip (-) 0 1) 2) 3)
    (-) 4 ((-) 3 (flip (-) (flip (-) 0 1) 2))
    (-) 4 ((-) 3 ((-) 2 (flip (-) 0 1)))
    (-) 4 ((-) 3 ((-) 2 ((-) 1 0)))
    (-) 4 ((-) 3 ((-) 2 1))
    (-) 4 ((-) 3 1)
    (-) 4 2
    2
-}

{-
    Ejercicio 5:
    Considerar las siguientes funciones:
-}
elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs 
                                        then [x]
                                    else
                                        x:elementosEnPosicionesPares (tail xs)

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
                                then x:entrelazar xs []
                           else
                                x:head ys:entrelazar xs (tail ys)
{-
    Indicar si la recursión utilizada en cada una de ellas es o no estructural.
    Si lo es, reescribirla utilizando foldr. En caso contrario, explicar el motivo.
-}

elementosEnPosicionesPares' :: [a] -> [a]
elementosEnPosicionesPares' = foldr (\(i, x) rec -> if even i then x:rec else rec) [] . zip [0..]

entrelazar' :: [a] -> [a] -> [a]
entrelazar' = foldr (\x rec ys -> if null ys then x:rec ys else x:head ys:rec (tail ys)) id

{-
    Para simplificar la notación, sea:
    step = \x rec ys -> if null ys then x:rec ys else x:head ys:rec (tail ys)
    
    Quedándonos:
    entrelazar' = foldr step id

    Ejemplo de ejecución:

    entrelazar' [1,3,5] [2,4,6]
    foldr step id [1,3,5] [2,4,6]
    step 1 (step 3 (step 5 id)) [2,4,6]
-- x = 1, rec = (step 3 (step 5 id)), ys = [2,4,6]
-- Como [2,4,6] /= null:
    1:2:(step 3 (step 5 id)) [4,6]
-- x = 3, rec = (step 5 id), ys = 4,6
-- Como [4,6] /= null:
    1:2:3:4:(step 5 id) [6]
-- x = 5, rec = id, ys = [6]
-- Como [6] /= null
    1:2:3:4:5:6:id []
-- id [] = []
    1:2:3:4:5:6:[]
    [1,2,3,4,5,6]

    Otro ejemplo:
    
    entrelazar' [1,3,4] [2]
    foldr step id [1,3,4] [2]
    step 1 (step 3 (step 4 id)) [2]
-- x = 1, rec = (step 3 (step 4 id)), ys = [2]
    1:2:(step 3 (step 4 id)) []
-- x = 3, rec = (step 4 id), ys = []

-}



{-
    Ejercicio 6:
    El siguiente esquema captura la recursión primitiva sobre listas:
-}
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x:xs) = f x xs (recr f z xs)
{-
    a. Definir la función sacarUna :: Eq a => a -> [a] -> [a], que dados un elemento
       y una lista devuelve el resultado de eliminar la primera aparición del elemento
    b. Explicar por qué el esquema de recursión estructural (foldr) no es adecuado para
       implementar la función sacarUna del punto anterior.
    c. Definir la función insertarOrdenado :: Ord => a -> [a] -> [a] que inserta un elemento
       en una lista ordenada (de manera creciente), de manera que se preserva el ordenamiento.
-}
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna x = recr (\y ys rec -> if x == y then ys else y:rec) []
{-
    Seguimiento de un ejemplo. Sea:

    sacarUna 3 [1,2,3,4]
-- Sea step = (\y ys rec -> if 3 == y then ys else y:rec)
    recr step [] [1,2,3,4]
-- x = 1, xs = [2,3,4], z = []
    step 1 [2,3,4] (recr step [] [2,3,4])
-- step recibe 3 parametros, todavia no puede resolverse
    step 1 [2,3,4] (step 2 [3,4] (recr step [] [3,4]))
    step 1 [2,3,4] (step 2 [3,4] (step 3 [4] (recr step [] [4])))
-- como 3 == 3, corta la recursión del recr porque ya puede devolver el caso
    step 1 [2,3,4] (step 2 [3,4] [4])
    step 1 [2,3,4] 2:[4]
    step 1 [2,3,4] [2,4]
    1:[2,4]
    [1,2,4]
-}
-- Recursión explícita:
-- sacarUna _ [] = []
-- sacarUna x (y:ys) = if x == y then ys else y:sacarUna x ys
{-
    Usar foldr en este caso no es posible, porque no nos permite acceder a la cola
    por fuera del caso recursivo, y necesitamos devolverlo en el caso de encontrar
    el primer elemento coincidente para de esa forma sacar solo el primero y no todos.
-}
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado x = recr (step x) [x]
    where step x y ys rec 
            | y>=x = x:y:ys
            | otherwise = y:rec
{-
    recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
    recr _ z [] = z
    recr f z (x:xs) = f x xs (recr f z xs)
>>> insertarOrdenado 7 []
[7]

>>> insertarOrdenado 7 [2]
[2,7]

>>> insertarOrdenado 7 [2,11]
[2,7,11]
-}
