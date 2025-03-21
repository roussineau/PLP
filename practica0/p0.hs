import Data.List (intersect)
{-
    Ejercicio 1: Dar el tipo y describir el comportamiento de las siguientes funciones del módulo Prelude de Haskell:

    null :: [a] -> Bool
    Devuelve True solamente si la lista está vacía

    head :: [a] -> a
    Devuelve el primer elemento de la lista. Da error si está vacía

    tail :: [a] -> [a]
    Devuelve una lista igual pero sin su primer elemento. Da error si está vacía

    init :: [a] -> [a]
    Devuelve una lista igual pero sin su último elemento. Da error si está vacía

    last :: [a] -> a
    Devuelve el último elemento de la lista. Da error si está vacía

    take :: Int -> [a] -> [a]
    Devuelve los primeros n elementos de la lista. Si n > longitud de la lista, devuelve la lista completa

    drop :: Int -> [a] -> [a]
    Elimina los primeros n elementos de la lista. Si n > longitud de la lista, devuelve la lista vacía

    (++) :: [a] -> [a] -> [a]
    Concatena dos listas

    concat :: [[a]] -> [a]
    Concatena una lista de listas en una única lista

    reverse :: [a] -> [a]
    Invierte el orden de los elementos de una lista

    elem :: a -> [a] -> Bool
    Devuelve True si el elemento está en la lista



    Las siguientes funciones son de la sección Útil > Apuntes y cosas útiles para programar > Haskell


    ** FUNCIONES DE PLEGADO: **
    foldr :: (a -> b -> b) -> b -> [a] -> b
    Aplica una función desde la derecha hacia la izquierda en una lista, con un valor inicial
    Ejemplo:
    foldr (+) 0 [1, 2, 3]
    -- 1 + (2 + (3 + 0)) = 6

    foldl :: (b -> a -> b) -> b -> [a] -> b
    Aplica una función desde la izquierda hacia la derecha en una lista, con un valor inicial
    Ejemplo:
    foldl (+) 0 [1, 2, 3]
    -- ((0 + 1) + 2) + 3 = 6

    foldr1 :: (a -> a -> a) -> [a] -> a
    Igual que foldr, pero no requiere un valor inicial
    Ejemplo:
    foldr1 (+) [1, 2, 3]
    -- 1 + (2 + 3) = 6

    foldl1 :: (a -> a -> a) -> [a] -> a
    Igual que foldl, pero no requiere un valor inicial
    Ejemplo:
    foldl1 (+) [1, 2, 3]
    -- ((1 + 2) + 3) = 6


    ** FUNCIONES DE TRANSFORMACIÓN Y FILTRADO: **
    map :: (a -> b) -> [a] -> [b]
    Aplica una función a cada elemento de la lista
    Ejemplo:
    map (*2) [1, 2, 3]
    -- [2, 4, 6]

    zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    Combina dos listas aplicando una función elemento a elemento
    Ejemplo:
    zipWith (+) [1, 2, 3] [4, 5, 6]
    -- [5, 7, 9]

    filter :: (a -> Bool) -> [a] -> [a]
    Devuelve solo los elementos que cumplen una condición
    Ejemplo:
    filter even [1, 2, 3, 4]
    -- [2, 4]


    ** FUNCIONES DE LISTAS Y BÚSQUEDA: **
    all :: (a -> Bool) -> [a] -> Bool
    Devuelve True sólo si todos los elementos cumplen una condición
    Ejemplo:
    all even [2, 4, 6]
    -- True

    any :: (a -> Bool) -> [a] -> Bool
    Devuelve True si al menos un elemento cumple la condición
    Ejemplo:
    any even [1, 3, 5, 6]
    -- True

    find :: (a -> Bool) -> [a] -> Maybe a
    Devuelve el primer elemento que cumple la condición, si es que existe
    Ejemplo:
    find even [1, 3, 4, 6]
    -- Just 4


    ** FUNCIONES SOBRE MAYBE: **
    isNothing :: Maybe a -> Bool
    Devuelve True si es Nothing
    Ejemplo:
    isNothing Nothing
    -- True

    isJust :: Maybe a -> Bool
    Devuelve True si es Just
    Ejemplo:
    isJust (Just 5)
    -- True

    fromJust :: Maybe a -> a
    Extrae el valor de un Just, pero falla con Nothing
    Ejemplo:
    fromJust (Just 5)
    -- 5


    ** FUNCIONES SOBRE LISTAS (ORDEN Y UNIÓN): **
    sort :: Ord a => [a] -> [a]
    Ordena una lista en orden ascendente
    Ejemplo:
    sort [3, 1, 4, 1]
    -- [1, 1, 3, 4]

    sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    Ordena una lista con una función personalizada
    Ejemplo:
    sortBy (flip compare) [1, 2, 3]
    -- [3, 2, 1] (Orden descendente)

    union :: Eq a => [a] -> [a] -> [a]
    Une dos listas sin duplicados
    Ejemplo:
    [1, 2, 3] `union` [3, 4, 5]
    -- [1, 2, 3, 4, 5]

    intersect :: Eq a => [a] -> [a] -> [a]
    Devuelve los elementos en común entre dos listas
    Ejemplo:
    [1, 2, 3] `intersect` [3, 4, 5]
    -- [3]

    
    ** OPERADORES Y COMPARACIÓN: ** 
    (==) :: Eq a => a -> a -> Bool
    (/=) :: Eq a => a -> a -> Bool
    Comparan si dos valores son iguales o distintos
    Ejemplos:
    5 == 5 -- True
    5 /= 3 -- True

    compare :: Ord a => a -> a -> Ordering
    Devuelve LT, EQ o GT según la comparación
    Ejemplos:
    compare 3 5 -- LT
    compare 5 5 -- EQ
    compare 7 2 -- GT

    comparing :: Ord b => (a -> b) -> a -> a -> Ordering
    Usado con sortBy para ordenar por un criterio específico
    Ejemplo:
    sortBy (comparing length) ["hola", "Haskell", "es"] -- ["es","hola","Haskell"]
-}

-- Ejercicio 2: Definir las siguientes funciones

-- a) valorAbsoluto :: Float -> Float
-- Dado un número, devuelve su valor absoluto
valorAbsoluto :: Float -> Float
valorAbsoluto x = sqrt (x*x)

-- b) bisiesto :: Int -> Bool 
-- Dado un número que representa un año, indica si el mismo es bisiesto o no
bisiesto :: Int -> Bool
bisiesto x | mod x 400 == 0 = True
           | mod x 4 == 0 = mod x 100 /= 0
           | otherwise = False

-- c) factorial :: Int -> Int
-- Dado un número natural, computa su factorial
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

-- d) cantDivisoresPrimos :: Int -> Int
-- Dado un número natural, devuelve su cantidad de divisores primos
cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos x = cantDivisoresPrimosAux x 2

cantDivisoresPrimosAux :: Int -> Int -> Int
cantDivisoresPrimosAux x p
    | esPrimo x = 1
    | p <= cota && mod x p == 0 = 1 + cantDivisoresPrimosAux (quitarFactor x p) (p+1)
    | p <= cota && mod x p /= 0 = cantDivisoresPrimosAux x (p+1)
    | otherwise = 0
    where cota = floor (sqrt (fromIntegral x))

quitarFactor :: Int -> Int -> Int
quitarFactor x p
    | mod x p == 0 = quitarFactor (div x p) p
    | otherwise = x

esPrimo :: Int -> Bool
esPrimo x = encontrarDivisor x 2 == x && x /= 1

encontrarDivisor :: Int -> Int -> Int
encontrarDivisor n k
    | mod n k == 0 = k
    | k + 1 > n = n
    | otherwise = encontrarDivisor n (k+1)


{-
    Ejercicio 3: Contamos con los tipos Maybe y Either definidos como sigue: 

    data Maybe a = Nothing | Just a
    data Either a b = Left a | Right b

    a) Definir la función inverso :: Float -> Maybe Float que dado un número,
    devuelve su inverso multiplicativo si está definido, o Nothing en caso contrario

    b) Definir la función aEntero :: Either Int Bool -> Int que convierte a un entero
    en una expresión que puede ser booleana o entera. En el caso de los booleanos,
    el entero que corresponde es 0 para False y 1 para True.
-}

inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1 / x)

aEntero :: Either Int Bool -> Int
aEntero (Left x) = x
aEntero (Right bool) = if bool then 1 else 0



-- Ejercicio 4: Definir las siguientes funciones sobre listas:

-- a) limpiar :: String -> String -> String
-- Elimina todas las apariciones de cualquier caracter de la primera cadena en la segunda
limpiar :: String -> String -> String
limpiar [] second = second
limpiar _ [] = []
limpiar first (s:ss)
    | s `elem` first = limpiar first ss
    | otherwise = s : limpiar first ss

-- b) difPromedio :: [Float] -> [Float]
-- Dada una lista de números, devuelve la diferencia de cada uno con el promedio general
difPromedio :: [Float] -> [Float]
difPromedio xs = map (subtract promedio) xs
  where
    promedio = sum xs / fromIntegral (length xs)

-- c) todosIguales :: [Int] -> Bool
-- Indica si una lista de enteros tiene todos sus elementos iguales
todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales (n:ns) 
    | null ns = True
    | otherwise = n == head ns && todosIguales ns



{-
    Ejercicio 5: Dado el siguiente modelo para árboles binarios:

    data AB a = Nil | Bin (AB a) a (AB a)

    definir las siguientes funciones:

    a) vacioAB :: AB a -> Bool
    Indica si un árbol es vacío (i. e. no tiene nodos)

    b) negacion :: AB Bool -> AB Bool
    Dado un árbol de booleanos, construye otro formado por la negación

    c) productoAB :: AB Int -> Int
    Calcula el producto de todos los nodos del árbol
-}



