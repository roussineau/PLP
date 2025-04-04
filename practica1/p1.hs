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



