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