-- Clase 01/04

module Apuntes1 where

data AEB a = Hoja a | Bin (AEB a) a (AEB a) deriving Show

miArbol = Bin (Hoja 2) 1 (Hoja 3)

foldAEB :: (a -> b) -> (b -> a -> b -> b) -> AEB a -> b
foldAEB fHoja fBin (Hoja a) = fHoja a
foldAEB fHoja fBin (Bin izq valor der) = 
    fBin (foldAEB fHoja fBin izq) valor (foldAEB fHoja fBin der)

-- fHoja = funcion que le aplicamos a cada Hoja a
-- fBin = funcion que le aplicamos a cada arbol binario AEB a

foldAEB' :: (a -> b) -> (b -> a -> b -> b) -> AEB a -> b
foldAEB' fHoja fBin aeb = 
    case aeb of
        Hoja a -> fHoja a
        Bin i r d ->
            fBin (rec i) r (rec d)
    where rec = foldAEB' fHoja fBin

cantHojasExpl :: AEB a -> Int
cantHojasExpl (Hoja a) = 1
cantHojasExpl (Bin i a d) = cantHojasExpl i + cantHojasExpl d

espejo :: AEB a -> AEB a
espejo = foldAEB Hoja (\recI r recD -> Bin recD r recI)

-- >>> espejo miArbol
-- Bin (Hoja 3) 1 (Bin (Hoja 3) 1 (Hoja 2))


-- Este problema se puede resolver con recursion estructural porque lo unico que necesito es la raiz y las subramas
ramas :: AEB a -> [[a]]
ramas = foldAEB (\a -> [[a]]) (\recI rama recD -> map (rama:) recI ++ map (rama:) recD )

-- >>> ramas miArbol
-- [[1,2],[1,3]]

-- implementar altura

{-
    La diferencia entre tipos abstractos y tipos algebraicos es que con los algebraicos podemos hacer
    lo que queramos desde otro contexto, lo cual no siempre es un comportamiento deseado
    Para evitar eso tenemos la posibilidad de exportar selectivamente los tipos, constructores y funciones
    que queramos

    module AB (AB(..), vacio, insertarABB) where

    de esta forma podriamos por ejemplo exportar solo la funcion insertarABB, y que solo se pueda construir 
    de forma ordenada para que siempre sean ABB 
-}

data Polinomio a =
    X
    | Cte a
    | Suma (Polinomio a) (Polinomio a)
    | Prod (Polinomio a) (Polinomio a)
    -- deriving instance (Show a) => Show (Polinomio a)

poli1 = Suma (Cte 1) (Prod X X) -- 1 + x^2

foldPoli :: b -> (a->b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPoli fX fCte fSuma fProd poli =
    case poli of 
        X -> fX
        Cte a -> fCte a
        Suma i d -> fSuma (rec i) (rec d)
        Prod i d -> fProd (rec i) (rec d)
    where
        rec = foldPoli fX fCte fSuma fProd 

evaluar :: Num a => a -> Polinomio a -> a
evaluar x = foldPoli x id (+) (*)

-- >>> evaluar poli1 2
-- esto se lee como 

evaluar' :: Num a => Polinomio a -> a -> a
evaluar' = foldPoli id const (\i d -> \x -> i x + d x) (\i d -> \x -> i x * d x)


data RoseTree a = Rose a [RoseTree a]

foldRT :: (a -> [b] -> b) -> RoseTree a -> b
foldRT fRose (Rose a hijos) = fRose a (map rec hijos)
    where
        rec = foldRT fRose

altura :: RoseTree a -> Int
altura = foldRT (\r alturaHijos ->
    case alturaHijos of 
        [] -> 1
        _ -> maximum (map (+1) alturaHijos))


-- hacer la funcion ramas para rosetrees

-- ENTENDIENDO FOLDR:

-- foldrPrueba :: (a->b->b) -> b -> [a] -> b
-- foldrPrueba f casoBase [] = casoBase
-- foldrPrueba f casoBase (x:xs) = f x (foldrPrueba f casoBase xs)

-- mapPrueba :: (a->b) -> [a] -> [b]
-- mapPrueba f [] = []
-- mapPrueba f (x:xs) = f x : mapPrueba f xs

-- mapFoldr :: (a->b) -> [a] -> [b]
-- mapFoldr f xs = foldr (\x rec -> f x : rec) [] xs

reverso :: [a] -> [a]
reverso = foldr (\x rec -> rec ++ [x]) []
-- La funcion lambda que recibe toma un parámetro y el acumulador; cuando llegue al caso base va a empezar a concatenar desde []

