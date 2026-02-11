import Data.Maybe


{-
    Final 12/12/25

    Sean los siguientes tipos:

    data Indice = I1 | I2 | I3
    data AT a = Hoja a | Nodo (Indice -> AT a)
    
    En donde AT representa el tipo de dato "Árbol Trébol":
    
    a) Dar el tipo e implementación de foldAT
    b) Definir la función altura :: AT a -> Int
    c) Dar el tipo el implementar mapAT que, dada una función, se la aplica a todas las hojas de un árbol trébol
    
    Obs.: Todo Nodo siempre tiene 3 árboles trébol asociados
-}

-- a)
data Indice = I1 | I2 | I3 deriving (Eq, Show)
data AT a = Hoja a | Nodo (Indice -> AT a) 

foldAT1 :: ((Indice -> b) -> b) -> (a -> b) -> AT a -> b
foldAT1 cNodo cHoja arbol = case arbol of
    Hoja a -> cHoja a
    Nodo f -> cNodo (rec . f)
    where rec = foldAT1 cNodo cHoja

foldAT2 :: (b -> b -> b -> b) -> (a -> b) -> AT a -> b
foldAT2 cNodo cHoja arbol = case arbol of
    Hoja a -> cHoja a
    Nodo f -> cNodo (rec $ f I1) (rec $ f I2) (rec $ f I3)
    where rec = foldAT2 cNodo cHoja

-- b)
altura :: AT a -> Int
altura = foldAT1 (\f -> 1 + max (f I1) (max (f I2) (f I3) )) (const 1)


-- c)
mapAT1 :: (a -> b) -> AT a -> AT b
mapAT1 m = foldAT1 Nodo (Hoja . m)

-- mapAT2 :: (a -> b) -> AT a -> AT b
-- mapAT2 m =/1 foldAT2 fRec (Hoja . m)
--     where fRec ri rm rd = Nodo (\i -> case i of I1 -> ri
--                                                 I2 -> rm
--                                                 I3 -> rd)

-- Test:
cantHojas :: AT a -> Int
cantHojas = foldAT1 (\f -> f I1 + f I2 + f I3) (const 1)

ejAT :: AT Int
ejAT = Nodo f1

f1 :: Indice -> AT Int
f1 I1 = Hoja 1
f1 I2 = Nodo f2
f1 I3 = Nodo f3

f2 :: Indice -> AT Int
f2 _ = Hoja 2

f3 :: Indice -> AT Int
f3 i | i == I1 || i == I3 = Hoja 3
f3 I2 = Nodo f4

f4 :: Indice -> AT Int
f4 _ = Hoja 4


suma2Primeros :: [Int] -> Int
suma2Primeros (x:y:_) = x+y
suma2Primeros _ = error "No hay 2 elems"

head' :: [a] -> a
head' = foldr const (error "No hay elems")


{-
    Final 19/12/25

    Dado el tipo

    Dicc k v = Def k v (Dicc k v) | Vacío 
    
    Donde puede estar definida varias veces la misma clave, y las definiciones que no son la última forman la "historia" de k:

    a) Dar el tipo e implementación de foldDicc
    b) Implementar definición :: Dicc k v -> k -> Maybe v, que devuelve la definición más nueva de k, y si no existe entonces Nothing 
    c) Implementar sinHistoria : Dicc k v -> k -> Dicc k v
    d) Implementar map que aplica a todos los valores del diccionario una función
    e) Implementar un map modificado que solo aplica la función a la última definición de cada clave

    Comentario: asumo que claves y valores se pueden comparar, por eso los Eq k y Eq v
-}


data Dicc k v = Def k v (Dicc k v) | Vacio deriving Show

foldDicc :: (k -> v -> b -> b) -> b -> Dicc k v -> b
foldDicc cDef cVacio d = case d of
    Vacio -> cVacio
    Def k v r -> cDef k v (foldDicc cDef cVacio r)

foldlDicc :: (b -> k -> v -> b) -> b -> Dicc k v -> b
foldlDicc cDef cVacio d = case d of
    Vacio -> cVacio
    Def k v r -> foldlDicc cDef (cDef cVacio k v) r

definicion :: Eq k => Dicc k v -> k -> Maybe v
definicion = foldDicc fDef fVacio
    where fVacio = const Nothing
          fDef k v f c =  if c == k then Just v else f c

sinHistoria :: Eq k => Eq v => Dicc k v -> Dicc k v
sinHistoria d = foldDicc fDef Vacio d
    where fDef k v r = if v == fromJust (definicion d k) then Def k v r else r

mapDicc :: (v -> b) -> Dicc k v -> Dicc k b
mapDicc f = foldDicc fDef Vacio
    where fDef k v = Def k (f v)

mapDiccMod :: Eq k => (v -> v) -> Dicc k v -> Dicc k v
mapDiccMod f = foldlDicc fDef Vacio
    where fDef ac k v = case definicion ac k of
            Nothing -> Def k (f v) ac
            Just _ -> Def k v ac

mapDiccLast :: Eq k => Eq v => (v -> b) -> Dicc k v -> Dicc k (Either v b)
mapDiccLast f d = revertirDicc (foldDicc fDef Vacio (revertirDicc d))
    where 
        fDef k v r = case definicion r k of
            Nothing -> Def k (Right (f v)) r
            Just _  -> Def k (Left v) r

revertirDicc :: Dicc k v -> Dicc k v
revertirDicc = foldDicc agregarAlFinal Vacio 

agregarAlFinal :: k -> v -> Dicc k v -> Dicc k v
agregarAlFinal k v  = foldDicc Def (Def k v Vacio) 

-- Test
enesimoPar :: Dicc Int Int
enesimoPar = Def 0 0 (Def 1 2 (Def 2 4 (Def 3 6 (Def 4 8 (Def 1 2 Vacio)))))

duplicar :: Int -> Int
duplicar = (*) 2
