-- Teóricas 0 y 1:

foldInt :: (Int -> a -> a) -> a -> Int -> a
foldInt f z 0 = z
foldInt f z n = f n (foldInt f z (n-1)) 

factorial :: Int -> Int
factorial = foldInt (*) 1

sumaN :: Int -> [Int] -> [Int]
sumaN _ [] = []
sumaN k (x:xs) = (k + x) : sumaN k xs

aparece :: Eq a => a -> [a] -> Bool
aparece _ [] = False
aparece n (x:xs) = n == x || aparece n xs

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar (x:xs)
    | not (null xs) = if x <= head rec then x : rec else head rec : ordenar (x : tail rec)
    | otherwise = [x]
    where rec = ordenar xs

data PuntoCardinal = Norte | Este | Sur | Oeste

opuesta :: PuntoCardinal -> PuntoCardinal
opuesta Norte = Sur
opuesta Este = Oeste
opuesta Sur = Norte
opuesta Oeste = Este

-- data Maybe a = Nothing | Just a
data AB a = Nil | Bin (AB a) a (AB a) deriving Show

buscar :: Eq a => a -> AB (a, b) -> Maybe b
buscar _ Nil = Nothing
buscar a (Bin i (k, v) d)
    | a == k = Just v
    | otherwise = case buscar a i of
                    Nothing -> buscar a d
                    Just x  -> Just x

-- foldr
foldrLista :: (a -> b -> b) -> b -> [a] -> b
foldrLista _ fBase [] = fBase
foldrLista fRec fBase (x:xs) = fRec x (foldrLista fRec fBase xs)

foldlLista :: (b -> a -> b) -> b -> [a] -> b
foldlLista _ fBase [] = fBase
foldlLista fRec fBase (x:xs) = fRec (foldlLista fRec fBase xs) x

mapLista :: (a -> b) -> [a] -> [b]
mapLista f = foldrLista (\x r -> f x : r) []

-- (.) :: (a -> b) -> (c -> a) -> c -> b
-- (f . g) x = f (g x) -- f compuesta de g de x

sonPares :: [Int] -> [Bool]
sonPares = foldrLista (\x r -> even x : r) []

longitudLista :: [a] -> Int
longitudLista = foldrLista (\_ r -> 1 + r) 0

filtrarLista :: (a -> Bool) -> [a] -> [a]
filtrarLista f = foldrLista (\x r -> if f x then x:r else r) []


merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge criterio = foldr paso id
    where
        paso x r [] = x : r []
        paso x r (y:ys) = if criterio x y then x : r (y:ys) else y : paso x r ys


mergesort :: (a -> a -> Bool) -> [a] -> [a]
mergesort criterio l = if length l <= 1
                       then l
                       else uncurry (merge criterio) ((\(l1, l2) -> (mergesort criterio l1, mergesort criterio l2)) (splitAt (div (length l) 2) l))

operatoria :: (a -> a -> a) -> [a] -> a
operatoria = foldr1Lista

foldr1Lista :: (a -> a -> a) -> [a] -> a
foldr1Lista _ [x] = x
foldr1Lista f (x:xs) = f x (foldr1Lista f xs)

sumatoria :: [Int] -> Int
sumatoria = operatoria (+)

productoria :: [Int] -> Int
productoria = operatoria (*)

mientras :: (a -> Bool) -> (a -> a) -> a -> a
mientras condicion f x = if condicion x then mientras condicion f (f x) else x

enesimoFibonacci :: Int -> Int
enesimoFibonacci n = (\(f, _, _) -> f) (mientras (\(_, _, contador) -> contador < n) (\(actual, siguiente, contador) -> (siguiente, actual + siguiente, contador + 1)) (0, 1, 0))

data ABI a = IBin (ABI a) a (ABI a) deriving Show



podadoDesdeNivel :: Int -> ABI a -> AB a
podadoDesdeNivel = foldInt f z
    where z _ = Nil
          f n r (IBin i v d) = Bin (r i) v (r d)

-- Para testear: nats es un ABI de todos los números naturales
-- El test sería: podadoDesdeNivel 3 nats
nats :: ABI Int
nats = generar 1
  where generar n = IBin (generar (n+1)) n (generar (n+1))



data Direccion = Izq | Der

type Camino = [Direccion]

type FuncionDeCaminos a = Camino -> a


funcionDeCaminosDe :: ABI a -> [Direccion] -> a
funcionDeCaminosDe (IBin i r d) [] = r
funcionDeCaminosDe (IBin i r d) (x:xs) = case x of
    Izq -> funcionDeCaminosDe i xs
    Der -> funcionDeCaminosDe d xs

-- correcursión
abiDe :: ([Direccion] -> a) -> ABI a
abiDe f = IBin i r d
    where r = f []
          i = abiDe (\c -> f (Izq : c))
          d = abiDe (\c -> f (Der : c))
-- Explicación: tengo que construir el árbol, y tengo la función f que me dice, dado un camino, a qué nodo llego
-- Si le pregunto por el camino vacío, me devuelve donde ya estoy
-- Si le pregunto por un camino que empieza con Izq, me devuelve uno que va por la rama izquierda
-- Si le pregunto por un camino que empieza con Der, me devuelve uno que va por la rama derecha


data Conj a = CConj [a] deriving Show
-- Invariante: la lista no debe tener repetidos

vacio :: Conj a
vacio = CConj []

foldConj :: (a -> b -> b) -> b -> Conj a -> b
foldConj _ fBase (CConj []) = fBase
foldConj fRec fBase (CConj (x:xs)) = fRec x (foldConj fRec fBase (CConj xs))

pertenece :: Eq a => a -> Conj a -> Bool
pertenece x = foldConj (\a r -> x == a || r) False

insertar :: Eq a => a -> Conj a -> Conj a
insertar x = foldConj (\a r -> if x == a then r else let CConj listaRec = r in CConj (a:listaRec)) (CConj [x])

eliminar :: Eq a => a -> Conj a -> Conj a
eliminar x = foldConj (\a r -> if x == a then r else let CConj listaRec = r in CConj (a:listaRec)) (CConj [])



allPairs :: [(Int, Int)]
allPairs = [ (x,y) | z <- [0..], x <- [0..z], y <- [0..z], z == x+y ]


quicksort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
quicksort _ [] = []
quicksort criterio (x:xs) = 
    let menores = [ a | a <- xs, criterio a x ]
        mayores = [ a | a <- xs, not (criterio a x) ]
    in  quicksort criterio menores ++ [x] ++ quicksort criterio mayores

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x:xs) = f x xs (recr f z xs)


foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f = recr (\x _ r -> f x r )

recr' :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr' f z = fst . foldr (\x (r, l) -> (f x l r, x:l)) (z, [])
-- Guardamos el estado (resultado, lista original) en una tupla siempre


-- Ejercicios de la guía de programación funcional

-- Ej 14
data AIH a = Hoja a | IHBin (AIH a) (AIH a) deriving Show

foldAIH :: (b -> b -> b) -> (a -> b) -> AIH a -> b
foldAIH f z (Hoja i) = z i
foldAIH f z (IHBin l r) = f (foldAIH f z l) (foldAIH f z r)

alturaAIH :: AIH a -> Int
alturaAIH = foldAIH (\ai ad -> 1 + max ai ad) (const 1) 

tamañoAIH :: AIH a -> Int
tamañoAIH = foldAIH (+) (const 1)

-- Ej 19
pitagoricas :: [(Int, Int, Int)]
pitagoricas = [ (a,b,c) | c <- [1..], a <- [1..c], b <- [1..c], a^2 + b^2 == c^2 ]

-- Ej 20
listasQueSuman :: Int -> [[Int]]
listasQueSuman 0 = [[]]
listasQueSuman n | n > 0 = [ x:xs | x <- [1..n], xs <- listasQueSuman (n-x) ]
-- Recursión global: usa múltiples llamados con diferentes subestructuras

-- Ej 21
todasLasListas :: [[Int]]
todasLasListas = [ l | x <- [1..], l <- listasQueSuman x]

-- Ej 22
todosLosAIH :: [AIH ()]
todosLosAIH = [ a | h <- [1..], a <- aihsDeCantHojas h]

aihsDeCantHojas :: Int -> [AIH ()]
aihsDeCantHojas 1 = [Hoja ()]
aihsDeCantHojas n | n > 1 = [ IHBin i d | hojasIzq <- [1..(n-1)],
                                          hojasDer <- [1..(n-1)],
                                          hojasDer == n - hojasIzq,
                                          i <- aihsDeCantHojas hojasIzq,
                                          d <- aihsDeCantHojas hojasDer ]
-- No es recursión estructural, sino global, porque necesitamos múltiples
-- llamados recursivos sobre subestructuras que no son las inmediatas
