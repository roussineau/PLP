data AIH a = Hoja a | Bin (AIH a) (AIH a)
-- Ejercicio 14)

aih :: AIH Integer
aih = Bin (Hoja 1) (Bin (Hoja 1) (Hoja 4))

-- a)
foldAIH :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH fHoja fBin aih = case aih of
    Hoja a -> fHoja a
    Bin i d -> fBin (foldAIH fHoja fBin i) (foldAIH fHoja fBin d)

-- b)
altura :: AIH a -> Integer
altura = foldAIH (const 1) (\ai ad -> 1 + max ai ad)

tamaño :: AIH a -> Integer
tamaño = foldAIH (const 1) (+)


-- Ejercicio 15)
-- i)
data Rosetree a = Rose a [Rosetree a]

rose :: Rosetree Int
rose = Rose 2 [Rose 3 [], Rose 4 [Rose 5 []]]

-- ii)
foldRose :: (a -> [b] -> b) -> Rosetree a -> b
foldRose f (Rose r rs) = f r (map (foldRose f) rs)

-- iii)
-- a)
hojas :: Rosetree a -> [a]
hojas = foldRose (\v r -> if null r then [v] else concat r)
-- b)