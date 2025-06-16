data AEB a = Hoja a | Bin (AEB a) a (AEB a)

foldAEB :: (a -> b) -> (b -> a -> b -> b) -> AEB a -> b
foldAEB fHoja fBin (Hoja a) = fHoja a
foldAEB fHoja fBin (Bin i r d) = fBin (rec i) r (rec d)
    where rec = foldAEB fHoja fBin

esPreRama :: Eq a => AEB a -> [a] -> Bool
esPreRama = foldAEB (\a xs -> null xs || xs == [a])
                    (\i r d xs -> case xs of
                                    [] -> True
                                    (y:ys) -> r == y && (i ys || d ys))
