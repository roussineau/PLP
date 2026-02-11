
module Memory(Addr, Mem, emptyMem, freeAddress, load, store) where

type Addr = Int
data Mem a = MM [(Addr, a)]

instance Show (Mem a) where
  show (MM _) = "<memoria>"

emptyMem :: Mem a
emptyMem = MM []

freeAddress :: Mem a -> Addr
freeAddress (MM []) = 0
freeAddress (MM xs) = maximum (map fst xs) + 1

load :: Mem a -> Addr -> a
load (MM xs) a =
  case lookup a xs of
    Just b  -> b
    Nothing -> error "La dirección de memoria no está."

store :: Mem a -> Addr -> a -> Mem a
store (MM xs) a b = MM ((a, b) : xs)

