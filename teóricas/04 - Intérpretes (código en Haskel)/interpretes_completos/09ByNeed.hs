import Control.Monad.State(State, evalState, get, put)

import Environment(Id, Env, emptyEnv, lookupEnv, extendEnv)
import Memory(Addr, Mem, emptyMem, freeAddress, store, load)

data Expr = EVar Id
          | ELam Id Expr
          | EApp Expr Expr

data Val = VN Int
         | VB Bool
         | VClosure Id Expr (Env Addr)
         | VThunk Expr (Env Addr)

instance Show Val where
  show (VN n)           = "VN " ++ show n
  show (VB b)           = "VB " ++ show b
  show (VClosure _ _ _) = "<clausura>"
  show (VThunk _ _)     = "<thunk>"

eval :: Expr -> Env Addr -> Mem Val -> (Val, Mem Val)
eval (EVar x)     env mem0 =
  let a = lookupEnv env x in
    case load mem0 a of
      VThunk e env' -> let (val, mem1) = eval e env' mem0 in
                         (val, store mem1 a val)
      val           -> (val, mem0)
eval (ELam x e)   env mem0 = (VClosure x e env, mem0)
eval (EApp e1 e2) env mem0 =
  let (VClosure x e1' env', mem1) = eval e1 env mem0
      addr = freeAddress mem1 
   in
      eval e1' (extendEnv env' x addr) (store mem1 addr (VThunk e2 env))

