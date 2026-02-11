import Environment
import Memory

data Expr = EConstNum Int
          | EConstBool Bool
          | EAdd Expr Expr
          | EVar Id
          | ELet Id Expr Expr
          | ESeq Expr Expr
          | EAssign Id Expr

data Val = VN Int
         | VB Bool
  deriving Show

addVal :: Val -> Val -> Val
addVal (VN a) (VN b) = VN (a + b)
addVal _ _           = error "Los valores no son numéricos."

eval :: Expr -> Env Addr -> Mem Val -> (Val, Mem Val)
eval (EConstNum n)  env mem0 = (VN n, mem0)
eval (EConstBool b) env mem0 = (VB b, mem0)
eval (EAdd e1 e2)   env mem0 =
  let (v1, mem1) = eval e1 env mem0
      (v2, mem2) = eval e2 env mem1
   in (v1 `addVal` v2, mem2)
eval (EVar x)       env mem0 = (load mem0 (lookupEnv env x), mem0)
eval (ELet x e1 e2) env mem0 =
  let (v1, mem1) = eval e1 env mem0
      addr = freeAddress mem1
      (v2, mem2) = eval e2 (extendEnv env x addr) (store mem1 addr v1)
   in (v2, mem2)
eval (ESeq e1 e2) env mem0 =
  let (v1, mem1) = eval e1 env mem0
      (v2, mem2) = eval e2 env mem1
   in (v2, mem2)
eval (EAssign x e) env mem0 =
  let (v1, mem1) = eval e env mem0 in
    (VN 0, store mem1 (lookupEnv env x) v1)

ejemplo :: Expr
ejemplo =
  ELet "x"
    (EConstNum 5)
    (ELet "y" (EConstNum 1)
      (ESeq
        (EAssign "x" (EAdd (EVar "x") (EVar "y")))
        (ESeq
          (EAssign "x" (EAdd (EVar "x") (EVar "y")))
          (ESeq
            (EAssign "x" (EAdd (EVar "x") (EVar "y")))
            (EVar "x")))))

