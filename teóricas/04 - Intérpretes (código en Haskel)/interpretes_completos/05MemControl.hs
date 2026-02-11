import Environment
import Memory

data Expr = EConstNum Int
          | EConstBool Bool
          | EAdd Expr Expr
          | EVar Id
          | ELet Id Expr Expr
          | ESeq Expr Expr
          | EAssign Id Expr
          --
          | ELtNum Expr Expr
          | EIf Expr Expr Expr
          | EWhile Expr Expr

data Val = VN Int
         | VB Bool
  deriving Show

addVal :: Val -> Val -> Val
addVal (VN a) (VN b) = VN (a + b)
addVal _ _           = error "Los valores no son numéricos."

ltNumVal :: Val -> Val -> Val
ltNumVal (VN a) (VN b) = VB (a < b)
ltNumVal _ _           = error "Los valores no son numéricos."

isTrueVal :: Val -> Bool
isTrueVal (VB b) = b
isTrueVal _      = False

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
eval (ELtNum e1 e2) env mem0 =
  let (v1, mem1) = eval e1 env mem0
      (v2, mem2) = eval e2 env mem1
   in (v1 `ltNumVal` v2, mem2)
eval (EIf e1 e2 e3) env mem0 =
  let (v1, mem1) = eval e1 env mem0 in
    if isTrueVal v1
     then eval e2 env mem1
     else eval e3 env mem1
eval (EWhile e1 e2) env mem0 =
  let (v1, mem1) = eval e1 env mem0 in
    if isTrueVal v1
     then
       let (v2, mem2) = eval e2 env mem1 in
         eval (EWhile e1 e2) env mem2
     else (VN 0, mem1)

ejemploIf :: Expr
ejemploIf =
  ELet "x" (EConstNum 21)
    (ELet "cond" (EConstBool True)
      (EIf (EVar "cond")
              (EAdd (EVar "x") (EVar "x"))
              (EConstNum 0)))

ejemploWhile :: Expr
ejemploWhile =
  ELet "x" (EConstNum 0)
    (ELet "y" (EConstNum 1)
      (ESeq
        (EWhile (ELtNum (EVar "x") (EConstNum 7))
           (ESeq
             (EAssign "x" (EAdd (EVar "x") (EConstNum 1)))
             (EAssign "y" (EAdd (EVar "y") (EVar "y")))))
        (EVar "y")))

