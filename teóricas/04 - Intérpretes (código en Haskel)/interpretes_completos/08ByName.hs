import Environment

data Expr = EVar Id
          | ELam Id Expr
          | EApp Expr Expr
          | EConstNum Int
          | EConstBool Bool
          | EAdd Expr Expr
          | ELet Id Expr Expr
          | EIf Expr Expr Expr
  deriving Show

data Thunk = TT Expr (Env Thunk)

data Val = VN Int
         | VB Bool
         | VClosure Id Expr (Env Thunk)

instance Show Val where
  show (VN n)           = "VN " ++ show n
  show (VB b)           = "VB " ++ show b
  show (VClosure _ _ _) = "<clausura>"

addVal :: Val -> Val -> Val
addVal (VN a) (VN b) = VN (a + b)
addVal _ _           = error "Los valores no son numéricos."

isTrueVal :: Val -> Bool
isTrueVal (VB b) = b
isTrueVal _      = False

eval :: Expr -> Env Thunk -> Val
eval (EVar x)      env =
  case lookupEnv env x of
    TT expr2 env2 -> eval expr2 env2
eval (ELam x e)    env = VClosure x e env
eval (EApp e1 e2)  env =
  let v1 = eval e1 env
   in case v1 of
        VClosure x e1' envOrig -> eval e1' (extendEnv envOrig x (TT e2 env))
        _ -> error "Lo que se aplica no es una función."
eval (EConstNum n)  env = VN n
eval (EConstBool b) env = VB b
eval (EAdd e1 e2)   env = eval e1 env `addVal` eval e2 env
eval (ELet x e1 e2) env = eval e2 (extendEnv env x (TT e1 env))
eval (EIf e1 e2 e3) env =
  if isTrueVal (eval e1 env)
   then eval e2 env
   else eval e3 env

ejemplo :: Expr
ejemplo =
  ELet "suma" (ELam "x"
                (ELam "y"
                  (EAdd (EVar "x") (EVar "y"))))
    (ELet "f" (EApp (EVar "suma") (EConstNum 5))
      (ELet "x" (EConstNum 0)
        (EApp (EVar "f") (EConstNum 3))))

