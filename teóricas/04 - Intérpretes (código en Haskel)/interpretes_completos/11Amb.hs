
import Environment

data Expr = EConstNum Int
          | EAdd Expr Expr
          | EVar Id
          | ELet Id Expr Expr
          | EAmb Expr Expr

eval :: Expr -> Env Int -> [Int]
eval (EConstNum n)  env = [n]
eval (EAdd e1 e2)   env =
  [n1 + n2 | n1 <- eval e1 env, n2 <- eval e2 env]
eval (EVar x)       env = [lookupEnv env x]
eval (ELet x e1 e2) env =
  [n2 | n1 <- eval e1 env,
        n2 <- eval e2 (extendEnv env x n1)]
eval (EAmb e1 e2) env =
  eval e1 env ++ eval e2 env

ejemplo :: Expr
ejemplo = 
  ELet "x" (EAmb (EConstNum 10) (EConstNum 20))
    (ELet "y" (EAmb (EConstNum 1) (EConstNum 2))
      (EAdd (EVar "x") (EVar "y")))

