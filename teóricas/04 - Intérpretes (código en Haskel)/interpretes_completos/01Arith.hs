
data Expr = EConstNum Int
          | EAdd Expr Expr

eval :: Expr -> Int
eval (EConstNum n) = n
eval (EAdd e1 e2)  = eval e1 + eval e2

