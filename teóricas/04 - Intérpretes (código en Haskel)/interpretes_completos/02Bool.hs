
data Expr = EConstNum Int
          | EConstBool Bool
          | EAdd Expr Expr

data Val = VN Int
         | VB Bool
  deriving Show

addVal :: Val -> Val -> Val
addVal (VN a) (VN b) = VN (a + b)
addVal _ _           = error "Los valores no son numéricos."

eval :: Expr -> Val
eval (EConstNum n)  = VN n
eval (EConstBool b) = VB b
eval (EAdd e1 e2)   = addVal (eval e1) (eval e2)

