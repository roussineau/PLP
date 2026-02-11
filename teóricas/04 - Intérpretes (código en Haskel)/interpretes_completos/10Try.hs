import Environment

data Expr = EConstNum Int
          | EAdd Expr Expr
          | EVar Id
          | ELet Id Expr Expr
          | EDiv Expr Expr
          | ETry Expr Expr

eval :: Expr -> Env Int -> Maybe Int
eval (EConstNum n)  env = Just n
eval (EAdd e1 e2)   env =
  case eval e1 env of
    Nothing -> Nothing
    Just n1 ->
      case eval e2 env of
        Nothing -> Nothing
        Just n2 -> Just (n1 + n2)
eval (EVar x)       env = Just (lookupEnv env x)
eval (ELet x e1 e2) env =
  case eval e1 env of
    Nothing -> Nothing
    Just n1 -> eval e2 (extendEnv env x n1)
eval (EDiv e1 e2) env =
  case eval e1 env of
    Nothing -> Nothing
    Just n1 ->
       case eval e2 env of
         Nothing -> Nothing
         Just n2 ->
           if n2 == 0
            then Nothing
            else Just (n1 `div` n2)
eval (ETry e1 e2) env =
  case eval e1 env of
    Nothing -> eval e2 env
    Just n1 -> Just n1

ejemplo :: Expr
ejemplo = ETry (EDiv (EConstNum 5) (EConstNum 0))
               (EConstNum 2)

