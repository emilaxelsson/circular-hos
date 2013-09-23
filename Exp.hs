module Exp where

data Exp = Var Name
         | App Exp Exp
         | Lam Name Exp

instance Show Exp
  where
    show (Var n)      = "var" ++ show n
    show (App f a)    = "(" ++ show f ++ " " ++ show a ++ ")"
    show (Lam n body) = "(\\var" ++ show n ++ " -> " ++ show body ++ ")"

type Name = Integer

dummy = 0

app :: Exp -> Exp -> Exp
app = App

lam :: (Exp -> Exp) -> Exp
lam f = Lam n body
  where
    body = f (Var n)
    n    = succ (maxLam body)

maxLam :: Exp -> Name
maxLam (Var _)   = dummy
maxLam (App a b) = max (maxLam a) (maxLam b)
maxLam (Lam n _) = n  -- stop search

-- \y.(\x.x)(\x.x)y
example = lam (\y -> app (app (lam (\x -> x)) (lam (\x -> x))) y)

