{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module ExpCompdata where

import Data.Foldable (toList)
import Data.Comp
import Data.Comp.Derive



--------------------------------------------------------------------------------
-- Generic code

data Var a = Var Name   deriving (Functor, Foldable)
data App a = App a a    deriving (Functor, Foldable)
data Lam a = Lam Name a deriving (Functor, Foldable)

instance ShowF Var where showF (Var n)      = "var" ++ show n
instance ShowF App where showF (App f a)    = "(" ++ f ++ " " ++ a ++ ")"
instance ShowF Lam where showF (Lam n body) = "(\\var" ++ show n ++ " -> " ++ body ++ ")"

type Name = Integer

dummy = 0

app :: (App :<: s) => Term s -> Term s -> Term s
app f a = inject (App f a)

lam :: (Var :<: s, Lam :<: s, Functor s, Foldable s) => (Term s -> Term s) -> Term s
lam f = inject $ Lam n body
  where
    body = f (inject (Var n))
    n    = succ (maxLam body)

maxLam :: (Lam :<: s, Functor s, Foldable s) => Term s -> Name
maxLam (Term f)
    | Just (Lam n _) <- proj f = n
    | otherwise                = maximum $ (0:) $ toList $ fmap maxLam f



--------------------------------------------------------------------------------
-- Example

data NUM a
    = Num Int
    | Add a a
  deriving (Functor, Foldable)

instance ShowF NUM
  where
    showF (Num n)   = show n
    showF (Add a b) = "(" ++ a ++ " + " ++ b ++ ")"

-- Language: Lambda calculus with arithmetic
type Exp = Term (Var :+: App :+: Lam :+: NUM)

instance Num Exp
  where
    fromInteger = inject . Num . fromInteger
    a + b       = inject (Add a b)

-- \y.(\x.x)(\x.x)y
example1 :: Exp
example1 = lam (\y -> app (app (lam (\x -> x)) (lam (\x -> x))) y)

example2 :: Exp
example2 = lam (\y -> app (lam (\x -> x+2+x)) (y+y))

