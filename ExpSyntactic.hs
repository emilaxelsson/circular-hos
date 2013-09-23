{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module ExpSyntactic where

import Language.Syntactic



--------------------------------------------------------------------------------
-- Generic code

data Var a where Var :: Name -> Var (Full a)
data App a where App :: App ((a -> b) :-> a :-> Full b)
data Lam a where Lam :: Name -> Lam (b :-> Full (a -> b))

instance Render Var where render (Var n)            = "var" ++ show n
instance Render App where renderArgs [f,a] App      = "(" ++ f ++ " " ++ a ++ ")"
instance Render Lam where renderArgs [body] (Lam n) = "(\\var" ++ show n ++ " -> " ++ body ++ ")"

type Name = Integer

dummy = 0

app :: (App :<: s) => ASTF s (a -> b) -> ASTF s a -> ASTF s b
app = appSym App

lam :: (Var :<: s, Lam :<: s) => (ASTF s a -> ASTF s b) -> ASTF s (a -> b)
lam f = appSym (Lam n) body
  where
    body = f (appSym (Var n))
    n    = succ (maxLam body)

maxLam :: (Lam :<: s) => AST s a -> Name
maxLam (Sym lam :$ _) | Just (Lam n) <- prj lam = n
maxLam (s :$ a) = maxLam s `max` maxLam a
maxLam _ = dummy



--------------------------------------------------------------------------------
-- Example

data NUM a
  where
    Num :: (Num a, Show a) => a -> NUM (Full a)
    Add :: NUM (a :-> a :-> Full a)

instance Render NUM
  where
    renderArgs [] (Num n) = show n
    renderArgs [a,b] Add  = "(" ++ a ++ " + " ++ b ++ ")"

-- Language: Lambda calculus with arithmetic
type Exp a = ASTF (Var :+: App :+: Lam :+: NUM) a

instance (Num a, Show a) => Num (Exp a)
  where
    fromInteger = appSym . Num . fromInteger
    (+) = appSym Add

-- \y.(\x.x)(\x.x)y
example1 :: Exp (a -> a)
example1 = lam (\y -> app (app (lam (\x -> x)) (lam (\x -> x))) y)

example2 :: Exp (Int -> Int)
example2 = lam (\y -> app (lam (\x -> x+2+x)) (y+y))

