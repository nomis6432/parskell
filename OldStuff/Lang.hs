{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Lang where

-- | Recursive types and structurally recursive functions

data Expr f = In (f (Expr f))

fold :: Functor f => (f a -> a) -> (Expr f -> a)
fold alg (In t) = alg (fmap (fold alg) t)

-- | Co-products (sums) of functors

data (f :+: g) a = Inl (f a) | Inr (g a)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl t) = Inl (fmap f t)
  fmap f (Inr t) = Inr (fmap f t)

-- | Functor inclusion

class (Functor f, Functor g) => f <: g where
  inj :: f a -> g a
  prj :: g a -> Maybe (f a)

instance Functor f => f <: f where
  inj = id
  prj = Just

instance (Functor f, Functor g) => f <: (f :+: g) where
  inj = Inl
  prj (Inl t) = Just t
  prj (Inr t) = Nothing

instance (Functor g, f <: h) => f <: (g :+: h) where
  inj = Inr . inj
  prj (Inl t) = Nothing
  prj (Inr t) = prj t

inject :: f <: g => f (Expr g) -> Expr g
inject = In . inj

-- | Values

data Val a = Val Int
  deriving Functor

val :: Val <: f => Int -> Expr f
val n = inject (Val n)

-- | Addition

data Add a = Add a a
  deriving Functor

add :: Add <: f => Expr f -> Expr f -> Expr f
add e1 e2 = inject (Add e1 e2)

-- | Multiplication

data Mul a = Mul a a
  deriving Functor

mul :: Mul <: f => Expr f -> Expr f -> Expr f
mul e1 e2 = inject (Mul e1 e2)

-- | Variables

type VarId = String

data Var a = Var VarId
  deriving Functor

{-
instance Functor Var where
  fmap f (Var x) = Var x
-}

var :: Var <: f => String -> Expr f
var x = inject (Var x)

-- | Rendering

class Functor f => Render f where
  renderAlg :: f String -> String

instance (Render f1, Render f2) => Render (f1 :+: f2) where
  renderAlg (Inl t) = renderAlg t
  renderAlg (Inr t) = renderAlg t

render :: Render f => Expr f -> String
render = fold renderAlg


instance Render Val where
  renderAlg (Val n) = show n

instance Render Add where
  renderAlg (Add e1 e2) = "(" ++ e1 ++ "+" ++ e2 ++ ")"

instance Render Mul where
  renderAlg (Mul e1 e2) = "(" ++ e1 ++ "*" ++ e2 ++ ")"

instance Render Var where
  renderAlg (Var x) = x

-- | Id

-- | Rendering

class Functor f => Diff f h where
  diffAlg :: f (Expr h, VarId -> Expr h) -> (Expr h, VarId -> Expr h)

instance (Diff f1 h, Diff f2 h) => Diff (f1 :+: f2) h where
  diffAlg (Inl t) = diffAlg t
    -- Inl t :: (f1 :+: f2) (VarId -> Expr h)
    -- Inl :: f1 a -> (f1 :+: f2) a
    -- t :: f1 (VarId -> Expr h)
    -- diffAlg t :: (VarId -> Expr h)

  diffAlg (Inr t) = diffAlg t

differentiate :: Diff f h => Expr f -> (VarId -> Expr h)
differentiate = snd . fold diffAlg

instance Val <: h => Diff Val h where
  diffAlg (Val n) = (val n, \x -> val 0)

instance Add <: h => Diff Add h where
  diffAlg (Add (e1,de1) (e2,de2)) =
     (add e1 e2, \x -> add (de1 x) (de2 x))

instance (Var <: h, Val <: h) => Diff Var h where
  diffAlg (Var x) = (var x, \y -> if x == y then val 1 else val 0)

instance  (Mul <: h, Add <: h) => Diff Mul h where
  diffAlg (Mul (e1, de1) (e2, de2)) = (mul e1 e2, \x -> (de1 x `mul` e2)  `add` (e1 `mul` de2 x))

-- | Examples

example1 :: Expr Val
example1 = val 5

example2 :: Expr (Val :+: Add)
example2 = val 3 `add` val 4

example3 :: Expr (Val :+: Mul)
example3 = val 3 `mul` val 4

example4 :: Expr (Val :+: (Add :+: Mul))
example4 = (val 3 `mul` val 4) `add` val 1

example5 :: Expr (Var :+: (Val :+: (Add :+: Mul)))
example5 = (val 3 `mul` var "x") `add` val 1

type BigLang = Expr (Var :+: (Val :+: (Add :+: Mul)))
