{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module OldStuff.Syntax.Types0 where

import Control.Monad
import Control.Monad.Fail
import Prelude hiding (abs)

-- | Recursive types and structurally recursive functions

data Fix f = In (f (Fix f))

fold :: Functor f => (f a -> a) -> (Fix f -> a)
fold alg (In t) = alg (fmap (fold alg) t)

-- | Co-products (sums) of functors


data (f :+: g) a = Inl (f a) | Inr (g a)

infixr :+:

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl t) = Inl (fmap f t)
  fmap f (Inr t) = Inr (fmap f t)

-- | Functor inclusion

class (Functor f, Functor g) => f :<: g where
  inj :: f a -> g a
  prj :: g a -> Maybe (f a)

instance Functor f => f :<: f where
  inj = id
  prj = Just

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
  prj (Inl t) = Just t
  prj (Inr t) = Nothing

instance (Functor g, f :<: h) => f :<: (g :+: h) where
  inj = Inr . inj
  prj (Inl t) = Nothing
  prj (Inr t) = prj t

inject :: (f :<: g) => f (Fix g) -> Fix g
inject = In . inj

match :: (g :<:f) => Fix f -> Maybe (g (Fix f))
match (In t) = prj t

-- | stuff


data Arith e = Val Int | Add   e e
  deriving Functor

val :: (Arith :<: f) => Int -> Fix f
val n1 = inject (Val n1)

add :: (Arith :<: f) => Fix f -> Fix f -> Fix f
add n1 n2 = inject (Add n1 n2)

data Except e = Throw   | Catch e e
  deriving Functor

throw :: (Except :<: f) => Fix f
throw = inject (Throw)

catch :: (Except :<: f) => Fix f -> Fix f -> Fix f
catch n1 n2 = inject (Catch n1 n2)

type Expr = Fix (Arith :+: Except)

-- | evaluate

class (Functor f, Monad m) => Eval f m where
  evAlg :: f (m (Value m)) -> m (Value m)

instance MonadFail m => Eval Arith m where
  evAlg (Val n)   = return (Num n)
  evAlg (Add x y) = do
    (Num n) <- x
    (Num m) <- y
    return (Num (n + m))

instance MonadPlus m => Eval Except m where
  evAlg (Throw)     = mzero
  evAlg (Catch x h) = x `mplus` h

instance (Eval f m, Eval g m) =>Eval (f :+: g) m where
  evAlg (Inl x) = evAlg x
  evAlg (Inr y) = evAlg y
{-
eval :: Eval f m => Fix f -> [Value m]
eval = fold evAlg

three :: Fix Arith
three = val 1 `add` val 2
error :: Fix (Arith :+: Except)
error = val 42 `add` throw
-}
-- | lambda stuff

data Lambda e = Index Int | Abs e | Apply e e
  deriving Functor

index :: (Lambda :<: f) => Int -> Fix f
index n1 = inject (Index n1)

abs :: (Lambda :<: f) => Fix f -> Fix f
abs n1 = inject (Abs n1)

apply :: (Lambda :<: f) => Fix f -> Fix f -> Fix f
apply n1 n2 = inject (Apply n1 n2)

data Value m where
  Num  :: Int -> Value m
  Clos :: Monad m => [Value m] ->m (Value m) -> Value m

class Monad m => CBVMonad m where
  env  :: m [Value m]
  with :: [Value m] -> m (Value m) -> m (Value m)

instance (CBVMonad m, MonadFail m) => Eval Lambda m where
  evAlg (Index i)   = do
    e <- env
    return (e !! i)
  evAlg (Abs t)     = do
    e <- env
    return (Clos e t)
  evAlg (Apply f x) = do
    (Clos ctx t) <- f
    c <- x
    with (c:ctx) t

e :: Fix (Lambda :+: Arith)
e = apply (abs (index 0)) (add (val 1) (val 2))
