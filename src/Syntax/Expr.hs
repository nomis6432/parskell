{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-} --TODO:: REMOVE!!

module Syntax.Expr where

import Syntax.GameMonad

import Control.Monad
import Control.Applicative

import Prelude as Prel

import Data.Map.Strict as Map

import Language.Syntactic
import Language.Syntactic.Functional

-- | different Types and constructors

data NUM a where
  Num :: Int -> NUM (Full Int)
  Add :: NUM (Int :-> Int :-> Full Int)
  Sub :: NUM (Int :-> Int :-> Full Int)
  Mul :: NUM (Int :-> Int :-> Full Int)
  Pow :: NUM (Int :-> Int :-> Full Int)

num :: (NUM :<: dom) => Int -> ASTF dom Int
num = inj . Num

(+) :: (NUM :<: dom) => ASTF dom Int -> ASTF dom Int -> ASTF dom Int
a + b = inj Add :$ a :$ b

(-) :: (NUM :<: dom) => ASTF dom Int -> ASTF dom Int -> ASTF dom Int
a - b = inj Sub :$ a :$ b

(*) :: (NUM :<: dom) => ASTF dom Int -> ASTF dom Int -> ASTF dom Int
a * b = inj Mul :$ a :$ b

(**) :: (NUM :<: dom) => ASTF dom Int -> ASTF dom Int -> ASTF dom Int
a ** b = inj Pow :$a :$ b

infixl 3 +
infixl 3 -
infixl 4 *
infixl 5 **

data Logic a where
  Bl   :: Bool -> Logic (Full Bool)
  Not  :: Logic (Bool :-> Full Bool)
  Eq   :: Eq  a => Logic (a :-> a :-> Full Bool)
  Lt   :: Ord a => Logic (a :-> a :-> Full Bool)
  Lteq :: Ord a => Logic (a :-> a :-> Full Bool)
  Gt   :: Ord a => Logic (a :-> a :-> Full Bool)
  Gteq :: Ord a => Logic (a :-> a :-> Full Bool)
  And  :: Logic (Bool :-> Bool :-> Full Bool)
  Or   :: Logic (Bool :-> Bool :-> Full Bool)

bool :: (Logic :<: dom) => Bool -> ASTF dom Bool
bool = inj . Bl

not :: (Logic :<: dom) => ASTF dom Bool -> ASTF dom Bool
not a = inj Not :$ a

(==) :: (Logic :<: dom, Eq a) => ASTF dom a -> ASTF dom a -> ASTF dom Bool
a == b = inj Eq  :$ a :$ b

(<) :: (Logic :<: dom, Ord a) => ASTF dom a -> ASTF dom a -> ASTF dom Bool
a < b = inj Lt  :$ a :$ b

(<=) :: (Logic :<: dom, Ord a) => ASTF dom a -> ASTF dom a -> ASTF dom Bool
a <= b = inj Lteq  :$ a :$ b

(>) :: (Logic :<: dom, Ord a) => ASTF dom a -> ASTF dom a -> ASTF dom Bool
a > b = inj Gt  :$ a :$ b

(>=) :: (Logic :<: dom, Ord a) => ASTF dom a -> ASTF dom a -> ASTF dom Bool
a >= b = inj Gteq  :$ a :$ b

(&&) :: (Logic :<: dom) => ASTF dom Bool -> ASTF dom Bool -> ASTF dom Bool
a && b = inj And :$ a :$ b

(||) :: (Logic :<: dom) => ASTF dom Bool -> ASTF dom Bool -> ASTF dom Bool
a || b = inj Or :$ a :$ b

infixl 1 &&
infixl 1 ||
infixl 2 ==
infixl 2 <
infixl 2 <=
infixl 2 >
infixl 2 >=

data If a where
  If :: If (Bool :-> a :-> a :-> Full a)

condition :: (If :<: dom) => ASTF dom Bool -> ASTF dom a -> ASTF dom a -> ASTF dom a
condition c t f = inj If  :$ c :$ t :$ f

data ExprComb a where
  Pass :: ExprComb (Full ())
  SemiColumn :: ExprComb (() :-> () :-> Full ())
  Drop :: ExprComb (a :-> Full ())
  While :: ExprComb (Bool :-> a :-> Full ())

pass :: (ExprComb :<: dom) => ASTF dom ()
pass = inj Pass

(%%) :: (ExprComb :<: dom) => ASTF dom () -> ASTF dom () -> ASTF dom ()
(%%) a b = inj SemiColumn :$ a :$ b

drp :: (ExprComb :<: dom) => ASTF dom a -> ASTF dom ()
drp a = inj Drop :$ a

($$) :: (ExprComb :<: dom) => ASTF dom a -> ASTF dom b -> ASTF dom ()
($$) a b = drp a %% drp b

while :: (ExprComb :<: dom) => ASTF dom Bool -> ASTF dom a -> ASTF dom ()
while c a = inj While :$ c :$ a

loop :: (ExprComb :<: dom, Logic :<: dom) => ASTF dom a -> ASTF dom ()
loop a = while (bool True) a

infixl 0 %%
infixl 0 $$

data Action a where
  Input  :: Action (Full Int)
  Output :: Action (Int :-> Full ())
  Load   :: Action (Int :-> Full Int)
  Save   :: Action (Int :-> Int :-> Full ())

input :: (Action :<: dom) => ASTF dom Int
input = inj Input

output :: (Action :<: dom) => ASTF dom Int -> ASTF dom ()
output a = inj Output :$ a

load :: (Action :<: dom) => ASTF dom Int -> ASTF dom Int
load a = inj Load :$ a

save :: (Action :<: dom) => ASTF dom Int -> ASTF dom Int -> ASTF dom ()
save a b = inj Save :$ a :$ b


-- cost logic

-- Cost definition

class Cost sym where
  costAlg :: sym sig -> Int

instance (Cost sub1, Cost sub2) => Cost (sub1 :+: sub2) where
  costAlg (InjL s) = costAlg s
  costAlg (InjR s) = costAlg s


-- fold
calcCost :: Cost s => AST s sig -> Int
calcCost = go
  where
    go :: Cost s => AST s sig -> Int
    go (Sym s)  = costAlg s
    go (s :$ a) = go s Prel.+ go a

instance Cost NUM where
  costAlg (Num _) = 0
  costAlg Add     = 1
  costAlg Sub     = 1
  costAlg Mul     = 1
  costAlg Pow     = 1

instance Cost Logic where
  costAlg (Bl _) = 0
  costAlg Not    = 1
  costAlg Eq     = 1
  costAlg Lt     = 1
  costAlg Lteq   = 1
  costAlg Gt     = 1
  costAlg Gteq   = 1
  costAlg And    = 1
  costAlg Or     = 1

instance Cost If where
  costAlg If = 1

instance Cost ExprComb where
  costAlg Pass = 0
  costAlg SemiColumn = 0
  costAlg Drop = 0
  costAlg While = 1

instance Cost Action where
  costAlg Input = 1
  costAlg Output = 1
  costAlg Load = 1
  costAlg Save = 1


-- | evaluation logic

-- EvalGame definition

class EvalGame sym where
  evalGame :: sym sig -> DenotationM GameM sig

instance (EvalGame sub1, EvalGame sub2) => EvalGame (sub1 :+: sub2) where
  evalGame (InjL s) = evalGame s
  evalGame (InjR s) = evalGame s


-- fold

evalGameDen :: (EvalGame s) => AST s sig -> DenotationM GameM sig
evalGameDen = go
  where
    go :: (EvalGame s) => AST s sig -> DenotationM GameM sig
    go (Sym s)  = evalGame s
    go (s :$ a) = go s $ go a

instance Eval NUM where
  evalSym (Num n) = n
  evalSym Add     = (Prel.+)
  evalSym Sub     = (Prel.-)
  evalSym Mul     = (Prel.*)
  evalSym Pow     = (Prel.^)

instance EvalGame NUM where
  evalGame (Num n) = return n
  evalGame Add     = \a b -> do
    x <- a
    y <- b
    return (x Prel.+ y)
  evalGame Sub     = \a b -> do
    x <- a
    y <- b
    return (x Prel.- y)
  evalGame Mul     = \a b -> do
    x <- a
    y <- b
    return (x Prel.* y)
  evalGame Pow     = \a b -> do
    x <- a
    y <- b
    return (x ^ y)

instance Eval Logic where
  evalSym (Bl b) = b
  evalSym Not    = Prel.not
  evalSym Eq     = (Prel.==)
  evalSym Lt     = (Prel.<)
  evalSym Lteq   = (Prel.<=)
  evalSym Gt     = (Prel.>)
  evalSym Gteq   = (Prel.>=)
  evalSym And    = (Prel.&&)
  evalSym Or     = (Prel.||)

instance EvalGame Logic where
  evalGame (Bl b) = return b
  evalGame Not = \a -> do
    x <- a
    return (Prel.not x)
  evalGame Eq     = \a b -> do
    x <- a
    y <- b
    return (x Prel.== y)
  evalGame Lt     = \a b -> do
    x <- a
    y <- b
    return (x Prel.< y)
  evalGame Lteq     = \a b -> do
    x <- a
    y <- b
    return (x Prel.<= y)
  evalGame Gt     = \a b -> do
    x <- a
    y <- b
    return (x Prel.> y)
  evalGame Gteq     = \a b -> do
    x <- a
    y <- b
    return (x Prel.>= y)
  evalGame And      = \a b -> do
    x <- a
    y <- b
    return (x Prel.&& y)
  evalGame Or      = \a b -> do
    x <- a
    y <- b
    return (x Prel.|| y)

instance Eval If where
  evalSym If = \c t f -> if c then t else f

instance EvalGame If where
  evalGame If = \c t f -> do
    c' <- c
    if c' then t
    else f

instance Eval ExprComb where
  evalSym _ = error "no symbolic evaluation for ExprComb"

instance EvalGame ExprComb where
  evalGame Pass = return ()
  evalGame SemiColumn = \a b -> do
    _ <- a
    _ <- b
    return ()
  evalGame Drop = \a -> do
    _ <- a
    return ()
  evalGame While = \c a -> do
    c' <- c
    if c' then do
      _ <- a
      evalGame While c a
    else return ()

instance Eval Action where
  evalSym _ = error "no symbolic evaluation for action"

instance EvalGame Action where
  evalGame Input = getInput
  evalGame Output = \x -> do
    n <- x
    putOutput n
  evalGame Load = \x -> do
    n <- x
    getLoad n
  evalGame Save = \x y -> do
    n <- x
    m <- y
    putSave n m

-- | combine EvalGame and Cost

class (EvalGame sym, Cost sym) => EvalGameCost sym where
  evalGameCost :: (Cost sym, EvalGame sym) => sym sig -> DenotationM GameM sig

instance (EvalGameCost sub1, EvalGameCost sub2) => EvalGameCost (sub1 :+: sub2) where
  evalGameCost (InjL s) = evalGameCost s
  evalGameCost (InjR s) = evalGameCost s


-- fold

evalGameCostDen :: (EvalGameCost s) => AST s sig -> DenotationM GameM sig
evalGameCostDen = go
  where
    go :: (EvalGameCost s) => AST s sig -> DenotationM GameM sig
    go (Sym s)  = evalGameCost s
    go (s :$ a) = go s $ go a

instance EvalGameCost NUM where
  evalGameCost (Num n) = do
    addCost (costAlg (Num n))
    evalGame (Num n)
  evalGameCost Add     = \a b -> do
    addCost (costAlg (Add))
    evalGame (Add) a b
  evalGameCost Sub     = \a b -> do
    addCost (costAlg (Add))
    evalGame (Sub) a b
  evalGameCost Mul     = \a b -> do
    addCost (costAlg (Mul))
    evalGame (Mul) a b
  evalGameCost Pow     = \a b -> do
    addCost (costAlg (Pow))
    evalGame (Pow) a b
