{-# LANGUAGE TypeOperators #-}

module Syntax.Syntax (
  -- operators
  -- Syntax.Expr.NUM
    Syntax.Expr.num
  , (Syntax.Expr.+)
  , (Syntax.Expr.-)
  , (Syntax.Expr.*)
  , (Syntax.Expr.**)
  -- Syntax.Expr.Logic
  , Syntax.Expr.bool
  , Syntax.Expr.not
  , (Syntax.Expr.==)
  , (Syntax.Expr.<)
  , (Syntax.Expr.<=)
  , (Syntax.Expr.>)
  , (Syntax.Expr.>=)
  , (Syntax.Expr.&&)
  , (Syntax.Expr.||)
  -- Syntax.Expr.If
  , Syntax.Expr.condition
  -- Syntax.Expr.ExprComb
  , Syntax.Expr.pass
  , (Syntax.Expr.$$)
  , Syntax.Expr.while
  , Syntax.Expr.loop
  -- Syntax.Expr.Action
  , Syntax.Expr.input
  , Syntax.Expr.output
  , Syntax.Expr.load
  , Syntax.Expr.save
  -- helper
  , expr
)where

import Syntax.Expr
import Syntax.GameMonad
import Language.Syntactic
import Language.Syntactic.Functional
import Data.Map.Strict as Map

expr :: ASTF (NUM :+: Logic :+: If :+: ExprComb :+: Action) a -> ASTF (NUM :+: Logic :+: If :+: ExprComb :+: Action) a
expr = id
