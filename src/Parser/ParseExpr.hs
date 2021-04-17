{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Parser.ParseExpr where

import Syntax.Expr
import Language.Syntactic.Functional
import Language.Syntactic

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec

{-
This shows that it is possible to parse these expressions with Parsec. This could
be used to create a more readible DSL than the EDSL.
-}

-- | numbers

parseNum :: (NUM :<: dom) => Parser (ASTF dom Int)
parseNum = do
  ns <- many digit
  return (num (read ns :: Int))

parseAdd :: (NUM :<: dom) => Parser (ASTF dom Int -> ASTF dom Int -> ASTF dom Int)
parseAdd = do
  char '+'
  return (Syntax.Expr.+)

parseMin :: (NUM :<: dom) => Parser (ASTF dom Int -> ASTF dom Int -> ASTF dom Int)
parseMin = do
  char '-'
  return (Syntax.Expr.-)

parseMul :: (NUM :<: dom) => Parser (ASTF dom Int -> ASTF dom Int -> ASTF dom Int)
parseMul = do
  char '*'
  return (Syntax.Expr.*)

parsePow :: (NUM :<: dom) => Parser (ASTF dom Int -> ASTF dom Int -> ASTF dom Int)
parsePow = do
  char '^'
  return (Syntax.Expr.**)

-- | logic
parseNot :: (Logic :<: dom) => Parser (ASTF dom Bool -> ASTF dom Bool)
parseNot = do
  char '!'
  return Syntax.Expr.not

parseEq :: (Logic :<: dom, Eq a) => Parser (ASTF dom a -> ASTF dom a -> ASTF dom Bool)
parseEq = do
  char '='
  return (Syntax.Expr.==)

parseLt :: (Logic :<: dom, Ord a) => Parser (ASTF dom a -> ASTF dom a -> ASTF dom Bool)
parseLt = do
  char '<'
  return (Syntax.Expr.<)

parseLteq :: (Logic :<: dom, Ord a) => Parser (ASTF dom a -> ASTF dom a -> ASTF dom Bool)
parseLteq = do
  char '<'
  char '='
  return (Syntax.Expr.<=)

parseGt :: (Logic :<: dom, Ord a) => Parser (ASTF dom a -> ASTF dom a -> ASTF dom Bool)
parseGt = do
  char '>'
  return (Syntax.Expr.>)

parseGteq :: (Logic :<: dom, Ord a) => Parser (ASTF dom a -> ASTF dom a -> ASTF dom Bool)
parseGteq = do
  char '>'
  char '='
  return (Syntax.Expr.>=)

-- | conditions

parseCondition :: (If :<: dom) => Parser (ASTF dom Bool -> ASTF dom a -> ASTF dom a -> ASTF dom a)
parseCondition = do
  char 'i'
  char 'f'
  return condition

ex1 :: (NUM :<: dom) => (ASTF dom Int)
ex1 = case parse parseNum "" "123" of
  Left _ -> num 0
  Right x -> x
