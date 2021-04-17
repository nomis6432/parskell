module OldStuff.Parser.Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec

import Data.Char

import Parser.ParserMonad
import CommonTypes
import Syntax.Types

-- | lexer
{-
parseExpression :: String -> ParserMonad (Expr a)
parseExpression input@(first:rest)
  -- parse Name
  | isLower first = undefined
  -- parse operator
  | isOperator input = undefined
  -- parse Numner
  | isNumber fist = undefined
  -- parse String
  | first == '\"' = undefined
  -- parse char
  | fist == '\'' = undefined
-}

isOperator :: String -> Bool
isOperator (x:y:xs) = x == '(' || isSymbol y


-- | parsers

parseName :: Parser String
parseName = do
  x <- lower
  xs <- many alphaNum
  return (x:xs)

parseInt :: Parser Int
parseInt = do
  ns <- many digit
  space
  return (read ns :: Int)

parseFloat :: Parser Float
parseFloat = do
  ns <- many digit
  char '.'
  ms <- many digit
  return (read (ns ++ "." ++ ms) :: Float)

-- todo escape character
parseString :: Parser String
parseString = do
  char '\''
  xs <- many $ noneOf ['\'']
  char '\''
  return xs

-- todo escape character
parseChar :: Parser Char
parseChar = do
  char '\''
  xs <- noneOf ['\'']
  char '\''
  return xs
