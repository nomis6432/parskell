{-# LANGUAGE TypeOperators #-}

module Parse where

import Text.ParserCombinators.Parsec
import Text.Parsec
import Text.Parsec.Combinator
import Language.Haskell.TH

parseExp  :: String -> Either ParseError (Q Exp)
parseExp = undefined

test :: String -> IO ()
test s = do
  print $ myPrint list
  return () where
    list = parse parseList "" s

myPrint :: Either ParseError Exp -> String
myPrint (Left e) = show e
myPrint (Right e) = show $ pprExp 0 e

test2 :: IO ()
test2 = do
  print $ parse parseList "" "[1, 2, 3, 4]"
  return ()

parseList :: Parser Exp
parseList = do
  spaces
  char '['
  xs <- commaSep parseInt
  spaces
  char ']'
  return (ListE xs)



parseInt :: Parser Exp
parseInt = do
  val <- many1 digit
  return (LitE (IntPrimL (read val :: Integer)))

commaSep :: Parser a -> Parser [a]
commaSep p = do
  first <- p
  next <- many (comma >> p)
  return (first : next)

comma :: Parser ()
comma = spaces >> char ',' >> spaces
