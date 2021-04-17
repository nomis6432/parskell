module OldStuff.Parser.ParserMonad where

import Control.Monad.State
import Control.Monad
import Control.Applicative
import Parser.ParserMessages
import Text.ParserCombinators.Parsec.Pos
import Text.Parsec

type Bracket = (SourcePos, Char)

type MyParser a = ParsecT String () ParserMonad a

newtype ParserMonad a = ParserMonad {parseM::SourcePos -> [Bracket] -> Either ParserError (a, SourcePos, [Bracket])}

instance Functor ParserMonad where
  fmap = liftM

instance Applicative ParserMonad where
  pure  x = ParserMonad (\pos brackets -> Right (x, pos, brackets))
  (<*>) = ap

instance Monad ParserMonad where
  p >>= f =
    ParserMonad (\pos brackets ->
      case parseM p pos brackets of
        Left x -> Left x
        Right (a, pos', brackets') ->
          parseM (f a) pos' brackets')
  return = pure

openBracket :: Char -> ParserMonad ()
openBracket c = ParserMonad (\pos brackets ->
  Right ((), pos, (pos, c):brackets))

closeBracket :: Char -> ParserMonad ()
closeBracket c = ParserMonad(\pos brackets ->
  case brackets of
    [] -> Left (ParserUnexpectedChar pos c)
    ((startpos, cbracket):rbrackets) -> if bracketsMatch cbracket c
      then Right ((), pos, rbrackets)
      else Left (ParserMismatchingBrackets startpos cbracket pos c))

bracketsMatch :: Char -> Char -> Bool
bracketsMatch '(' ')' = True
bracketsMatch '[' ']' = True
bracketsMatch  _   _  = False
