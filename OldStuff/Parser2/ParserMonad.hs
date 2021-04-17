module OldStuff.Parser2.ParserMonad where

import Text.Parsec
import Parser2.ParserOptions
import Parser2.ParserMessages
import Control.Monad
-- import Control.Monad.Trans.
import Control.Monad.Trans

type Bracket = (SourcePos, Char)

type MyParser a = ParsecT String () (ReaderT () (Either ParserError)) a

newtype ParserState a = ParserState {parseState :: ParserOptions -> [Bracket] -> Either ParserError (a, ParserOptions, [Bracket])}

instance Functor ParserState where
  fmap = liftM

instance Applicative ParserState where
  pure x = ParserState (\ops brackets -> Right (x, ops, brackets))
  (<*>) = ap

instance Monad ParserState where
  p >>= f =
    ParserState (\ops brackets ->
      case parseState p ops brackets of
        Left x -> Left x
        Right (a, ops', brackets') ->
          parseState (f a) ops' brackets')
  return = pure



-- ExceptionErrorManaDtransformer
-- EnviromentOfReaderMonadTransformer
