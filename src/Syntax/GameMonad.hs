module Syntax.GameMonad where

import Control.Monad
import Control.Applicative

import Data.Map.Strict as Map


-- | The different states of the game
--   Finished : succesfully finished executing. [Int] is result list and tInt the cost
--   Failed : Something went wrong. String explains error, Int
--   Running : game is running
data GameState a = Finished [Int] Int | Failed String | Running (a, [Int], [Int], Map.Map Int Int, Int)
  deriving (Show)

newtype GameM a = GameM {gameM:: [Int] -> [Int] -> Map.Map Int Int -> Int -> GameState a}

instance Functor GameM where
  fmap = liftM
instance Applicative GameM where
  pure x = GameM (\inp out vars cost -> Running (x, inp, out, vars, cost))
  (<*>) = ap

instance Monad GameM where
  p >>= f  =
    GameM (\inp out vars cost ->
      case gameM p inp out vars cost of
        Failed x -> Failed x
        Finished x cost -> Finished x cost
        Running (res, inp', out', vars', cost') ->
          gameM (f res) inp' out' vars' cost')
  return = pure

-- returns a value from the input
getInput :: GameM Int
getInput = GameM (\inp out vars cost ->
  case inp of
    (i:inp') -> Running (i, inp', out, vars, cost)
    _        -> Finished (reverse out) cost)

-- puts a value in the output
putOutput :: Int -> GameM ()
putOutput x = GameM (\inp out vars cost ->
  Running ((), inp, x:out, vars, cost))

-- loads a value with a certain index
getLoad :: Int -> GameM Int
getLoad x = GameM (\inp out vars cost ->
  case Map.lookup x vars of
    Just y -> Running (y, inp, out, vars, cost)
    Nothing -> Failed ("no value with index " ++ show x))

-- stores a value to a certain index
putSave :: Int -> Int -> GameM ()
putSave x y = GameM (\inp out vars cost ->
  Running ((), inp, out, Map.insert x y vars, cost))

-- add cost
addCost :: Int -> GameM ()
addCost x = GameM (\inp out vars cost ->
  Running ((), inp, out, vars, cost + x))
