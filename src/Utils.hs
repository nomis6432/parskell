{-# LANGUAGE TypeOperators #-}

module Utils where

import Syntax.Expr
import Syntax.GameMonad
import Language.Syntactic
import Language.Syntactic.Functional
import Data.Map.Strict as Map


type Expr a = ASTF (NUM :+: Logic :+: If :+: ExprComb :+: Action) a

startRunGame expr inp = gameM (evalGameCostDen (drp expr :: Expr ())) inp [] Map.empty 0

checkSolution expr inp fun = case startRunGame expr inp of
  Failed mes -> "The execution failed with the following error :\n"
  Running (_, inp', out', _, _) -> "The game is still running.\n" ++
    "current output:\n " ++ show out' ++ "\ncurrent input:\n" ++ show inp' ++
    "\n the program stops when it tries to get input but none is left"
  Finished sol cost -> if sol Prelude.== realSol then
      "You got the correct output! Congratulations!\nInput:\n" ++ show inp ++
      "\noutput:\n" ++ show sol ++
      "\nthe size of your solution is:\n" ++ show (calcCost (drp expr :: Expr ())) ++
      "\nthe execution cost is:\n" ++ show cost
    else
      "The result is wrong. You got:\n" ++ show sol ++
      "\nbut the correct solution was:\n" ++ show realSol ++
      "\nthe input was:\n" ++ show inp
        where
          realSol = fun inp
