module Main where

import System.Environment
import Utils

import Levels.Level1 as L1
import Checks.Level1 as C1
import Levels.Level2 as L2
import Checks.Level2 as C2
import Levels.Level3 as L3
import Checks.Level3 as C3
import Levels.Level4 as L4
import Checks.Level4 as C4
import Levels.Level5 as L5
import Checks.Level5 as C5
import Levels.Level6 as L6
import Checks.Level6 as C6
import Levels.Level7 as L7
import Checks.Level7 as C7
import Levels.Level8 as L8
import Checks.Level8 as C8

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Start Running Level " ++ head args)
  case args of
    ("1":_) -> putStrLn (checkSolution L1.solution C1.testInput C1.calcSol)
    ("2":_) -> putStrLn (checkSolution L2.solution C2.testInput C2.calcSol)
    ("3":_) -> putStrLn (checkSolution L3.solution C3.testInput C3.calcSol)
    ("4":_) -> putStrLn (checkSolution L4.solution C4.testInput C4.calcSol)
    ("5":_) -> putStrLn (checkSolution L5.solution C5.testInput C5.calcSol)
    ("6":_) -> putStrLn (checkSolution L6.solution C6.testInput C6.calcSol)
    ("7":_) -> putStrLn (checkSolution L7.solution C7.testInput C7.calcSol)
    ("8":_) -> putStrLn (checkSolution L8.solution C8.testInput C8.calcSol)
    _ -> putStrLn "ERROR: Please enter a valid level"
  return ()
