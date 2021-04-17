module Checks.Level4 where

testInput :: [Int]
testInput = [1, 5, 12, 10, 8, 9]

calcSol :: [Int] -> [Int]
calcSol [] = []
calcSol (x:xs) = (if (x < 10) then [x] else []) ++ calcSol xs
