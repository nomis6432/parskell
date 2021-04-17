module Checks.Level5 where

testInput :: [Int]
testInput = [1, 5, 12, 10, 8, 9, 0]

calcSol :: [Int] -> [Int]
calcSol [] = []
calcSol xs = fmap (^2) xs
