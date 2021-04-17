module Checks.Level6 where

testInput :: [Int]
testInput = [1, 5, 12, 10, 8, 9, 0]

calcSol :: [Int] -> [Int]
calcSol [] = []
calcSol xs = fmap (\x -> x^x) xs
