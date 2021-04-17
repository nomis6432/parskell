module Checks.Level2 where

testInput :: [Int]
testInput = [1, 2, 3, 4, 5, 6, 7, 8]

calcSol :: [Int] -> [Int]
calcSol [] = []
calcSol (a:b:xs) = [b, a] ++ calcSol xs
calcSol _ = error "uneven list"
