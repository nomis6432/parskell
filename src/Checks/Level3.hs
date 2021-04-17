module Checks.Level3 where

testInput :: [Int]
testInput = [1, 2, 3, 4, 10, 11, 12, 14]

calcSol :: [Int] -> [Int]
calcSol [] = []
calcSol (a:b:xs) = [a - 1, b + 1] ++ calcSol xs
calcSol _ = error "uneven list"
