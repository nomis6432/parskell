module Checks.Level7 where

testInput :: [Int]
testInput = [1, 5, 12, 10, 8, 9, 0, 2, 3, 6]

calcSol :: [Int] -> [Int]
calcSol [] = []
calcSol (x0:x1:x2:x3:x4:xs) = [x0^0 + x1^1 + x2^2 + x3^3 + x4^4] ++ calcSol xs
