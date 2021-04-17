module Checks.Level8 where

testInput :: [Int]
testInput = [1, 2, 5, 4, 6, 5, 7, 8, 6, 8, 9 ,9, 10]

calcSol :: [Int] -> [Int]
calcSol [] = []
calcSol (x:xs) = x : calcSol' x xs

calcSol' :: Int -> [Int] -> [Int]
calcSol' _ [] = []
calcSol' last (x:xs) = if x >= last then x : calcSol' x xs else calcSol' last xs
