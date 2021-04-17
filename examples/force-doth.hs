main :: IO ()
main = return ()

squareSum :: [Int] -> Int
squareSum = sum . map (^2)
