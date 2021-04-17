{-# LANGUAGE TypeOperators #-}

import Syntax.Types


main::IO ()
main = return ()

-- veelterm
calc :: [Int] -> Int
calc xs = sum (zipWith (^) xs [0, 1..])

calcExpr :: Expr ((LitE [Int]):+: Func3E)
calcExpr = func1E (litE sum) (func3E (litE zipWith) (litE (^)) (varE "xs") (litE [0, 1..]))
