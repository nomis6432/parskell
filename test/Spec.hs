{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

import Syntax.Syntax
import qualified Prelude as Prel
import Language.Syntactic.Syntax
import Syntax.Expr
import Utils

main :: Prel.IO ()
main = Prel.putStrLn "Test suite not yet implemented"


ex1 :: (NUM :<: dom, Logic :<: dom, If :<: dom, ExprComb :<: dom, Action :<: dom) => ASTF dom Prel.Bool
ex1 = (num 0) > (num 0)

ex2 :: (NUM :<: dom) => ASTF dom Prel.Int
ex2 = (num 5 + num 0) * num 6

ex3 :: (NUM :<: dom, Logic :<: dom) => ASTF dom Prel.Bool
ex3 = ex2 == num 30

ex4 :: (NUM :<: dom, Logic :<: dom, If :<: dom) => ASTF dom Prel.Int
ex4 = condition ex3 ex2 (num 2 ** num 2)

ex5 :: (NUM :<: dom, Logic :<: dom, If :<: dom, ExprComb :<: dom, Action :<: dom) => ASTF dom ()
ex5 = save (num 0) (num 0) $$
  while ((load (num 0)) < (num 5)) (
    save (num 0) (load (num 0) + (num 1)) $$
    input ) $$
  output (load (num 0))
