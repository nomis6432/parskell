module Levels.Level1 where

import Syntax.Syntax (expr, loop, pass, input, output)
import Prelude (Bool (True, False))

{-
To get things started write a DSL that takes all the inputs and puts them in the output
Some operators that you'll need:
- loop
- pass
- input
- output
check the README for a description. expr just denotes the type. Just write your
code where the loop is located
-}


solution = expr (
    loop (
      output (input)
    )
  )
