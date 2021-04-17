module Levels.Level4 where

import Syntax.Syntax (expr, loop, pass, input, output, ($$), save, load, num, (+), (-) ,
  condition, (==), (<), (<=), (>), (>=), bool)
import Prelude ()

{-
Next write a DSL that only returns elements that are smaller than 10. New operators:
- condition
- ==
- <
- <=
- >
- >=
Note: You can use pass if for the else block of the condition if you dont need it
you also shouldn't put a $$ at the end of your last expression in your condition
-}


solution = expr (
    loop (
      save (num 0) input $$
      condition (load (num 0) < num 10) (
        output (load (num 0))
      ) (
        pass
      )
    )
  )
