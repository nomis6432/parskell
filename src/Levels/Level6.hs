module Levels.Level6 where

import Syntax.Syntax (expr, loop, pass, input, output, ($$), save, load, num, (+), (-) ,
  condition, (==), (<), (<=), (>), (>=), bool, not, while, (*))
import Prelude (Bool (True, False))

{-
Now square all values by themselves. You can now use multiplication.
New operators:
- *
This shouldn't be very different from the previous level
-}


solution = expr (
    loop (
      pass
    )
  )
