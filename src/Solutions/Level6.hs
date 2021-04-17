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
      save (num 0) input $$
      save (num 1) (load (num 0)) $$
      save (num 2) (num 1) $$
      while (not (load (num 0) == num 0))(
        save (num 0) (load (num 0) - num 1) $$
        save (num 2) (load (num 1) * load (num 2))
      ) $$
      output (load (num 2))
    )
  )
