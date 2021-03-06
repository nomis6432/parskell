module Levels.Level5 where

import Syntax.Syntax (expr, loop, pass, input, output, ($$), save, load, num, (+), (-) ,
  condition, (==), (<), (<=), (>), (>=), bool, not, while)
import Prelude (Bool (True, False))

{-
Now square all the values by 2. You don't get to use multiplication (yet)
new operator :
- not
- while
Tip you'll need a lot more variables now
-}


solution = expr (
    loop (
      pass $$
      while (bool True) (
        pass
      ) $$
      pass
    )
  )
