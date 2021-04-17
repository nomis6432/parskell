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
      save (num 0) input $$
      save (num 1) (load (num 0)) $$
      save (num 2) (num 0) $$
      while (not (load (num 0) == num 0)) (
        save (num 0) (load (num 0) - num 1) $$
        save (num 2) (load (num 1) + load (num 2))
      ) $$
      output (load (num 2))
    )
  )
