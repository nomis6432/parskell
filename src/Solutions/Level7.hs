module Levels.Level7 where

import Syntax.Syntax (expr, loop, pass, input, output, ($$), save, load, num, (+), (-) ,
  condition, (==), (<), (<=), (>), (>=), bool, not, while, (*), (**))
import Prelude (Bool (True, False))

{-
Now evaluate a polynomial of size 5. so if x0 x1, x2, x3, x4 are the first 5 elements
you need to return x0**0 + x**1 + x2 ** 2 + x3 **3 + x4 ** 4
new operators:
- **
You can assume that the size of the list is a multiple of 5
-}


solution = expr (
    loop (
      save (num 0) (num 0) $$
      save (num 1) (num 0) $$
      while (load (num 0) < num 5) (
        save (num 1) (input ** load (num 0) + load (num 1)) $$
        save (num 0) (load (num 0) + num 1)
      ) $$
      output (load (num 1))
    )
  )
