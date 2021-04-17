module Levels.Level3 where

import Syntax.Syntax (expr, loop, pass, input, output, ($$), save, load, num, (+), (-))
import Prelude ()

{-
Next write a DSL that adds 1 to every uneven element on an uneven position and subtracts
1 on every even position.
- -
- +
Note: Lists start at 0
-}

solution = expr (
    loop (
      output (input - num 1) $$
      output (input + num 1)
    )
  )
