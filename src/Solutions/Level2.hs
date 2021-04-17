module Levels.Level2 where

import Syntax.Syntax (expr, loop, pass, input, output, ($$), save, load, num)
import Prelude (Bool (True, False))

{-
Next write a DSL that switches the first and second element of each list
Some new operators that you'll need:
- $$
- save
- load
- num
Hint: Don't put a $$ after the last line
-}


solution = expr (
    loop (
      save (num 0) input $$
      output input $$
      output (load (num 0))
    )
  )
