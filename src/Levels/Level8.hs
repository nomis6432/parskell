module Levels.Level8 where

import Syntax.Syntax
import Prelude ()

{-
You've seen all the operators now! Lets implement a sorting algorithm
with complexity O(n) called Stalin Sort.
Remove any element that is not in order. From small to big.
-}


solution = expr (
    loop (
      save (num 0) input $$
      output (load (num 0)) $$
      loop (
        save (num 1) input $$
        condition (load (num 1) >= load (num 0)) (
          output (load (num 1)) $$
          save (num 0) (load (num 1))
        ) pass
      )
    )
  )
