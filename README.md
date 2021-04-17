# parskell

An embeded DSL in Haskell inspired by the game [Human Resource Machine](https://en.wikipedia.org/wiki/Human_Resource_Machine).
Write a DSL with a limited amount of instructions that takes an input list and produces an output list.

The EDSL is made using [Syntactic](http://hackage.haskell.org/package/syntactic-3.8.2) and the paper [A Generic Abstract Syntax Model for Embedded Languages](https://emilaxelsson.github.io/documents/axelsson2012generic.pdf).

## instructions
To run the DSL you'll need `stack`. The Levels are located in the `src/Levels` folder. Start with `Level1.hs` and read the top comment.
To test your result run `stack run <x>` where you replace `<x>` with the level. The solutions can be found in the `src/Solutions` folder.


## operators
To explain the different operators we'll use x, y, z,... to denote the parameters and (x::Type) to denote the Type the expressions evaluates to. When the Type is generic we'll use a, b,... . When a Haskell literal is expected we'll use n e.g. 1, True,...


### numerical
* num n :: Int  
represents a literal of type Int where n is the value e.g. 2
* (x::Int) + (y::Int) :: Int  
adds x and y together
* (x::Int) - (y::Int) :: Int  
subtracts y from x
* (x::Int) * (y::Int) :: Int  
multiplies x and y together
* (x::Int) ** (y::Int) :: Int  
raises a to the power of b

### Logic
* (bool n) :: Bool  
represents a literal of type Bool where n is the value e.g. True
* not (x:Bool) :: Bool  
negates the boolean x
* (x::Int) == (y::Int) :: Bool  
returns whether or not x is equal to y
* (x::Int) < (y::Int) :: Bool  
returns whether or not x is less than y
* (x::Int) <= (y::Int) :: Bool  
returns whether or not x is less than or equal to y
* (x::Int) > (y::Int) :: Bool  
returns whether or not x is greater than y
* (x::Int) >= (y::Int) :: Bool  
returns whether or not x is greater than or equal to y
* (x::Bool) && (y::Bool) :: Bool  
returns whether or not both x and y are True
* (x::Bool) && (y::Bool) :: Bool  
returns whether or not x and/or y are True

### Conditions
* condition (x::Bool) (y::a) (z::a) :: a  
executes y if x is True. Otherwise execute z.

### Combinators
* pass :: ()  
does nothing. Can be used as placeholder or for empty else condition.
* (x::a) $$ (y::b) :: ()  
first execute a and then b
* while (x::Bool) (y::a) :: ()  
executes y while x is True
* loop (x::a) :: ()
keeps executing x. Equivalent to a while (bool True) loop

### Enviroment
* input :: Int  
consumes a value from the input list and returns it. If there are no values left, the program will terminate.
* output (x::Int) :: ()  
places an integer in the output list.
* save (x::Int) (y::Int) :: ()  
saves the value y with as key x.
* load (x::Int) :: Int  
loads the value with as key x.
