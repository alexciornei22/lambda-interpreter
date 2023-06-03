# Haskell lambda calculus interpreter
To use the interpreter you must have GHC installed on your machine. Instructions on how to install can be found [here](https://www.haskell.org/ghcup/).
To run the intrepreter use the following command:
```
$ runhaskell ./main.hs
```
Exiting is done with ``exit`` command.

The interpreter will evaluate any lambda expression and output the **beta normal form** of that expression.
When writing an expression in the interpreter, replace the 'λ' character with '\\': ``λx.x -> \x.x``

# Macros
The interpreter allows the user to create macros, with the following syntax:

**\<Macro\> = \<Expression\>**
  
The macro can then be used in the interpreter by adding a ``$`` character in front (ex. ``$test``)

# Details
The program is organized in several files:
- [Expr.hs](/Expr.hs) - contains the definition of the lambda expression ``Expr`` and code line ``Code`` data types
- [Lambda.hs](/Lambda.hs) - functions used for reducing expressions (using *normal* or *applicative* order) and evaluating macros
- [Parser.hs](/Parser.hs) - the implementation of the Parser Monad, used for converting strings from the interpreter input into
the data types used by the program
- [main.hs](/main.hs) - the program entrypoint
