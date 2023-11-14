# Type System and Functional Programming

Hello, this is a repository containing **my** solved lab materials for Type System and Functional Programming course from Automatic Control and Computer Science, UPB.

Teacher: Mihnea Costin MURARU

Even if they are written by me does not mean that all the solves are original :P. I am learning.

## Resoureces

- [Learn you a Haskell](http://learnyouahaskell.com/chapters)
- [TSFP Labs](http://elf.cs.pub.ro/tsfp/labs/haskell-types)


## Explanations

At least for `Interpreter` part which is the main one and by far the hardest, I'll try to explain some things in the comments as I go thorough either already existing skelet code or as I write the code.

## Labs breakdown

- Lab 01: `IntroSkel`
  - Link: http://elf.cs.pub.ro/tsfp/labs/haskell-intro
  - Path: `IntroSkel/`
  - It is supposed to be a "quick" recap of Haskell language features / functional programming concepts in general.
  - Chapters 1-6, optionally 7 from [`Learn you a haskell`](http://learnyouahaskell.com/chapters) are recommended for this

- Lab 02: `TypesSkel`
  - Link: http://elf.cs.pub.ro/tsfp/labs/haskell-types
  - Path: `TypesSkel/`
  - An introduction to `types`, `polymorphism in Haskell`, `type classes`, `Functor`s
  - Chapter 8 from [`Learn you a haskell`](http://learnyouahaskell.com/chapters) is a **very** good reading for this

- Lab 03: `Interpreter`
  - Link: http://elf.cs.pub.ro/tsfp/labs/parser
  - Path: `Interpreter/src/Syntax/`, `Interpreter/src/Main.hs`
  - For now, `cd` into `Interpreter/`, `ghci Main.hs` and run `main`
  - This lab consisted of:
    - implementing parsers for the tokens (`[a-zA-Z]+` (words), `\`, `(`, `)`, `=`, `.`, `EOF`):
      - See [`Interpreter/src/Syntax/Parser.hs`](Interpreter/src/Syntax/Parser.hs)
    - defining `data Expression` to hold the different `Expression` types:
      - Variable: `x`
      - Lambda function: `\x.Expr`
      - Application: `(Expr Expr)`
      - Definition: `var=Expr` (can only occur at top level, see `parseProgram` definition)
      - See [`Interpreter/src/Syntax/Expression.hs`](Interpreter/src/Syntax/Expression.hs)
      - Instance `Show` for `Expression`
    - impement parser for the `Expression` types above:
      - See [`Interpreter/src/Syntax/Grammar.hs`](Interpreter/src/Syntax/Grammar.hs)

- Lab 04: `Textual Substitution`
  - Link: http://elf.cs.pub.ro/tsfp/labs/substitution
  - Path: [`Interpreter/src/Evaluation/Substitution.hs`](Interpreter/src/Evaluation/Substitution.hs)
  - This lab consisted of:
    - implementing the substitution rules for the Untyped Lambda Calculus interpreter
    - proper evaluation steps are coming in the next lab
  - You can `cd` into `Interpreter/src` and `ghci Evaluation/Substitution.hs` then test the substitution with the following cases:
    - `<\x.x>[y/x]`
      - Input: `subst "x" (Var "y") (Lambda "x" (Var "x"))`
      - Expected result: `\x.x`
    - `<\y.y>[y/x]`
      - Input: `subst "x" (Var "y") (Lambda "y" (Var "y"))`
      - Expected result: `\y#.y#` because the original `y` from `\y.y` gets replaced with `y#` before doing the substitution (that does not exist). Ideally, this should be optimized into outputting `\y.y`, but I don't see how to do that without hardcoding.
    - `<\y.(x y)>[(\x.x y)/x]`
      - Input: `subst "x" (Application (Lambda "x" (Var "x")) (Var "y")) (Lambda "y" (Application (Var "x") (Var "y")))`
      - Expected result: `\y#.((\x.x y) y#)`
