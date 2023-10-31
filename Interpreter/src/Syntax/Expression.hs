module Syntax.Expression where

{-
    TODO: implement proper Show
-}
data Expression = Var String
                | Lambda String Expression
                | Application Expression Expression
                | Definition String Expression deriving Show

newtype Program = Expressions [Expression] deriving (Show)
