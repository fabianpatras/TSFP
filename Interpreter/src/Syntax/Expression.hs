module Syntax.Expression where

data Expression = Var String
                | Lambda String Expression
                | Application Expression Expression deriving Show

data TopLvlExpression = Definition Expression Expression
                      | NormalExpression Expression deriving Show

newtype Program = ListOfTopLvlExpressions [TopLvlExpression] deriving (Show)
