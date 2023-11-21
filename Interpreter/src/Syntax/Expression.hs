module Syntax.Expression where

data Expression = Var String
                | Lambda String Expression
                | Application Expression Expression
                | Definition String Expression
                deriving Eq

instance Show Expression where
    -- show :: Expression -> String
    show expr = case expr of
        Var var -> var
        Lambda var expr -> "\\" ++ var ++ "." ++ show expr
        Application left right -> "(" ++ show left ++ " " ++ show right ++ ")"
        Definition var expr -> var ++ "=" ++ show expr

newtype Program = Expressions [Expression] deriving (Show)
