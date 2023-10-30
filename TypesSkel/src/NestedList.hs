module NestedList where

import Classes
import List ()

{-
    Define the 'NestedList' type, for handling lists with an arbitrary
    level of intricateness, which would be impossible to specify
    using plain Haskell lists. For instance, the String representation
    of such a nested list might be "[1, [2, 3], 4, 5]".

    Instantiate the following classes with the nested list type:
    * 'Show'
    * 'Functor'
    * 'Container'
    * 'Invertible'

    The inversion should be performed DEEPLY i.e., for the elements as well.
-}


data NestedList a = Nil | Value a | Nested [NestedList a]
-- Value 5
-- []       -> Nil
-- [1]      -> 1 :-: Nil
-- [1, 2]   -> 1 :-: 2 :-: Nil
-- [[]]     -> Nested (Nil)
-- [1, []]  -> Nested (1 :-: Nil)
-- [[], 1]  -> Nested ()
-- [[]]     -> Nested [Nil]
-- [[32]]   -> Nested [Nested [Elem 32]]
-- [[[]]]   -> Nested [Nested [Nil]]
-- []

-- [ [ [] ], 1, [[1],[]],3]
testList = Nested [Nested [Nil], Value 1, Nested [Nested [Value 2], Nil], Value 3]


instance Show a => Show (NestedList a) where
    show Nil = "[]"
    show (Value x) = show x
    show (Nested xs) = show xs


instance Functor NestedList where
    fmap _ Nil = Nil
    fmap f (Value x) = Value (f x)
    fmap f (Nested xs) = Nested (map (fmap f) xs)

instance Container NestedList where
    contents Nil = []
    contents (Value x) = [x]
    contents (Nested xs) = concatMap contents xs

instance Invertible a => Invertible (NestedList a) where
    invert Nil = Nil
    invert (Value x) = Value (invert x)
    invert (Nested xs) = Nested (invert xs)

-- infixr 5 :-:
-- infixr 5 :--:
-- data NestedList a = Nil | a :-: (NestedList a) | (NestedList a) :--: (NestedList a) ------------------------- Nested [NestedList a]

-- Varinata gresita, ceu Nil, Value a, Neste [NestedList a]

-- data NestedList a = Value a | Nested [NestedList a]
-- Varianta "buna"
-- []       -> Nested []
-- [1]      -> Nested [Value 1]
-- [1, 2]   -> Nested [Value 1, Value 2]
-- [[]]     -> Nested [Nested []]
-- [1, []]  -> Nested [Value, Nested []]
-- [[], 1]  -> Nested [Nested [], Value 1]
-- [[], []] -> Nested [Nested [], Nested []]


-- []       -> Nil
-- [1]      -> 1 :-: Nil
-- [1, 2]   -> 1 :-: 2 :-: Nil
-- [[]]     -> Nested (Nil)
-- [[], []] -> Nested ()
