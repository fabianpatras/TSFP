{-# LANGUAGE ScopedTypeVariables #-}

module Tree where

import Classes

{-
    The binary tree implementation from
    'http://learnyouahaskell.com/making-our-own-types-and-typeclasses'.

    Instantiate the following classes with the tree type:
    * 'Show'
    * 'Container'
    * 'Invertible'

    A possible String representation of a binary tree such as

                    4                       4
                   / \                          2
                  2   5     might be                1
                 / \                                3
                1   3                           5

    The inversion should be performed DEEPLY i.e., for the elements as well.
-}

data Tree a
    = EmptyTree
    | Node a (Tree a) (Tree a)
    deriving (Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

treeFromList :: (Ord a) => [a] -> Tree a
treeFromList = foldr treeInsert EmptyTree

instance Functor Tree where
    fmap _ EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x)
                                            (fmap f leftsub)
                                            (fmap f rightsub)


                    -- show        indent   in     out
identPrint :: Show a => Int -> a -> String
identPrint 0 x = show x ++ "\n"
identPrint n x = (foldr (++) [] $ take n $ repeat " ") ++ (show x) ++ "\n"

identShow :: Show t => Tree t -> Int -> [String]
identShow EmptyTree _ = []
identShow (Node elem left right) n = (identShow left (n + 1)) ++ [identPrint n elem] ++ (identShow right (n + 1))

instance Show t => Show (Tree t) where
    show tree = foldl (++) [] $ identShow tree 0


myTree = Node 4 (Node 2 (Node 1 EmptyTree EmptyTree ) (Node 3 EmptyTree EmptyTree )) (Node 5 EmptyTree EmptyTree)

instance Container Tree where
    contents EmptyTree = []
    contents (Node elem left right) = (contents left) ++ [elem] ++ (contents right)

instance Invertible t => Invertible (Tree t) where
    invert EmptyTree = EmptyTree
    invert (Node elem left right) = Node (invert elem) (invert right) (invert left)

myTreeBool = Node False (Node False EmptyTree EmptyTree) (Node True (Node False EmptyTree EmptyTree) EmptyTree)

