module Chapter8
    ( Point(..)
    , Shape(..)
    , surface
    , baseCircle
    , baseRect
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

-- point, radius
-- point, point
{-
    data Shape = Circle Float Float Float | Rectangle Float Float Float Float    deriving (Show)

    surface :: Shape -> Float

    surface (Circle _ _ r) = 2 * pi * r
    surface (Rectangle x1 y1 x2 y2) = abs $ (x1 - x2) * (y1 - y2)
-}

-- primul `Point` e "(Data) Type"
-- al doilea `point` e Value Constructor care e o functie cu tipul
-- Point :: Float -> Float -> Point
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving Show

surface :: Shape -> Float

surface (Circle _  r) = 2 * pi * r
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs $ (x1 - x2) * (y1 - y2)

-- map (Circle (Point 10 20)) [1,2,3]

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Point -> Shape
baseRect = Rectangle (Point 0 0)



------- Person
-- data Person = Person String String Int Float String String deriving (Show)

-- data Person = Person { firstName :: String
--                      , lastName :: String
--                      , age :: Int
--                      , height :: Float
--                      , phoneNumber :: String
--                      , flavor :: String
--                      } deriving (Show)

-- `Maybe' a` e un "(Data) Type Constructor"
-- `Maybe' Char` e un "(Data) Type" cu adevarat
-- `Nothing'` si `Just' a` sunt "Value constructors"
data Maybe' a = Nothing' | Just' a deriving (Show)



--------- Vector
data Vector a = Vector a a a deriving (Show)
vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)


-- ~~~~~~~~ Derived instances ~~~~~~~~

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)



data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)


-- ~~~~~~~~ Type synonyms ~~~~~~~~

-- we're just making a synonym for an already existing type. !!!
type String' = [Char]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

phoneBook :: PhoneBook
phoneBook = [("Ionut", "076..."), ("Andreea", "0765...")]

-- `k` si `v` sunt tipuri.
-- Numim o lista de tipul [(k, v)] -> `AssocList k v`
type AssocList k v = [(k, v)]


-- type IntMap v = Map Int v

-- `Map` takes 2 types as input, but we can ommit the second when we do this because `IntMap` also has a type parameter.
type IntMap = Map Int

func :: IntMap Char -> Bool
func x = True


data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)


lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "The combination " ++ show lockerNumber ++ " is not available for use!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Loker " ++ show lockerNumber ++ " is taken!!!!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]


-- ~~~~~~~~ Recursive data structures ~~~~~~~~

-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)

-- `infixr` binds to the right with precedence `5`
-- which is weaker than `+` or `*`
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5  .++
(.++) :: List a -> List a -> List a
Empty .++ xs = xs
(x :-: xs) .++ ys = x :-: (xs .++ ys)

l1 = 1 :-: 2 :-: 32 :-: Empty
l2 = 55 :-: Empty


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = Node x EmptyTree EmptyTree
treeInsert x (Node y left right)
    | x == y = Node y left right                -- we don't insert
    | x < y  = Node y (treeInsert x left) right -- insert the number into the left subtree
    | x > y  = Node y left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node y left right)
    | x == y = True
    | x < y = treeElem x left
    | x > y = treeElem x right


bigTree :: Tree Int
bigTree = foldr treeInsert EmptyTree [8,3,7,5,6,1,9,0]

-- ~~~~~~~~ Typeclasses 102 ~~~~~~~~

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light - No Go"
    show Yellow = "Yellow light - Slow down"
    show Green = "Green light - You Go Go Go!"


class YesNo a where
    yesno :: a -> Bool

-- instance YesNo String where
--     yesno "" = False
--     yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance (YesNo a) => YesNo (Maybe a) where
    yesno (Just x) = yesno x
    yesno Nothing = False

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

-- ~~~~~~~~ The Functor typeclass ~~~~~~~~

-- the real implementation, i guess
-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

-- `f` from above is a type constructor that takes one type parameter.
-- `Maybe` is a `Type Constructor` that takes one type as a parameter (`Maybe a`)

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

{-
    `Functor` e un typeclass (interfata care spune behaviour) pentru
    `Type Constructor`s (Tipuri de date parametrizabile) care defineste
    `fmap`, o functie care mapeaza din `Type Constructor` a in `Type Constructor` b

    Caz concret, mapeaza [a] -> [b], adica [Int] -> [Char]
    sau Maybe a -> Maybe b, adica Maybe Int -> Maybe Bool
    SAAAAU MapInt a -> MapInt b, adica Map Int a -> Map Int b, adica Map Int Strin -> Map Int Bool SAU Map Int Strin -> Map Int String
-}

{-
    Fun fact: putem sa "implementam" Functor si pentru `Type Construtor`s care iau 2 tipuri ca input.
    Facem asta prin a da ca input deja un parametru specificat, adica:

    ```
    instance Functor (Either a) where
        fmap f (Right x) = Right (f x)
        fmap f (Left x) = Left x
    ```

    Aici, `Either a` e un `Type Constructor` care ia un tip si intoarce `Either a <ceva>` ala
-}

class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

instance (Ord k) => Functor' (Map k) where
    fmap' f input_map = Map.fromList $ map (\(key, v) -> (key, f v)) $ Map.toList input_map
