module Chapters1_6 where

factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x - 1)

fibb :: Integer -> Integer
fibb 0 = 0
fibb 1 = 1
fibb n = fibb (n - 1) + fibb (n - 2)


capital :: String -> String
capital "" = "Empty string, you stupid!!"
capital word@(x:xs) = "The first letter of " ++ word ++ " is " ++ [x]

lessThan5 :: Integer -> String
lessThan5 number
    | number < 5 = "Yes, " ++ (show number) ++ " is less than 5"
    | number < 25 = "No, but less than 25"
    | otherwise = "You stupid cunt!"

max' :: (Ord a) => a -> a -> a
max' x y
    | x > y = x
    | otherwise = y


myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a < b = LT
    | a == b = EQ
    | otherwise = GT

tellBmi :: (RealFloat a) => a -> a -> String
tellBmi weight height
    | bmi < 18.0 = "Skinny bitch!"
    | otherwise = "Fat fuck!"
    where bmi = weight / height ^ 2

cylinderArea :: (RealFloat a) => a -> a -> a
cylinderArea r h =
    let sideArea = 2 * pi * r * h
        topSide = pi * r ^ 2
    in sideArea + 2 * topSide

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty list, bitch"
                                               [x] -> "only one element, you cunt!"
                                               xs -> " has more elements!!!"

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' 0 _ = []
replicate' x y = y:replicate' (x - 1) y

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)

