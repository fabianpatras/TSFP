module Basics where  -- (10p)

{-
    1. (1p)
    Implement the 'reverse' function, which returns a given list
    in reverse order, using explicit recursion.
    Do NOT employ any higher-order functions.
-}

reverseRec1 :: [a] -> [a]
reverseRec1 [] = []
reverseRec1 (x:xs) = reverseRec1 xs ++ [x]

{-
    2. (1p)
    Same as (1), but change the direction on which the output is built.
    For example, if (1) built the list on return from recursion,
    you should now built the list when recursing forward.
-}

helperRec :: [a] -> [a] -> [a]

-- helperRec [] [] = []
-- helperRec acc [] = acc
-- helperRec acc [x] = acc ++ [x]
-- helperRec acc (x:xs) = helperRec acc xs ++ [x]


-- Construieste argumentul cu care se apeleaza recursiv functia.
-- Asa facem tail optimisation I guess.
helperRec acc [] = acc
helperRec acc (x:xs) = helperRec (x:acc) xs

reverseRec2 :: [a] -> [a]
reverseRec2 = helperRec []

{-
    3. (1.5p)
    Same as (1), but use a higher-order function instead.
    Do NOT employ explicit recursion.
    Make sure that your solution faithfully reflects the process from (1).
-}

reverseHO1 :: [a] -> [a]
reverseHO1 = foldr (\x acc -> acc ++ [x]) []

{-
    4. (1.5p)
    Same as (2), but use a higher-order function instead.
    Do NOT employ explicit recursion.
    Make sure that your solution faithfully reflects the process from (2).
-}

reverseHO2 :: [a] -> [a]
reverseHO2 = foldl (\acc x -> x:acc) []

-- sau si mai smeker
-- reverseHO2 = foldl (flip (:)) []


{-
    5. (1p)
    Implement the power set function, which returns the set of all subsets
    of a set, using explicit recursion.
-}


powerSetHelper :: a -> [[a]] -> [[a]]
powerSetHelper y xs = [y : x | x <- xs] ++ xs

powerSetRec :: [a] -> [[a]]
powerSetRec [] = [[]]
powerSetRec (h : t) = powerSetHelper h (powerSetRec t)

-- powerSetRec [] = [[]]
-- powerSetRec (x:[]) = [x] : powerSetRec []
-- powerSetRec (x:y:[]) = 
-- powerSetRec (x:xs) = 

{-
    6. (1.5p)
    Same as (5), but use higher-order functions instead.
    Do NOT employ explicit recursion.
    Make sure that your solution faithfully reflects the process from (5).
-}

powerSetHO :: [a] -> [[a]]
powerSetHO = foldr powerSetHelper [[]]

{-
    7. (0.5p)
    Compute the cartesian product of two lists, using list comprehensions.
-}

cartesian2 :: [a] -> [b] -> [(a, b)]
cartesian2 xs ys = [(x,y) | x <- xs, y <- ys]

{-
    8. (2p)
    Similar to (7), but extend to any number of lists.
-}


-- Problema subrecursiva e ca am o lista, primul elemint din lista de liste
-- 
cartesian :: [[a]] -> [[a]]

cartesian [] = [[]] 
cartesian (h : t) = [el1 : el2 | el1 <- h, el2 <- cartesian t]
