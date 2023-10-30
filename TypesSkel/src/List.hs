module List where

import Classes

{-
    Instantiate the following classes with the Haskell list type:
    * 'Container'
    * 'Invertible'

    The inversion should be performed DEEPLY i.e., for the elements as well.
-}

instance Container [] where
    -- contents :: c a -> [a]
    contents = id

instance (Invertible a) => Invertible [a] where
    -- invert :: a -> a, unde aici a e de fapt [a]
    -- f (.) g = \x f (g x)
    -- adica avem
    -- (map invert) (.) reverse = \x -> (map invert) (reverse x)
    --  = \x -> map invert $ reverse x
    --  = ia o lista x, ii face reverse ([1,2] -> [2,1]), apoi pentru fiecare element, ii aplica invert
    -- check with `invert [[True,False], [False]]`
    invert = (map invert) . reverse

    -- map :: (a -> b) -> [a] -> [b]
    -- invert :: a -> a -- dar in cazul nostru e (va fi) [a] -> [a]
    -- reverse :: [a] -> [a]
    -- (.) :: (b -> c) -> (a -> b) -> a -> c
    -- (.) has lowe precedence than function composition

    -- map invert . reverse == (map invert) . reverse
    -- (a -> b) -> [a] -> [b]
    --  unde (a - > b) e `invert`-u, care e `a -> a`
    -- deci `map invert` va avea `[a] -> [a]`
    -- (.) :: (b -> c) -> (a -> b) -> a -> c
    -- unde (b -> c) e functia `map invert` care are `[el] -> [el]`
    -- deci b == c == [el]
    -- reverse are [a] -> [a]
    -- deci ([el] -> [el]) -> ([a] -> [a]) -> (a          ) -> (a           )
    --      (map invertu ) -> (reverse   ) -> (lista input) -> (lista output)

