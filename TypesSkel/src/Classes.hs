module Classes where

{-
    A class for container types, which are able to enumerate their elements
    using a Haskell list. What is the kind of 'c'?

    A: kind of `c` is `* -> *`.
-}
class Container c where
    contents :: c a -> [a]

{-
    A class for types with invertible values. What is the kind of 'a'?
    The default invert operation is the identity.

    A: kind of `a` is `*`
-}
class Invertible a where
    invert :: a -> a
    invert = id

{-
    Primitive types are instances of the 'Invert' class.
    According to the default definition of 'invert', nothing is actually
    performed onto the primitive values.
-}
instance Invertible Char
instance Invertible Bool where
    invert = not
instance Invertible Int
instance Invertible Integer
instance Invertible Float
instance Invertible Double