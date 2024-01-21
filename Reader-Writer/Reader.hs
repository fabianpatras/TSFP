{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Reader where

import Control.Applicative (Applicative (liftA2))

{-
    The Reader monad is similar to the State monad, but the hidden state
    is seen only as an initial read-only environment, which is used by all
    sequenced computations, but is never "changed".

    Defined in Control.Monad.Reader.
    See https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Reader.html

    A possible implementation is given below, where r is the type
    of the environment, and a is the type of the result of the computation.
    By analogy with the state monad, the second s in s -> (a, s) would no longer
    be necessary, since the first s is never "modified".

    TODO: Replace all the undefined portions below, such that
    (runReader test 10) returns 225.
-}
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    {-
        Apply the reader operation on the resulting `Reader`'s input state, then
        apply the function `f` to that.
    -}
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader op) = Reader $ \r -> f $ op r

instance Applicative (Reader r) where
    {-
        Construct a reader which always produces the same result.
    -}
    pure :: a -> Reader r a
    pure result = Reader $ const result

    {-
        Produce the two results from the two input Readers, apply `f` to them,
        then wrap the reusult into a Reader.
    -}
    liftA2 :: (a -> b -> c) -> Reader r a -> Reader r b -> Reader r c
    liftA2 f (Reader op1) (Reader op2) = Reader $ \r -> f (op1 r) (op2 r)

instance Monad (Reader r) where
    {-
        We have Applicative instanced for `Reader r`.
    -}
    return :: a -> Reader r a
    return = pure

    {-
        Make sure the same state `r` is passed to both input and output readers.

        Output reader is defined as a lambda which takes a state `r` as parameter
        so we just have to call the operation of the first readed on this input
        state `r` before applying the function f and also apply the same `r` to
        the operation of the reader resulting from `f`.
    -}
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader op) >>= f = Reader $ \r -> runReader (f (op r)) r

{-
    Returns the environment as the result.
-}
ask :: Reader r r
ask = Reader id

{-
    Runs the given computation within a modified environment, obtained
    by transforming the existing environment through the given function.
    The transformation is only visibile within the "local" computation.

    Examples:

    >>> runReader (local (* 10) double) 10
    200
-}
local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader op) = do
    -- shorter "uglier" version:
    -- op . f <$> ask
    env <- ask
    return $ op (f env)

{-
    >>> runReader test 10
    225
-}
test :: Reader Int Int
test = do
    x <- double
    y <- local (* 10) double  -- multiplies the environment by 10 before doubling
    z <- half                 -- the previous transformation has no effect here
    return $ x + y + z

{-
    Returns two times the environment, which is an Int.

    Examples:

    >>> runReader double 10
    20
-}
double :: Reader Int Int
double = do
    x <- ask
    return $ x * 2

{-
    Returns half the environment, which is an Int.

    Examples:

    >>> runReader half 10
    5
-}
half :: Reader Int Int
half = do
    x <- ask
    return $ x `div` 2