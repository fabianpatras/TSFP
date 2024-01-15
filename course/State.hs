{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module State where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity hiding (fix)
import qualified Data.Map as M
import qualified Control.Applicative as input

{-
    The type of computations operating on a state of type s and giving a result
    of type a, along with the new state.

    Defined in Control.Monad.State. Instance of Functor, Applicative, Monad.
    See http://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Lazy.html
-}
newtype State s a = State { runState :: s -> (a, s) }

{-
    ME: The important part about state is that it is a `newtype` of a function.

    `State s a` is just a wrapper for `runState :: s -> (a, s)` which is a function.
-}


-- push :: a -> [a] -> [a]
-- pop :: [a] -> a

{-
    A push operation on a stack.
    See http://learnyouahaskell.com/for-a-few-monads-more#state

    (push a) is seen as a computation operating on a state of type [a]
    (a stack) and returning an unimportant result. What matters is the new
    state (stack), obtained by pushing the given element onto top
    of the old stack.

    Example:

    >>> runState (push 3) [2, 1]
    ((), [3,2,1])
-}
push :: a -> State [a] ()
push a = State $ \s -> ((), a : s)

{-
    `push 3` is effectively `State $ \s -> ((), 3 : s)` which is wrapper over a
    lambda function which takes a "state" (a list of ints) and returns a pair of
    the uninteresting result `()` and the updated result of pushing `3` onto
    that list `3 : s`.

    `runState (push 3)`: `runState` is the name of the function which accesses
    the lambda `s -> (a, s)` from within a `State`.
    So `runState (push 3)` results in the lambda: `\s -> ((), 3 : s)`.

    Knowing the above:

    >>> runState (push 3) [2, 1]
    Firstly, `runState (push 3)` is `\s -> ((), 3 : s)` so the above line results in
    >>> \s -> ((), 3 : s) [2, 1]
        which is a function call .. a pretty verbose one as well
    >>> ((), 3 : [2, 1])
    >>> ((), [3, 2, 1])
-}


{-
    A pop operation on a stack. Does not check for the empty stack.
    See http://learnyouahaskell.com/for-a-few-monads-more#state

    pop is seen as a computation operating on a state of type [a] (a stack)
    and returning its top element. The new state reflects the change.

    Example:

    >>> runState pop [2, 1]
    (2, [1])
-}
pop :: State [a] a
pop = State $ \(a : s) -> (a, s)

{-
    Now, `pop` doesn't take an argument, but it creates a `State` with the inner
    lambda `\(a : s) -> (a, s)` which pattern-matches so the argument of
    `\s -> (a, s)` is a list with at least one element and returns the element
    as the result and the rest of the list as the new state (reflecting the
    pop).

    >>> runState pop [2, 1]
    >>> \(a : s) -> (a, s) [2, 1]
        Which is an ordinary function call.
    >>> (2, [1])
-}

{-
    Monad instance, for sequencing computations operating on the *same* type
    of state. Notice that s in (State s) is fixed for a given sequence
    of operations.
-}
instance Monad (State s) where
    {-
        The minimal computation that still returns the given value
        as its result. In this case, minimal = no state change.

        return :: a -> State s a
    -}
    return a = State $ \s -> (a, s)

    -- (>>=) :: State s a -> (a -> State s b) -> State s b
    State st >>= f = State $ \s ->
        let (a, s') = st s
        in  runState (f a) s'

    {-
        Let's recap:
        - bind: (>>=) :: m a -> (a -> m b) -> m b
            takes 2 arguments:
                1) a value of type `a` which is "boxed" inside a context of type `m`
                - For our State example here, `m` is `State s`
                2) a function which takes as input a value of type `a` and returns
                a value of type `b` which is "boxed" inside a type `m`
                - For our example `push :: a -> State [a] ()` could be and example
                which is an operation (`State` in itself is also an operation (lambda))
            In our example, `(>>=)` does :
            1) Unboxes `m a` which is `State s a` into `State st`
                and now we can access the `runState` which is `st`
            2) Creates a new `State` which will return in the end which takes a
                value of type `s` (from `State s a`)
            3) Applies the `runState` `st` on that state `s`
            4) Pattern-matches the resul to access the new result `a` and the new
                state `s`.
            5) Uses the funtion `f` (argument to bind) to apply it to the result
                `a`. This will result in a `State s b`
            6) Then will apply `runState` onto the new State `State s b` which is
                just a way of accessing the inner lamba
            7) Runs that lambda with the `s'` as argument and this will result in
                a pair of a new result `a'` let's say and a new state `s''` let's
                say
            8) The final `State s b` returned by the `(>>=)` (bind) call is a
                lambda which takes as input the state `s` from step 2) and return
                the pair returned at step 7)
    -}

    -- st1 >> st2 = st1 >>= const st2

    {-
        (>>) :: m a -> m b -> m b
        (>>) this function is defined as
        st1 >> st2 = st1 >>= const st2
        The interesting part is `const st2` because `const :: a -> b -> a` takes
        2 arguments and returns the first one

        Combined with (>>=) defined above we would have
        State st >>= f = State $ \s ->
            let (a, s') = st s
            in runState (f a) s'

        For simplicity replace `st1` with `State st1` so we already have
        pattern-matched input.Applicative

        State st1 >> State st2 = State st1 >>= (const State st2)
            = State $ \s ->
                let (a, s') = st1 s
                in runState (const (State st2) a) s'
            = State $ \s ->
                let (a, s') = st1 s
                in runState (State st2) s'
            = State $ \s ->
                let (a, s') = st1 s
                in st2 s'
        ----------
        State st1 >> State st2 = State $ \s ->
            let (_, s') = st1 s
            in st2 s'

        Which means that we construct a lambda (an operation) whose input is modified
        by the first's state operation (`st1 s`), the result is discarded, but the
        state is kept `s'` and then modify it with the action of the second state.
    -}


{-
    Computation operating on a state of type s and returning that very state
    as its result.

    Examples:

    >>> runState get [1, 2]
    ([1, 2], [1, 2])
-}
get :: State s s
get = State $ \s -> (s, s)

{-
    (put s) is a computation operating on a state of type s and replacing
    the underlying state with s. The actual result is unimportant.

    Examples:

    >>> runState (put [3, 4]) [1, 2]
    ((), [3, 4])
-}
put :: s -> State s ()
put s = State $ \_ -> ((), s)

{-
    (modify f) is a computation operating on a state of type s and transforming
    the underlying state through f. The actual result is unimportant.

    Examples:

    >>> runState (modify (0 :)) [1, 2]
    ((), [0, 1, 2])
-}
modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

{-
    Sequence of stack computations.

    Examples:

    >>> runState pushLength1 [1, 2]
    ((), [2, 1, 2])
-}
pushLength1 :: State [Int] ()
pushLength1 =   pop            -- remove the top element using pop,
            >>= push           -- pass it on to push, which places it back,
            >>  get            -- get the current stack, using (>>), because
                               -- we are not interested in the result of push,
                               -- which is always (),
            >>= push . length  -- compute its length and push the latter

{-
    Based on pushLength1, but uses the monadic do notation.
-}
pushLength2 :: State [Int] ()
pushLength2 = do
    a <- pop
    push a
    s <- get
    push $ length s  -- up to this point, does the same thing as pushLength1,
    put [5, 5, 5]    -- completely replaces the current stack with [5, 5, 5],
    modify tail      -- applies tail onto this stack, always yielding [5, 5]

{-
    Sequences all the operations in a list, by building a single operation
    which returns the list of individual results.

    Defined as sequence in Control.Monad.
    See http://hackage.haskell.org/package/base-4.7.0.1/docs/Control-Monad.html#v:sequence

    Verbose implementation:

    mySequence []         = return []
    mySequence (op : ops) = liftM2 (:) op $ mySequence ops

    Examples:

    >>> runState (sequence [pop, pop, pop]) [1, 2, 3, 4]
    ([1, 2, 3], [4])
-}
mySequence :: Monad m => [m a] -> m [a]
mySequence = foldr (liftM2 (:)) (return [])

{-
    Applications of the monadic generalizations of list functions.

    See:
    * http://hackage.haskell.org/package/base-4.7.0.1/docs/Control-Monad.html#g:4
    * http://hackage.haskell.org/package/base-4.7.0.1/docs/Control-Monad.html#g:5

    The functions ending in "_" e.g., sequence_, discard the individual results
    and return (). This is useful for example in the case of multiple pushes,
    which would return a useless list of ()s.
-}

{-
    Sequences three pushes.
-}
complex1 :: State [Int] ()
complex1 = sequence_ [push 2, push 3, push 4]

{-
    Another variant of complex1.
-}
complex2 :: State [Int] ()
complex2 = mapM_ push [2, 3, 4]

{-
    Sequences 10 (push 2) computations.
-}
complex3 :: State [Int] ()
complex3 = replicateM_ 10 $ push 2

{-
    Retains only the first occurrence of each element in a list.

    filterM, whose first argument is a function of type (a -> m Bool), allows
    for testing a property which depends not only on the current list element,
    but also on the current context, hidden within the monad m.

    Here, a = Int, m = State [Int]. The state has the type [Int], for keeping
    the previously seen elements, while iterating over the list.

    Examples:

    >>> runState (unique [1, 2, 3, 3, 4, 2, 1]) []
    ([1, 2, 3, 4], [4, 3, 2, 1])
-}
unique :: [Int] -> State [Int] [Int]
unique = filterM check
  where
    check :: Int -> State [Int] Bool
    check a = do
        seen <- get            -- elements seen so far
        if elem a seen         -- was the current element seen?
            then return False  -- if so, then reject it
            else do            -- otherwise
                push a         -- store it
                return True    -- and accept it just this once

{-
    The monadic generalizations of list functions can be used to define various
    parsers.

    Examples:

    >>> runParser (mapM token "ab") "abc"
    Just ("ab", "c")

    >>> runParser (replicateM 3 $ token 'a') "aaaabc"
    Just ("aaa", "abc")

    The first example recognizes a whole string. The second, a fixed number
    of occurrences of the same token.
-}

{-
    Monadic open recursion (see lecture on fixed points).

    A previously disccused example of Fibonacci numbers now uses a monadic
    version of open recursions, paving the way for storing the results
    of intermediate computations using, for example, a State monad.

    For further details, see:
    https://www.cs.utexas.edu/~wcook/Drafts/2006/MemoMixins.pdf
-}
fix :: (a -> a) -> a
fix f = f (fix f)

bigFibo :: Monad m => (Int -> m Int) -> Int -> m Int
bigFibo _ 0 = return 0
bigFibo _ 1 = return 1
bigFibo f n = liftM2 (+) (f $ n - 1) (f $ n - 2)

{-
    If we simply use the Identity monad, no gain is obtained in execution time.
-}
fakeMemo :: (Int -> Identity Int) -> Int -> Identity Int
fakeMemo f n = f n

fibo = runIdentity . fix (fakeMemo . bigFibo)

{-
    However, we now employ a State monad, and we get a signigicant decrease
    in computation time.
-}
type Memo a b = State (M.Map a b)

bigMemo :: Ord a => (a -> Memo a b b) -> (a -> Memo a b b)
bigMemo f n = do
    dictionary <- get
    case M.lookup n dictionary of
        Just result -> return result
        Nothing     -> do
            result <- f n
            modify $ M.insert n result
            return result

memoFibo n = runState (fix (bigMemo . bigFibo) n) M.empty