{-|
    A parsing module, based on applicative functors.
-}
module Syntax.Parser
    ( Parser
    , runParser
    , backSlash
    , dot
    , eof
    , equals
    , parse
    , leftParen
    , rightParen
    , whiteSpace
    , word
    ) where

import Data.Char
import Control.Applicative

{-|
    The type of parsers.

    Each parser is seen as a function taking an input string to a pair between
    the parse result and the rest of the input, if the parsing succeeds, or
    to @Nothing@, if the parsing fails. The function type is wrapped into a
    @newtype@ declaration, for implementation hiding. The parser type is
    parametrized over the type of its result, @a@.
-}
newtype Parser a = P { runParser :: String -> Maybe (a, String) }

-- | Never parses anything, always returning @Nothing@.
failure :: Parser a
failure = P $ const Nothing

{-
    const :: a -> b -> a
    const x _ =  x

    Nothing :: Maybe c

    ($) :: (d -> e) -> d -> e
    ($) f x = f x
    ~~
    `const` takes 2 arguments and returns the first, ignoring the second
    so `const Nothing` will be a function which will take one argument,
    ignore it, and return `Nothing`.

    const Nothing :: b -> Maybe c
    a == Maybe c

    `($)` takes a function `(d -> e)` and an argument `d` and applies the
    function to the argument. It has the highest precedence and it right-to-left
    associative.

    ($) :: (d -> e) -> d -> e
    const :: a -> b -> a

    Or, `const` takes one argument of type `a` and return a function which
    takes one arg of type `b` and returns something ot type `a`.
    const :: a -> (b -> a)

    So `a -> (b -> a)` == `d -> e` which means that
    `d` == `a`
    `e` == `b -> a`

    So `$ const` has the type:
    $ const :: a -> (b -> a)
    which is
    $ const :: a -> b -> a

    And now, the fist argument being `Nothing`, we get

    $ const Nothing :: b -> Maybe c

    And `P` is a Value Constructor which has the following type
    P :: (String -> Maybe (a, String)) -> Parser a

    So it takes as input a function of type
    String -> Maybe (a, String)

    And we're passing `$ const Nothing` which is of type
    b -> Maybe c

    By pattern mathching we get that
    `b` == `String`
    `Maybe c` == `Maybe (a, String)` so `c` == `(a, String)`

    `$ const Nothing` is out inner function, `runParser`.

    It takes as input any String and returns a `Nothing` of type `Maybe (a, String)`

    We can run this parser by calling the accessor function `runParser`, which is
    effectively the name of the value of type `String -> Maybe (a, String)` which
    `Parser a` contains:

    runParser :: Parser a -> String -> Maybe (a, String)

    The first argument is the `Parser a` which we want to access the inner function of.
    The second argument is the input string of the parser.
-}


{-|
    Parses an explicitly given value, without consuming any input.

    Examples:

    >>> runParser (success 1) "abc"
    Just (1, "abc")
-}
success :: a -> Parser a
success result = P $ \s -> Just (result, s)

{-
    Now, compared with `failure` which produces a `Parser a` that always
    fails to parser, this one needs to succeed in every parse attempt.

    This means, it needs to return a result of type `Just (a, String)`.
    This further means that it needs somehting of type `a` to return.
    So it needs an input argument of type `a`.

    Then, the same thing happens inside `P` constructor

    It is being passed `$` which takes one argument, the function to apply.
    Effectively, `runParser` is that lambda inside `\s -> Just (result, s)`
    where `result` is being passed when the `Parser a` value is constructed
    with `success`.
-}

{-|
    Parses a given character.

    Examples:

    >>> runParser (token 'a') "abc"
    Just ('a', "bc")

    >>> runParser (token 'a') "bbc"
    Nothing
-}
token :: Char -> Parser Char
-- token tok = spot (== tok)
token     = spot . (==)

{-
    We want to be able to create custom Parsers which can parse different stuff.
    An example is a `Char`. We want to be able to parse a `Char`.

    We're doing that with an auxiliary function: `spot`.

    We have `spot` implemented. `spot` takes a predicate as input.

    We can construct that predicate to be something like
    \input_token -> input_token == to_be_matched_char
    which is an equallity for a constant `Char` `to_be_matched_char`.

    There is `(==)` function with the signature
    (==) :: (Eq a) => a -> a -> Bool
    We can create a predicate of type
    Char -> Bool
    by passing a `Char` as the first argument of `(==)`.

    This would look like `\tok -> == tok`, but we have `tok` as input to `token` function.

    Q: How do we write it point free?
    A: `token = spot . (==)`

    Let's check it by looking at the types.

    spot :: (Char -> Bool) -> Parser Char
    (==) :: Eq a => a -> a -> Bool
    (.)  :: (c -> d) -> (b -> c) -> b -> d
    (.) f g x = f (g x)
    OR

    (.)  :: (c -> d) -> (b -> c) -> (b -> d)
    (.) f g = \x -> f (g x)
    Which is to be read that is takes 2 functions as arguments and returns a function.

    `(.)` is defined as `infixr 9 .` which means that `.` or `.` has the highest precedence
    possible and it ir right associative. So `f . g . h` would be interpreted as `f . (g . h)`

    `.` can be used in infix notation like `f . g` or in prefix notation like `(.) f g`.

    In our case `spot . (==)` is equivalent to `(.) spot (==)`

    Let's start the type analysis, we want to get the type of `spot . (==)` or `(.) spot (==)`.
    spot :: (Char -> Bool) -> Parser Char
    (==) :: Eq a => a -> a -> Bool
    (.)  :: (c -> d) -> (b -> c) -> b -> d

    (.) spot (==) means that
        - `spot` is of type `c -> d`
        - `(==)` is of type `b -> c`

    But `spot` is `(Char -> Bool) -> Parser Char` so:
        - `c` == `Char -> Bool`
        - `d` == `Parser Char`
    And `(==)` is `a -> a -> Bool` which is `a -> (a -> Bool)` so:
        - `b` == `a`
        - `c` == `a -> Bool`, but `c` == `Char -> Bool` so:
            - `a` == `Char` so:
                - b` == `Char`

    The type of `spot . (==)` is `b -> d` which is `Char -> Parser Char`.
    Simple as that :)

    How is `spot . (==)` to be read is like this:
    It translates to the lambda `\x -> spot ((==) x)` so we have to apply to
    `spot` as argument the result of `(==) x` which is a predicate that checks for
    equatily to `x`. And we know that `spot` is exactly looking for that, a predicate
    to use to create the `Parser Char`.

    Nice? I guess.
-}

{-|
    Parses a character that satisfies a given property.

    Examples:

    >>> runParser (spot isLetter) "abc"
    Just ('a', "bc")

    >>> runParser (spot isLetter) "123"
    Nothing
-}
spot :: (Char -> Bool) -> Parser Char
spot prop = P f
  where
    f [] = Nothing
    f (x : xs)
        | prop x    = Just (x, xs)
        | otherwise = Nothing

{-
    `spot` takes a predicate as input and creates a Parser which parses
    `Char`s that pass that predicate / property.

    By how `spot` is implemented we can see that `f` is actually `runParser`.

    The type of `f` is `[Char] -> Maybe (Char, [Char])` which is
    `String -> Maybe (Char, String)`.

    `f` is looking at the first element of its input.

    If the element does not exist then the parser fails and returns `Nothing`.
    If the element exists and is passes the property, then return that element and
    the rest of the list (of `Char`s).
    If the element does not passes the property, return `Nothing` agian.

    We'll use this to implement `token` from above which maches exact a constant `Char`.
-}

instance Functor Parser where
    {-
        Applies a function onto the result of a parser. Also written as (<$>).

        It is also said that fmap "lifts" or promotes a function of type
        (a -> b) to the parsing context, yielding a function of type
        (Parser a -> Parser b).

        fmap :: (a -> b) -> Parser a -> Parser b

        Intuitively (but not quite correct):

        fmap :: (a -> b) -> (String -> Maybe (a, String))
                         -> (String -> Maybe (b, String))
    -}
    fmap f (P p) = P $ \s -> fmap (applyToFirst f) $ p s
    {-           = P $ fmap (applyToFirst f) . p
                         ^
                         |
            this fmap corresponds to Maybe

        instance Functor Maybe where
            fmap :: (a -> b) -> Maybe a -> Maybe b
            fmap f (Just x) = Just $ f x
            fmap f Nothing  = Nothing

        Verbose version:

        fmap f (P p) = P $ \s -> case p s of
            Just (res, s') -> Just (f res, s')
            Nothing        -> Nothing
    -}

{-
    The `Functor` is "applied" to a Type Constructor of kind `* -> *`.

    For us, this means `Parser` WITHOUT `a`.

    Let's see this point free way of writing `fmap`.
    fmap f (P p) = P $ fmap (applyToFirst f) . p

    It can be read as:
                 = P $ \s -> (fmap (applyToFirst f))         (p s)
                              fmap :: a -> b          `Maybe (a, String)`
                                    applyToFirst applies f to the first of the tuple.

    There is an alias to fmap (compare it to `($)`):

     ($)  ::              (a -> b) ->   a ->   b
    (<$>) :: Functor f => (a -> b) -> f a -> f b
    <$> = fmap

    `<$>` is defined as `infixl 4 <$>` which means if is left associative
    so `f <$> g <$> h` is to be interpreted as `(f <$> g) <$> h`

    With `<$>` we'll have this type of calling:
    >>> even <$> (1,2)
    (1, True)


    `Functor` class also has `(<$)` operator which is defined as `infixl 4  <$`
    so it is left associative, just like `(<$>)`.

    ```Replace all locations in the input with the same value.```
    (<$) :: a -> f b -> f a
    (<$) = fmap . const

    This is to be read like this:
    `(<$)` takes 2 arguments as input one is a "normal"/"doesnt-matter-if-functor-or-not" type
    the second is a functor.

    (<$) = fmap . const
         = \x -> fmap (const x)
         which can be rewritten as
    (<$) x = fmap (const x)
           = fmap (\y -> const x y)
         which can be rewritten as
    (<$) x functor = fmap (\y -> const x y) functor
    Which is to be read like this:
        We have a the `functor` and a value `x` and we replace the inner value of
        the `functor` with the value `x`

    >>> 32 <$ Just "Salut"
    Just 32

    What we did is to throw out the window the inner value of the functor and replace it
    with the "hardcoded" value of `x`.

    >>> map ((<$) 32) [Just "Hello", Just "Andrei"]
    [Just 32, Just 32]
-}

{-|
    Parses a letter at the beginning of the input string.

    Examples:

    >>> runParser letter "abc"
    Just ('a', "bc")

    >>> runParser letter "123"
    Nothing
-}
letter :: Parser Char
letter = spot isLetter

{-|
    Parses a digit at the beginning of the input string, and returns
    the result as an @Int@:

    Examples:

    >>> runParser digit "123"
    Just (1, "23")

    >>> runParser digit "abc"
    Nothing
-}
digit :: Parser Int
-- digit = fmap digitToInt $ spot isDigit
digit   = digitToInt <$> spot isDigit

{-
    How do we read this?
    We can read it like this:
        - We have the `Parser Char` produced by `spot isDigit`
        This parser parses digits (i.e. `0` .. `9`), but we want
        To get an integer representation of that digit.
        - We have a function which transforms digits to `Int`s
        - Because we have `fmap` implemented for `Parser` (without `a`)
        we know we can apply `fmap f parser` and this will change 2 things:
            * The type of the parser: from `Parser Char` we get a `Parser Int`
            Firstly, `Parser Char` which is `spot isDigit` is of a concrete
            type `String -> Maybe (Char, String)`.
            Then, out `fmap` instance for `Parser`, applies `f` as a nested `fmap` "call"
            to the result of the input string parsed by the `Parser Char`.
            * The value returned from the parser, by the process above.
-}

{-
    Applicative functors are stronger than general functors, in their ability
    to sequence several computations.

    For example, if we wanted to sequence two digit parsers (above), a first
    attempt using fmap would yield

    (,) <$> digit :: Parser (a -> (Int, a))

    that is, a Parser wrapping a function which we would have no means
    to extract in order to apply it to the result of the second digit parser.

    Applicatives solve this problem, by providing a way to apply a wrapped
    function onto a wrapped argument, yielding a wrapped result (see (<*>)).

    fmap in Functor can be implemented solely in terms of pure and (<*>),
    meaning that any Applicative is also a Functor:

    fmap f x = pure f <*> x
-}
instance Applicative Parser where
    {-
        The minimum functionality parser that still returns the given value
        as its result. In this case, minimum functionality = no actual parsing.

        pure :: a -> Parser a
    -}
    pure = success

    {-
        Performs function application withing a parsing context. Notice that
        the function, of type @(a -> b)@, its argument, of type @a@,
        and its result, of type @b@ are all parsing results.

        Used for sequencing several parsers, and passing their results
        as arguments to another function.

        (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    -}
    P p <*> P p' = P $ \s -> case p s of
        Just (f, s') -> fmap (applyToFirst f) $ p' s'
        Nothing      -> Nothing
    {-
        Verbose version:

        P p <*> P p' = P $ \s -> case p s of
            Just (f, s') -> case p' s' of
                                Just (r, s'') -> Just (f r, s'')
                                Nothing       -> Nothing
            Nothing      -> Nothing
    -}

    {-
        After implementing the two above, the following two operators come
        for free:

        (*>) :: Parser a -> Parser b -> Parser b
        (<*) :: Parser a -> Parser b -> Parser a

        They sequence two parsers but only keep the results of one. The `>` or
        `<` symbols point toward the parser the result of which is kept.
    -}

{-|
    Parses a letter followed by a digit.

    Examples:

    >>> runParser letterDigit "a12"
    Just (('a', 1), "2")
-}
letterDigit :: Parser (Char, Int)
letterDigit = (,) <$> letter <*> digit
        --  = liftA2 (,) letter digit

{-|
    Parses an expression of the form @\<digit\> \<operator\> \<digit\>@, where
    @\<operator\>@ can be any of +, -, *, /.

    Examples:

    >>> runParser operation "1+2"
    Just ((1, '+', 2), "")
-}
operation :: Parser (Int, Char, Int)
operation = (,,) <$> digit <*> spot (`elem` "+-*/") <*> digit
      --  = liftA3 (,,) digit (spot (`elem` "+-*/")) digit

{-|
    An extended version of 'operation', which also consumes parantheses.

    Examples:

    >>> runParser parOperation "(1+2)"
    Just ((1, '+', 2), "")
-}
parOperation :: Parser (Int, Char, Int)
parOperation = (,,) <$> (token '(' *> digit)
                    <*> spot (`elem` "+-*/")
                    <*> (digit <* token ')')

instance Alternative Parser where
    {-
        The alternative between two parsers. If the first parser succeeds,
        keeps the result. Otherwise, tries the second parser. Notice that
        both parsers need results of the same type.

        (<|>) :: Parser a -> Parser a -> Parser a
    -}
    P p <|> P p' = P $ \s -> maybe (p' s) Just (p s)
    {-
        Verbose version:

        P p <|> P p' = P $ \s -> case p s of
            j@(Just _) -> j
            Nothing    -> p' s
    -}

    {-
        The neutral parser with respect to the alternation operator.
        This is precisely the parser that always fails.

        empty :: Parser a
    -}
    empty = failure

    {-
        After implementing the two above, the following two functions come
        for free:

        many :: Parser a -> Parser [a]
        some :: Parser a -> Parser [a]

        'many' uses a parser to recognize 0 or more occurrences
        of its result. 'some', 1 or more.
    -}

{-|
    Parses an \'A\' in a case insensitive manner.

    Examples:

    >>> runParser insensitiveA "ab"
    Just ('a', "b")

    >>> runParser insensitiveA "Ab"
    Just ('A', "b")
-}
insensitiveA :: Parser Char
insensitiveA = token 'a' <|> token 'A'

{-|
    Parses 1 or more \'A\'s in a case insensitive manner.

    Examples:

    >>> runParser insensitiveAs "aAab"
    Just ("aAa", "b")
-}
insensitiveAs :: Parser String  -- String = [Char]
insensitiveAs = some insensitiveA

{-|
    End of input parser.
-}
eof :: Parser ()
eof = P f
  where
    f "" = Just ((), "")
    f _  = Nothing

{-|
    Applies a parser onto an input string, and returns the result.

    Examples:

    >>> parse digit "123"
    Just 1

    >>> parse digit "abc"
    Nothing
-}
parse :: Parser a -> String -> Maybe a
parse = (fmap fst .) . runParser

{-|
    Applies a function to the first component of a pair.
-}
applyToFirst :: (a -> b) -> (a, c) -> (b, c)
applyToFirst f (x, y) = (f x, y)


-- Start of token Parser

{-
    Parses at least one letter.
    Will be used to parse variable names such as "x" or "xy" or declaration names
    such as "true" from "frue=\x.\y.x"
-}
word :: Parser String
word = some letter

{-
    Parses `\` characher.
-}
backSlash :: Parser Char
backSlash = spot (== '\\')

{-
    Parses `.` character.
-}
dot :: Parser Char
dot = spot (== '.')

{-
    Parses `=` character.
-}
equals :: Parser Char
equals = spot (== '=')


{-
    Parses '(' character.
-}
leftParen :: Parser Char
leftParen = spot (== '(')

{-
    Parses ')' character.
-}
rightParen :: Parser Char
rightParen = spot (== ')')


{-
    Parses any of " \t\n\r\f\v".
-}
whiteSpace :: Parser Char
whiteSpace = spot isSpace
