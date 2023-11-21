module Evaluation.Substitution where

import Syntax.Expression
import Syntax.Grammar (parseProgram)
import Data.Set
{-|
    Returns a set of free variables in an expression.
-}
freeVarsSet :: Expression -> Set String
freeVarsSet (Var x) = singleton x
freeVarsSet (Lambda x e) = freeVarsSet e \\ singleton x
freeVarsSet (Application e1 e2) = freeVarsSet e1 `union` freeVarsSet e2
freeVarsSet _ = error "We can't compute the set of free variables on definitions!"

{-|
    Returns the list of free variables in an expression.
-}
freeVars :: Expression -> [String]
freeVars = toList . freeVarsSet

{-|
    Returns a set of bound variables in an expression.

    It is equivalent to computing a set of all variables in an expression then
    substraction all the free ones, so we don't really need the stand-alone definition.
-}
boundVarsSet :: Expression -> Set String
boundVarsSet (Var _) = empty
boundVarsSet (Lambda x e) = boundVarsSet e `union` singleton x
boundVarsSet (Application e1 e2) =
    (boundVarsSet e1 \\ freeVarsSet e2)
    `union`
    (boundVarsSet e2 \\ freeVarsSet e1)
boundVarsSet _ = error "We can't compute the set of bound variables on definitions!"

{-|
    Helper function that derives a new variable name based on an old one
    and which is not presend in a certain set of names.

    The strategy is to append `#` after the old name until the condition holds.
-}
generateNewName :: String -> Set String -> String
generateNewName x vars
    | not (x `member` vars) = x
    | otherwise = generateNewName (x ++ "#") vars

{-|
    Performs the substitution of the free occurrences of a variable within
    an expression with another expression.
-}
subst :: String      -- ^ Variable to be substituted
      -> Expression  -- ^ New expression to substitute the free occurances of the variable with
      -> Expression  -- ^ Existing expression in which to perform the substitution
      -> Expression  -- ^ Resulting expression
subst x e (Var y)
    | y == x = e
    | otherwise = Var y

subst x e' (Lambda y e)
    | y == x = Lambda x e
    | y /= x && not (y `member` fve') = Lambda y (subst x e' e)
 -- | y /= x && y `member` freeVarsSet e'
    | otherwise = Lambda z (subst x e' (subst y (Var z) e))
    where
        z    = generateNewName y (fve `union` fve')
        fve  = freeVarsSet e
        fve' = freeVarsSet e'

subst x e (Application e' e'') = Application (subst x e e') (subst x e e'')

-- internal :: String -> Expression
-- internal = maybe (error "Syntax error!") head . parseProgram

-- test_freeVars :: IO ()
-- test_freeVars = do
--   assertListsEqualAsSets ["x"] $ freeVarsInternal "x"
--   assertListsEqualAsSets [] $ freeVarsInternal "\\x.x"
--   assertListsEqualAsSets ["y"] $ freeVarsInternal "\\x.y"
--   assertListsEqualAsSets ["x", "y"] $ freeVarsInternal "(x y)"
--   assertListsEqualAsSets ["x", "y", "z"] $ freeVarsInternal "((x y) z)"
--   assertListsEqualAsSets [] $ freeVarsInternal "(\\x.x \\y.y)"
--   assertListsEqualAsSets ["x"] $ freeVarsInternal "(\\x.x x)"
--   assertListsEqualAsSets [] $ freeVarsInternal "\\x.(x x)"
--   where
--     freeVarsInternal = freeVars . internal

-- test_subst :: IO ()
-- test_subst = do
--   -- Replace x with z in x
--   assertEqual "z" $ substInternal "x" "z" "x"
--   assertEqual "y" $ substInternal "x" "z" "y"
--   assertEqual "\\x.x" $ substInternal "x" "z" "\\x.x"
--   assertEqual "\\y.z" $ substInternal "x" "z" "\\y.x"
--   assertEqual "\\y#.y" $ substInternal "x" "y" "\\y.x"
--   assertEqual "(y y)" $ substInternal "x" "y" "(x x)"
--   assertEqual "\\y#.(y# y)" $ substInternal "x" "y" "\\y.(y x)"
--   assertEqual "\\y#.(y \\y#.y)" $ substInternal "x" "y" "\\y.(x \\y.x)"
--   where
--     substInternal var new expr = show $ subst var (internal new) (internal expr)
