module Evaluation.Big where

import Syntax.Expression
import Evaluation.Context
import qualified Evaluation.Normal as NormalOrder
import qualified Evaluation.Applicative as ApplicativeOrder

import Data.Traversable
import Data.Tuple

{-|
    Big-step evaluation of a given expression, within a given context.
    The evaluation should stop when either the value is reached,
    or the expression cannot be reduced further.

    The first argument is the small-step evaluation function.
-}
evalBig :: (Expression -> Context -> (Expression, Context))  -- ^ Small-stepper
        -> Expression             -- ^ Expression to be evaluated
        -> Context                -- ^ Context where the evaluation takes place
        -> (Expression, Context)  -- ^ Evaluation result,
                                  --   together with a possibly enriched context
                                  --   in case of definition

-- If we get to a Variable, we stop
-- evalBig f var@(Var v) context = (var, context)

evalBig f var@(Var x) context = case f var context of
                                    res@(var'@(Var x'), ctx) -> if x == x' then res else (evalBig f var' ctx)
                                    res@(e, ctx) -> evalBig f e ctx

-- evalBig f lambda@(Lambda x e) context = case f lambda context of


evalBig f expr context = case f expr context of
                            res@(var@(Var _), ctx) -> evalBig f var ctx
                            res@(Lambda _ _, _) -> res
                            res@(app@(Application _ _), ctx) -> evalBig f app ctx

{-|
    Big-step evaluation of a list of expressions, starting with
    the given context and using it throughout the entire list,
    for propagating the encountered definitions.

    The first argument is the small-step evaluation function.
-}
evalList :: (Expression -> Context -> (Expression, Context))
         -> [Expression]
         -> Context
         -> ([Expression], Context)
-- evalList = undefined
evalList f exprs context = swap $ mapAccumL (\c e -> swap $ evalBig f e c) context exprs
