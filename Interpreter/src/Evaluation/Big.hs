module Evaluation.Big where

import Syntax.Expression
import Evaluation.Context

import Data.Traversable
import Data.Tuple
import Control.Monad.Trans.State.Lazy()

evalBigM :: (Expression -> Eval Expression)  -- ^ Small-stepper
         -> Expression                       -- ^ Expression to be evaluated
         -> Eval Expression                  -- ^ Resulting Eval state

evalBigM f expr = do
    case expr of
        Var x -> f (Var x)
        Lambda x e -> return $ Lambda x e
        Application e e' -> evalBigM f (Application e e')
        Definition _ _ -> return undefined


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

evalBig f expr context = case f expr context of
                            (var@(Var _), ctx) -> f var ctx
                            res@(Lambda _ _, _) -> res
                            (app@(Application _ _), ctx) -> evalBig f app ctx
                            (Definition _ _, _) -> undefined


evalListM :: (Expression -> Eval Expression)
          -> [Expression]
          -> Eval [Expression]
evalListM = mapM


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
evalList f exprs context = swap $ mapAccumL (\c e -> swap $ evalBig f e c) context exprs
