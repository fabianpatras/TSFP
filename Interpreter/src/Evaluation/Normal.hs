module Evaluation.Normal where

import Evaluation.Substitution
import Evaluation.Context
import Syntax.Expression


import qualified Data.Map.Lazy as M
import Data.Maybe
import Control.Monad.Trans.State.Lazy

evalM :: Expression            -- ^ Expression to be evaluated
      -> Eval Expression       -- ^ Evaluation operation with hidden Context
evalM expr = do
    case expr of
        -- For a variable, check if the variable means something in the context and return
        -- or else it's just a variable and the evaluation ends
        Var x -> do
            Context ctx <- get
            return $ Data.Maybe.fromMaybe (Var x) (M.lookup x ctx)

        -- This is the end of a computation for normal-order evaluation.
        -- We don't care if `e` cand still be evaluated.
        Lambda x e -> do
            return $ Lambda x e

        -- Now, for `Application`s we got two cases:
        -- 1) Reduce step, when we have a lambda as the left expression
        Application (Lambda x e) e' -> do
            return $ subst x e' e

        -- 2) Eval, when we have something that is not a lambda as the left expression
        Application e e'' -> do
            e' <- evalM e
            return $ Application e' e''

        -- Finally, evaluation of a definition modifies the context
        -- and returns the expression
        Definition var e -> do
            Context ctx <- get
            put $ Context $ M.insert var e ctx
            return e


{-|
    Small-step normal-order evaluation of a given expression,
    within a given context.
-}
eval :: Expression             -- ^ Expression to be evaluated
     -> Context                -- ^ Context where the evaluation takes place
     -> (Expression, Context)  -- ^ Evaluation result, together with a possibly
                               --   enriched context, in case of definition

eval = runState . evalM

{-

    Old, non-Mondaic version os `eval`.

    -- For a variable, check if the variable means something in the context and return
    -- or else it's just a variable and the evaluation ends
    eval (Var x) ctx@(Context m) = (Data.Maybe.fromMaybe (Var x) (M.lookup x m), ctx)

    -- This is the end of a computation for normal-order evaluation.
    -- We don't care if `e` cand still be evaluated.
    eval lambda@(Lambda _ _) context = (lambda, context)

    -- Now, for `Application`s we got two cases:
    -- 1) Reduce step, when we have a lambda as the left expression
    eval (Application (Lambda x e) e') context = (subst x e' e, context)

    -- 2) Eval, when we have something that is not a lambda as the left expression
    eval (Application e e'') context = (Application e' e'', context')
        where (e', context') = eval e context

    -- Finally, evaluation of a definition modifies the context
    -- and returns the expression
    eval (Definition var e) (Context m) = (e, Context $ M.insert var e m)

-}
