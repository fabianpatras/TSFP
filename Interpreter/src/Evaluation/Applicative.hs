module Evaluation.Applicative where

import Syntax.Expression
import Evaluation.Substitution
import Evaluation.Context

import qualified Data.Map.Lazy as M
import Data.Maybe

{-|
    Small-step applicative-order evaluation of a given expression,
    within a given context.
-}
eval :: Expression             -- ^ Expression to be evaluated
     -> Context                -- ^ Context where the evaluation takes place
     -> (Expression, Context)  -- ^ Evaluation result, together with a possibly
                               --   enriched context, in case of definition

-- For a variable it's the same as for Normal-Order Evaluation
eval (Var x) ctx@(Context m) = (Data.Maybe.fromMaybe (Var x) (M.lookup x m), ctx)

-- For a lambda it's the same as for Normal-Order Evaluation
eval lambda@(Lambda _ _) context = (lambda, context)


-- Now, for `Application`s we got three cases:
-- 1) Reduce step, only when the right expression is a value
eval (Application (Lambda x e) var@(Var _)) context = (subst x var e, context)
eval (Application (Lambda x e) lambda@(Lambda _ _)) context = (subst x lambda e, context)

-- 3) Eval_2, when the left expression is a Lambda and the right expression can be evaluted,
-- evaluate the right expression
--
-- It NEEDS to be declared before `Eval_1` so Haskell will pick the particular case over
-- the generic one
eval (Application lambda@(Lambda _ _) e) context = (Application lambda e', context')
    where (e', context') = eval e context

-- 2) Eval_1, When the left expression can be evaluated, evaluate it
eval (Application e e'') context = (Application e' e'', context')
    where (e', context') = eval e context

-- Finally, evaluation a definition modifies the context
-- and returns the epression
eval (Definition var e) (Context m) = (e, Context $ M.insert var e m)
