module Evaluation.Context where

import Syntax.Expression

import qualified Data.Map.Lazy as M
import Control.Monad.Trans.State

newtype Context = Context (M.Map String Expression)
                  deriving (Show, Eq)

emptyContext :: Context
emptyContext = Context M.empty

type Eval a = State Context a -- or type Eval = State Context
