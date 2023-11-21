module Evaluation.Context where

import Syntax.Expression

import qualified Data.Map.Lazy as M

newtype Context = Context (M.Map String Expression)
                  deriving (Show, Eq)

emptyContext :: Context
emptyContext = Context M.empty
