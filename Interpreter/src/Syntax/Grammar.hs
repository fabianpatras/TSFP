module Syntax.Grammar where

import Syntax.Expression
import Syntax.Parser
import Control.Applicative
    (Applicative (liftA2), Alternative ((<|>), many, some))

parseProgram :: String -> Maybe [TopLvlExpression]
parseProgram = parse (many parseTopLvlExpression)

parseVar :: Parser Expression
parseVar = Var <$> word

parseLambda :: Parser Expression
parseLambda = liftA2
                Lambda
                (backSlash *> word <* dot)
                parseExpression

parseApplication :: Parser Expression
parseApplication = liftA2
                    Application
                    (leftParen *> many whiteSpace *> parseExpression <* some whiteSpace)
                    (parseExpression <* many whiteSpace  <* rightParen)

parseExpression :: Parser Expression
parseExpression = (parseApplication <|> parseLambda <|> parseVar) -- <* many whiteSpace

parseDefinition :: Parser TopLvlExpression
parseDefinition = liftA2
                    Definition
                    (parseExpression <* equals)
                    parseExpression

parseNormalExpression :: Parser TopLvlExpression
parseNormalExpression = NormalExpression <$> parseExpression

parseTopLvlExpression :: Parser TopLvlExpression
parseTopLvlExpression = parseDefinition <|> parseNormalExpression

