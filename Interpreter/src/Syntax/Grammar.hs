module Syntax.Grammar where

import Syntax.Expression
import Syntax.Parser
import Control.Applicative
    (Applicative (liftA2), Alternative ((<|>), many, some))

parseProgram :: String -> Maybe [Expression]
parseProgram = parse (many (parseTopLevel <* many whiteSpace) <* eof)

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
parseExpression = parseApplication <|> parseLambda <|> parseVar

parseDefinition :: Parser Expression
parseDefinition = liftA2
                    Definition
                    (word <* equals)
                    parseExpression

parseTopLevel :: Parser Expression
parseTopLevel = parseDefinition <|> parseExpression
