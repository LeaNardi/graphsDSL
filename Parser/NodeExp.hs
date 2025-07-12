module Parser.NodeExp 
  ( parseNodeExp
  , parseNodeExpWith
  ) where

import Text.ParserCombinators.Parsec ( (<|>), try, Parser )
import Text.Parsec.Token ( GenTokenParser( reserved, identifier, stringLiteral) )

import Parser.Lexer ( gdsl )
import ASTGraphs ( NodeExp(..), QueueExp(..) )

-- Main parser for external use (will be wired up in Core.hs)
parseNodeExp :: Parser NodeExp
parseNodeExp = error "parseNodeExp: should be defined in Core.hs with proper wiring"

-- Parameterized parser
parseNodeExpWith :: Parser QueueExp -> Parser NodeExp
parseNodeExpWith queueParser = 
  try (NodeLit <$> stringLiteral gdsl)
  <|> try (VarNode <$> identifier gdsl)
  <|> try (parseDequeueNodeWith queueParser)

parseDequeueNodeWith :: Parser QueueExp -> Parser NodeExp
parseDequeueNodeWith queueParser = do
  reserved gdsl "dequeue"
  queue <- queueParser
  return (DequeueNode queue)