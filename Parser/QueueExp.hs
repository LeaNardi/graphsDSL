module Parser.QueueExp 
  ( parseQueueExp
  , parseQueueExpWith
  ) where

import Text.ParserCombinators.Parsec ( (<|>), try, Parser )
import Text.Parsec.Token ( GenTokenParser( reserved, identifier) )
import Data.Functor (($>))

import Parser.Lexer ( gdsl )
import ASTGraphs ( QueueExp(..), NodeExp(..) )

-- Main parser for external use (will be wired up in Core.hs)
parseQueueExp :: Parser QueueExp
parseQueueExp = error "parseQueueExp: should be defined in Core.hs with proper wiring"

-- Parameterized parser
parseQueueExpWith :: Parser NodeExp -> Parser QueueExp
parseQueueExpWith nodeParser = 
  try parseNewQueue
  <|> try parseVarQueue
  <|> try (parseEnqueueWith nodeParser)

parseNewQueue :: Parser QueueExp
parseNewQueue = reserved gdsl "newQueue" $> NewQueue

parseVarQueue :: Parser QueueExp
parseVarQueue = VarQueue <$> identifier gdsl

parseEnqueueWith :: Parser NodeExp -> Parser QueueExp
parseEnqueueWith nodeParser = do
  reserved gdsl "enqueue"
  queue <- parseQueueExp  -- This will be resolved in Core.hs
  node <- nodeParser
  return (Enqueue queue node)