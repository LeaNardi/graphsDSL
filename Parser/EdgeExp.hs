module Parser.EdgeExp 
  ( parseEdgeExp
  , parseEdgeExpWith
  ) where

import Text.ParserCombinators.Parsec ( (<|>), try, Parser )
import Text.Parsec.Token ( GenTokenParser( reserved, identifier) )

import Parser.Lexer ( gdsl )
import ASTGraphs ( EdgeExp(..), ListEdgeExp(..) )

-- Main parser for external use (will be wired up in Core.hs)
parseEdgeExp :: Parser EdgeExp
parseEdgeExp = error "parseEdgeExp: should be defined in Core.hs with proper wiring"

-- Parameterized parser
parseEdgeExpWith :: Parser ListEdgeExp -> Parser EdgeExp
parseEdgeExpWith listEdgeParser = 
  try (VarEdge <$> identifier gdsl)
  <|> try (parseHeadEdgeWith listEdgeParser)

parseHeadEdgeWith :: Parser ListEdgeExp -> Parser EdgeExp
parseHeadEdgeWith listEdgeParser = do
  reserved gdsl "head"
  edges <- listEdgeParser
  return (HeadEdge edges)