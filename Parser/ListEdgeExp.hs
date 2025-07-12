module Parser.ListEdgeExp 
  ( parseListEdgeExp
  , parseListEdgeExpWith
  ) where

import Text.ParserCombinators.Parsec ( (<|>), try, Parser )
import Text.Parsec.Token ( GenTokenParser( reserved, identifier) )

import Parser.Lexer ( gdsl )
import ASTGraphs ( ListEdgeExp(..), GraphExp(..) )

-- Main parser for external use (will be wired up in Core.hs)
parseListEdgeExp :: Parser ListEdgeExp
parseListEdgeExp = error "parseListEdgeExp: should be defined in Core.hs with proper wiring"

-- Parameterized parser
parseListEdgeExpWith :: Parser GraphExp -> Parser ListEdgeExp
parseListEdgeExpWith graphParser = 
  try (VarEdgeList <$> identifier gdsl)
  <|> try (parseGetEdgesWith graphParser)
  <|> try parseSortByWeight
  <|> try parseTailEdges

parseGetEdgesWith :: Parser GraphExp -> Parser ListEdgeExp
parseGetEdgesWith graphParser = do
  reserved gdsl "get_edges"
  graph <- graphParser
  return (GetEdges graph)

parseSortByWeight :: Parser ListEdgeExp
parseSortByWeight = do
  reserved gdsl "sort_by_weight"
  edges <- parseListEdgeExp  -- This will be resolved in Core.hs
  return (SortByWeight edges)

parseTailEdges :: Parser ListEdgeExp
parseTailEdges = do
  reserved gdsl "tail"
  edges <- parseListEdgeExp  -- This will be resolved in Core.hs
  return (TailEdges edges)