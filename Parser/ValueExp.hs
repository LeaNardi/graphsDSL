module Parser.ValueExp 
  ( parseValueExp
  , parseValueExpWith
  ) where

import Text.ParserCombinators.Parsec ( (<|>), try, Parser )
import ASTGraphs ( ValueExp(..), IntExp(..), GraphExp(..), NodeExp(..), EdgeExp(..), ListExp(..), QueueExp(..), UnionFindExp(..), ListEdgeExp(..) )

-- Main parser for external use (will be wired up in Core.hs)
parseValueExp :: Parser ValueExp
parseValueExp = error "parseValueExp: should be defined in Core.hs with proper wiring"

-- Parameterized parser
parseValueExpWith :: Parser IntExp -> Parser GraphExp -> Parser NodeExp -> Parser EdgeExp -> Parser ListExp -> Parser QueueExp -> Parser UnionFindExp -> Parser ListEdgeExp -> Parser ValueExp
parseValueExpWith intParser graphParser nodeParser edgeParser listParser queueParser unionFindParser listEdgeParser = 
  try (IntVal <$> intParser)
  <|> try (GraphVal <$> graphParser)
  <|> try (NodeVal <$> nodeParser)
  <|> try (EdgeVal <$> edgeParser)
  <|> try (ListVal <$> listParser)
  <|> try (QueueVal <$> queueParser)
  <|> try (UnionFindVal <$> unionFindParser)
  <|> try (ListEdgeVal <$> listEdgeParser)