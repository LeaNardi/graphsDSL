module Parser.UnionFindExp 
  ( parseUnionFindExp
  , parseUnionFindExpWith
  ) where

import Text.ParserCombinators.Parsec ( sepBy, (<|>), try, Parser )
import Text.Parsec.Token ( GenTokenParser( reserved, identifier, brackets, parens, stringLiteral, reservedOp) )

import Parser.Lexer ( gdsl )
import ASTGraphs ( UnionFindExp(..), NodeExp(..) )

-- Main parser for external use (will be wired up in Core.hs)
parseUnionFindExp :: Parser UnionFindExp
parseUnionFindExp = error "parseUnionFindExp: should be defined in Core.hs with proper wiring"

-- Parameterized parser
parseUnionFindExpWith :: Parser NodeExp -> Parser UnionFindExp
parseUnionFindExpWith nodeParser = 
  try parseNewUnionFind
  <|> try parseVarUnionFind
  <|> try (parseUnionWith nodeParser)

parseNewUnionFind :: Parser UnionFindExp
parseNewUnionFind = do
  reserved gdsl "newunionfind"
  ValuedUnionFind <$> parseUnionFindList

parseVarUnionFind :: Parser UnionFindExp
parseVarUnionFind = VarUnionFind <$> identifier gdsl

parseUnionWith :: Parser NodeExp -> Parser UnionFindExp
parseUnionWith nodeParser = do
  reserved gdsl "union"
  node1 <- nodeParser
  node2 <- nodeParser
  uf <- parseUnionFindExp  -- This will be resolved in Core.hs
  return (Union node1 node2 uf)

parseUnionFindList :: Parser [(NodeExp, NodeExp)]
parseUnionFindList = brackets gdsl (parseUnionFindEntry `sepBy` reservedOp gdsl ",")

parseUnionFindEntry :: Parser (NodeExp, NodeExp)
parseUnionFindEntry = parens gdsl (do
                                    node1 <- stringLiteral gdsl
                                    reservedOp gdsl ","
                                    node2 <- stringLiteral gdsl
                                    return (NodeLit node1, NodeLit node2))