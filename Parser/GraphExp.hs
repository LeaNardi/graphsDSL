module Parser.GraphExp 
  ( parseGraphExp
  , parseValuedGraph
  , parseVarGraph
  , parseAddEdge
  , parseAddNode
  , parseGraphIntersection
  , parseGraphComplement
  ) where

import Text.ParserCombinators.Parsec ( sepBy, (<|>), try, Parser )
import Text.Parsec.Token ( GenTokenParser( integer, reserved, identifier, brackets, parens, stringLiteral, reservedOp) )
import Data.Functor (($>))

import Parser.Lexer ( gdsl )
import ASTGraphs ( GraphExp(..), NodeExp(..), IntExp(..), EdgeExp(..) )

parseGraphExp :: Parser GraphExp
parseGraphExp = try parseValuedGraph
             <|> try parseVarGraph
             <|> try parseAddEdge
             <|> try parseAddNode
             <|> try parseGraphIntersection
             <|> try parseGraphComplement

parseValuedGraph :: Parser GraphExp
parseValuedGraph = do
  reserved gdsl "newgraph"
  ValuedGraph <$> parseNodeList

parseVarGraph :: Parser GraphExp
parseVarGraph = VarGraph <$> identifier gdsl

parseAddEdge :: Parser GraphExp
parseAddEdge = do
  reserved gdsl "addedge"
  g <- parseGraphExp
  n1 <- stringLiteral gdsl
  n2 <- stringLiteral gdsl
  w <- integer gdsl
  let edge = ValuedEdge (NodeLit n1, NodeLit n2, Const w)
  return (AddEdge g edge)

parseAddNode :: Parser GraphExp
parseAddNode = do
  reserved gdsl "addnode"
  g <- parseGraphExp
  n <- stringLiteral gdsl
  return (AddNode g (NodeLit n))

parseGraphIntersection :: Parser GraphExp
parseGraphIntersection = do
  reserved gdsl "intersect"
  firstGraph <- parseGraphExp
  reserved gdsl "with"
  secondGraph <- parseGraphExp
  reserved gdsl "end"
  return (GraphIntersection firstGraph secondGraph)

parseGraphComplement :: Parser GraphExp
parseGraphComplement = do
  reserved gdsl "complement"
  givenGraph <- parseGraphExp
  return (GraphComplement givenGraph)

parseNodeList :: Parser [(NodeExp, [(NodeExp, IntExp)])]
parseNodeList = brackets gdsl (parseNodeEntry `sepBy` reservedOp gdsl ",")

parseNodeEntry :: Parser (NodeExp, [(NodeExp, IntExp)])
parseNodeEntry = parens gdsl (do
                                node <- stringLiteral gdsl
                                reservedOp gdsl ","
                                edges <- parseEdgeList
                                return (NodeLit node, edges))

parseEdgeList :: Parser [(NodeExp, IntExp)]
parseEdgeList = brackets gdsl (parseEdgeEntry `sepBy` reservedOp gdsl ",")

parseEdgeEntry :: Parser (NodeExp, IntExp)
parseEdgeEntry = parens gdsl (do
                                destNode <- stringLiteral gdsl
                                reservedOp gdsl ","
                                weight <- integer gdsl
                                return (NodeLit destNode, Const weight))