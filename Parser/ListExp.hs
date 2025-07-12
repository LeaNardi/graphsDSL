module Parser.ListExp 
  ( parseListExp
  , parseNewList
  , parseVarList
    , parseAdjacentNodes
    , parseAddList
    , parseDequeueNode
    , parseQueueExp
    , parseNewQueue
    , parseVarQueue
    , parseEnqueue
    , parseVarEdge
    , parseHeadEdge
    , parseListEdgeExp
    , parseVarEdgeList
    , parseGetEdges
    , parseSortByWeight
    , parseUnionFindExp
    , parseNewUnionFind
    , parseVarUnionFind
    , parseUnion
    , parseUnionFindList
    , parseUnionFindEntry
  ) where

import Text.ParserCombinators.Parsec ( chainl1, sepBy, (<|>), try, Parser )
import Text.Parsec.Token ( GenTokenParser( integer, reserved, identifier, brackets, parens, stringLiteral, reservedOp) )
import Data.Functor (($>))

import Parser.Lexer ( gdsl )
import ASTGraphs ( ListExp(..), NodeExp(..), GraphExp(..), 
                   QueueExp(..), EdgeExp(..), ListEdgeExp(..), 
                   UnionFindExp(..), IntExp(..) )
import Parser.GraphExp ( parseGraphExp, parseNodeExp, parseEdgeExp )

parseListExp :: Parser ListExp
parseListExp = try parseNewList
            <|> try parseVarList
            <|> try parseAdjacentNodes
            <|> try parseAddList

parseNewList :: Parser ListExp
parseNewList = reserved gdsl "newList" $> NewList

parseVarList :: Parser ListExp
parseVarList = VarList <$> identifier gdsl

parseAdjacentNodes :: Parser ListExp
parseAdjacentNodes = do
  reserved gdsl "adjacentNodes"
  graph <- parseGraphExp
  node <- parseNodeExp
  return (AdjacentNodes graph node)

parseAddList :: Parser ListExp
parseAddList = do
  reserved gdsl "addList"
  list <- parseListExp
  node <- parseNodeExp
  return (AddList list node)



parseDequeueNode :: Parser NodeExp
parseDequeueNode = do
  reserved gdsl "dequeue"
  queue <- parseQueueExp
  return (DequeueNode queue)

parseQueueExp :: Parser QueueExp
parseQueueExp = try parseNewQueue
             <|> try parseVarQueue
             <|> try parseEnqueue

parseNewQueue :: Parser QueueExp
parseNewQueue = reserved gdsl "newQueue" $> NewQueue

parseVarQueue :: Parser QueueExp
parseVarQueue = VarQueue <$> identifier gdsl

parseEnqueue :: Parser QueueExp
parseEnqueue = do
  reserved gdsl "enqueue"
  queue <- parseQueueExp
  node <- parseNodeExp
  return (Enqueue queue node)



parseHeadEdge :: Parser EdgeExp
parseHeadEdge = do
  reserved gdsl "head"
  edges <- parseListEdgeExp
  return (HeadEdge edges)

parseListEdgeExp :: Parser ListEdgeExp
parseListEdgeExp = try parseVarEdgeList
                <|> try parseGetEdges
                <|> try parseSortByWeight

parseVarEdgeList :: Parser ListEdgeExp
parseVarEdgeList = VarEdgeList <$> identifier gdsl

parseGetEdges :: Parser ListEdgeExp
parseGetEdges = do
  reserved gdsl "get_edges"
  graph <- parseGraphExp
  return (GetEdges graph)

parseSortByWeight :: Parser ListEdgeExp
parseSortByWeight = do
  reserved gdsl "sort_by_weight"
  edges <- parseListEdgeExp
  return (SortByWeight edges)

parseUnionFindExp :: Parser UnionFindExp
parseUnionFindExp = try parseNewUnionFind
                 <|> try parseVarUnionFind
                 <|> try parseUnion

parseNewUnionFind :: Parser UnionFindExp
parseNewUnionFind = do
  reserved gdsl "newunionfind"
  ValuedUnionFind <$> parseUnionFindList

parseVarUnionFind :: Parser UnionFindExp
parseVarUnionFind = VarUnionFind <$> identifier gdsl

parseUnion :: Parser UnionFindExp
parseUnion = do
  reserved gdsl "union"
  node1 <- parseNodeExp
  node2 <- parseNodeExp
  uf <- parseUnionFindExp
  return (Union node1 node2 uf)

parseUnionFindList :: Parser [(NodeExp, NodeExp)]
parseUnionFindList = brackets gdsl (parseUnionFindEntry `sepBy` reservedOp gdsl ",")

parseUnionFindEntry :: Parser (NodeExp, NodeExp)
parseUnionFindEntry = parens gdsl (do
                                    node1 <- stringLiteral gdsl
                                    reservedOp gdsl ","
                                    node2 <- stringLiteral gdsl
                                    return (NodeLit node1, NodeLit node2))