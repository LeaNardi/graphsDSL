module Parser.Core where

import Text.ParserCombinators.Parsec ( chainl1, sepBy, (<|>), try, Parser )
import Text.Parsec.Token ( GenTokenParser( integer, reserved, identifier, brackets, parens, stringLiteral, reservedOp) )
import Data.Functor (($>))
import Debug.Trace (trace, traceShow)

import Parser.Lexer ( gdsl )
import ASTGraphs (  
                    Variable, Weight,
                    Value(..),
                    Graph, Edge, Node, UnionFind, Queue,
                    IntExp(..),
                    BoolExp(..),
                    GraphExp(..),
                    Comm(..),
                    ValueExp(..),
                    ListEdgeExp(..),
                    ListExp(..),
                    QueueExp(..),
                    EdgeExp(..),
                    NodeExp(..),
                    UnionFindExp(..)
                  )


parseIntExp :: Parser IntExp
parseIntExp  = chainl1 parseMulExp parseAddOp


parseMulExp :: Parser IntExp
parseMulExp = chainl1 parseAtomIntExp parseMulOp


parseAtomIntExp :: Parser IntExp
parseAtomIntExp = parens gdsl parseIntExp
               <|> try (UMinus <$> (reservedOp gdsl "-" *> parseAtomIntExp))
               <|> try (Const <$> integer gdsl)
               <|> try (VarInt <$> identifier gdsl)
               <|> try parseQuestion


parseAddOp :: Parser (IntExp -> IntExp -> IntExp)
parseAddOp = reservedOp gdsl "+" $> Plus
          <|> reservedOp gdsl "-" $> Minus


parseMulOp :: Parser (IntExp -> IntExp -> IntExp)
parseMulOp = reservedOp gdsl "*" $> Times
          <|> reservedOp gdsl "/" $> Div
          <|> reservedOp gdsl "%" $> Mod


parseQuestion :: Parser IntExp
parseQuestion = do
                  cond <- parseBoolExp
                  reservedOp gdsl "?"
                  e1 <- parseIntExp
                  reservedOp gdsl ":"
                  Question cond e1 <$> parseIntExp


parseBoolExp :: Parser BoolExp
parseBoolExp = chainl1 parseAndExp parseOrOp


parseAndExp :: Parser BoolExp
parseAndExp = chainl1 parseAtomBoolExp parseAndOp


parseAtomBoolExp :: Parser BoolExp
parseAtomBoolExp = parens gdsl parseBoolExp
                <|> try (Not <$> (reservedOp gdsl "~" *> parseAtomBoolExp))
                <|> try (reserved gdsl "true"  $> BTrue)
                <|> try (reserved gdsl "false" $> BFalse)
                <|> try parseComparisonIntExp


parseOrOp :: Parser (BoolExp -> BoolExp -> BoolExp)
parseOrOp = reservedOp gdsl "|" $> Or


parseAndOp :: Parser (BoolExp -> BoolExp -> BoolExp)
parseAndOp = reservedOp gdsl "&" $> And


parseComparisonIntExp :: Parser BoolExp
parseComparisonIntExp = (\l op r -> op l r) <$> parseIntExp <*> parseComparisonOp <*> parseIntExp


parseComparisonOp :: Parser (IntExp -> IntExp -> BoolExp)
parseComparisonOp =  (reservedOp gdsl "=" $> Eq)
                 <|> (reservedOp gdsl "<" $> Lt)
                 <|> (reservedOp gdsl ">" $> Gt)


parseComm :: Parser Comm
parseComm = chainl1 parseSimpleComm parseSeqOp


parseSeqOp :: Parser (Comm -> Comm -> Comm)
parseSeqOp = reservedOp gdsl ";" $> Seq


parseSimpleComm :: Parser Comm
parseSimpleComm = try parseSkip
               <|> try parseCond
               <|> try parseWhile
               <|> try parseFor
               <|> try parsePrint
               <|> try parseLetValue


parseSkip :: Parser Comm
parseSkip = reserved gdsl "skip" $> Skip


parseCond :: Parser Comm
parseCond = do
  reserved gdsl "cond"
  cond <- parseBoolExp
  reserved gdsl "then"
  trueBranch <- parseComm
  reserved gdsl "else"
  falseBranch <- parseComm
  reserved gdsl "end"
  return (Cond cond trueBranch falseBranch)


parseWhile :: Parser Comm
parseWhile = do
  reserved gdsl "while"
  cond <- parseBoolExp
  reserved gdsl "do"
  body <- parseComm
  reserved gdsl "end"
  return (While cond body)


parseFor :: Parser Comm
parseFor = do
  reserved gdsl "for"
  var <- identifier gdsl
  reserved gdsl "in"
  list <- parseListExp
  reserved gdsl "do"
  body <- parseComm
  reserved gdsl "end"
  return (For var list body)


parsePrint :: Parser Comm
parsePrint = do
  reserved gdsl "print"
  expr <- parseValueExp
  return (Print expr)


parseLetValue :: Parser Comm
parseLetValue = do
  var <- identifier gdsl
  reservedOp gdsl ":="
  try (parseLetGraphVal var) <|> try (parseLetIntVal var)


parseLetIntVal :: Variable -> Parser Comm
parseLetIntVal var = LetValue var . IntVal <$> parseIntExp


parseLetGraphVal :: Variable -> Parser Comm
parseLetGraphVal var = LetValue var . GraphVal <$> parseGraphExp


parseGraphExp :: Parser GraphExp
parseGraphExp = try parseValuedGraph
             <|> try parseVarGraph
            --  <|> try parseKruskal
             <|> try parseAddEdge
            --  <|> try parseAddDirectedEdge
             <|> try parseAddNode
             <|> try parseGraphIntersection
             <|> try parseGraphComplement


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

-- parseKruskal :: Parser GraphExp
-- parseKruskal = Kruskal <$> (reserved gdsl "kruskal" *> parseGraphExp)


parseAddNode :: Parser GraphExp
parseAddNode = do
  reserved gdsl "addnode"
  g <- parseGraphExp
  n <- stringLiteral gdsl
  return (AddNode g (NodeLit n))


-- parseAddDirectedEdge :: Parser GraphExp
-- parseAddDirectedEdge = do
--   reserved gdsl "addedge"
--   g <- parseGraphExp
--   n1 <- stringLiteral gdsl
--   n2 <- stringLiteral gdsl
--   w <- integer gdsl
--   return (AddDirectedEdge g (n1, n2) w)


parseAddEdge :: Parser GraphExp
parseAddEdge = do
  reserved gdsl "addedge"
  g <- parseGraphExp
  n1 <- stringLiteral gdsl
  n2 <- stringLiteral gdsl
  w <- integer gdsl
  let edge = ValuedEdge (NodeLit n1, NodeLit n2, Const w)
  return (AddEdge g edge)


parseVarGraph :: Parser GraphExp
parseVarGraph = VarGraph <$> identifier gdsl


parseValuedGraph :: Parser GraphExp
parseValuedGraph = do
  reserved gdsl "newgraph"
  ValuedGraph <$> parseNodeList


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

parseValueExp :: Parser ValueExp
parseValueExp = try (IntVal <$> parseIntExp)
             <|> try (GraphVal <$> parseGraphExp)
             <|> try (NodeVal <$> parseNodeExp)
             <|> try (EdgeVal <$> parseEdgeExp)
             <|> try (ListVal <$> parseListExp)
             <|> try (QueueVal <$> parseQueueExp)
             <|> try (UnionFindVal <$> parseUnionFindExp)
             <|> try (ListEdgeVal <$> parseListEdgeExp)

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

parseNodeExp :: Parser NodeExp
parseNodeExp = try (NodeLit <$> stringLiteral gdsl)
            <|> try (VarNode <$> identifier gdsl)
            <|> try parseDequeueNode

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

parseEdgeExp :: Parser EdgeExp
parseEdgeExp = try parseVarEdge
            <|> try parseHeadEdge

parseVarEdge :: Parser EdgeExp
parseVarEdge = VarEdge <$> identifier gdsl

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