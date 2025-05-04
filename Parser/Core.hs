module Parser.Core where

import Text.ParserCombinators.Parsec ( chainl1, sepBy, (<|>), try, Parser )
import Text.Parsec.Token ( GenTokenParser( integer, reserved, identifier, brackets, parens, stringLiteral, reservedOp) )
import Data.Functor (($>))
import Debug.Trace (trace)

import Parser.Lexer ( gdsl )
import ASTGraphs ( BoolExp(..), Comm(..), GraphExp(..), IntExp(..), Node, ValueExp(GraphVal, IntVal), Variable, Weight )


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
               <|> try parseRepeat
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


parseRepeat :: Parser Comm
parseRepeat = do
  reserved gdsl "repeat"
  body <- parseComm
  reserved gdsl "until"
  cond <- parseBoolExp
  reserved gdsl "end"
  return (Repeat cond body)


parseLetValue :: Parser Comm
parseLetValue = do
  var <- identifier gdsl
  reservedOp gdsl ":="
  try (parseLetIntVal var) <|> try (parseLetGraphVal var)


parseLetIntVal :: Variable -> Parser Comm
parseLetIntVal var = LetValue var . IntVal <$> parseIntExp


parseLetGraphVal :: Variable -> Parser Comm
parseLetGraphVal var = LetValue var . GraphVal <$> parseGraphExp


parseGraphExp :: Parser GraphExp
parseGraphExp = try parseKruskal
             <|> try parseAddUndirectedEdge
             <|> try parseAddDirectedEdge
             <|> try parseAddNode
             <|> try parseValuedGraph
             <|> try parseVarGraph


parseKruskal :: Parser GraphExp
parseKruskal = Kruskal <$> (reserved gdsl "kruskal" *> parseGraphExp)


parseAddNode :: Parser GraphExp
parseAddNode = do
  reserved gdsl "addnode"
  g <- parseGraphExp
  n <- stringLiteral gdsl
  return (AddNode g n)


parseAddDirectedEdge :: Parser GraphExp
parseAddDirectedEdge = do
  reserved gdsl "addedge"
  g <- parseGraphExp
  n1 <- stringLiteral gdsl
  n2 <- stringLiteral gdsl
  w <- integer gdsl
  return (AddDirectedEdge g (n1, n2) w)


parseAddUndirectedEdge :: Parser GraphExp
parseAddUndirectedEdge = do
  reserved gdsl "adduedge"
  g <- parseGraphExp
  n1 <- stringLiteral gdsl
  n2 <- stringLiteral gdsl
  w <- integer gdsl
  return (AddUndirectedEdge g (n1, n2) w)


parseVarGraph :: Parser GraphExp
parseVarGraph = VarGraph <$> identifier gdsl


parseValuedGraph :: Parser GraphExp
parseValuedGraph = do
  reserved gdsl "newgraph"
  ValuedGraph <$> parseNodeList


parseNodeList :: Parser [(Node, [(Node, Weight)])]
parseNodeList = brackets gdsl (parseNodeEntry `sepBy` reservedOp gdsl ",")


parseNodeEntry :: Parser (Node, [(Node, Weight)])
parseNodeEntry = parens gdsl (do
                                node <- stringLiteral gdsl
                                reservedOp gdsl ","
                                edges <- parseEdgeList
                                return (node, edges))


parseEdgeList :: Parser [(Node, Weight)]
parseEdgeList = brackets gdsl (parseEdgeEntry `sepBy` reservedOp gdsl ",")


parseEdgeEntry :: Parser (Node, Weight)
parseEdgeEntry = parens gdsl (do
                                destNode <- stringLiteral gdsl
                                reservedOp gdsl ","
                                weight <- integer gdsl
                                return (destNode, weight))