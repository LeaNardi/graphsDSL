module Parser.Core where

import Text.Parsec ( notFollowedBy, choice)
import Text.ParserCombinators.Parsec ( try, sepBy, (<|>), Parser, option, many )
import Text.Parsec.Token ( GenTokenParser( integer, reserved, identifier, brackets, parens, stringLiteral, reservedOp, comma), float )
import Data.Functor (($>))

import Parser.Lexer ( gdsl )
import ASTGraphs ( Expr(..), Comm(..), BinOpType(..), CompOpType(..), FunctionType(..), Variable )

-- ============
-- parseComm

parseComm :: Parser Comm
parseComm = do
  first <- parseSimpleComm
  rest <- many (reservedOp gdsl ";" >> parseSimpleComm)
  return $ foldl Seq first rest

parseSimpleComm :: Parser Comm
parseSimpleComm = try parseSkip
               <|> try parseCond
               <|> try parseWhile
               <|> try parseFor
               <|> try parsePrint
               <|> try parseAssignment
               <|> try parseVisualize
               <|> try parseForNeighbors
               <|> try parseForNeighbors
               <|> try parseForNodes
               <|> try parseForEdges
               <|> try parseForIncident
               <|> try parseForComponent

-- =====================================
-- Parser de expresiones generales

parseExpr :: Parser Expr
parseExpr = try parseConditional
          <|> try parseLogical

parseConditional :: Parser Expr
parseConditional = do cond <- parseLogical
                      reservedOp gdsl "?"
                      thenExpr <- parseExpr
                      reservedOp gdsl ":"
                      elseExpr <- parseExpr
                      return $ Question cond thenExpr elseExpr
  

parseLogical :: Parser Expr
parseLogical = do
  left <- parseComparison
  rest <- many ((,) <$> ((reservedOp gdsl "&&" $> Comparison And)
                      <|> (reservedOp gdsl "||" $> Comparison Or))
                    <*> parseComparison)
  return $ foldl (\acc (op, right) -> op acc right) left rest


parseComparison :: Parser Expr
parseComparison = try (do
  left <- parseArithmetic
  op <- (reservedOp gdsl "==" $> Comparison Eq)
    <|> (reservedOp gdsl "<" $> Comparison Lt)
    <|> (reservedOp gdsl ">" $> Comparison Gt)
  right <- parseArithmetic
  return $ op left right)
                        <|> parseArithmetic

parseArithmetic :: Parser Expr
parseArithmetic = do
  left <- parseTerm
  rest <- many ((,) <$> ((reservedOp gdsl "+" $> BinOp Plus)
                      <|> (reservedOp gdsl "-" $> BinOp Minus))
                    <*> parseTerm)
  return $ foldl (\acc (op, right) -> op acc right) left rest

parseTerm :: Parser Expr
parseTerm = do
  left <- parseFactor
  rest <- many ((,) <$> ((reservedOp gdsl "*" $> BinOp Times)
                      <|> (reservedOp gdsl "/" $> BinOp Div)
                      <|> (reservedOp gdsl "%" $> BinOp Mod))
                    <*> parseFactor)
  return $ foldl (\acc (op, right) -> op acc right) left rest

parseFactor :: Parser Expr
parseFactor = try parseNumber
           <|> try parseBool
           <|> try parseString
           <|> try parseEmptyLiterals
           <|> try parseFunction
           <|> try parseVariable
           <|> try parseValuedGraph
           <|> try parseValuedEdge
           <|> try parseQueue
           <|> try parseList
           <|> try parseUnionFind
           <|> try parseUnary
           <|> parseParens

-- ===============
-- Parsers de literales

parseNumber :: Parser Expr
parseNumber = try parseFloat <|> parseInt
  where
    parseInt = IntLit <$> integer gdsl
    parseFloat = FloatLit . realToFrac <$> float gdsl

parseBool :: Parser Expr
parseBool = (reserved gdsl "true" $> BoolLit True)
         <|> (reserved gdsl "false" $> BoolLit False)

parseString :: Parser Expr  
parseString = StringLit <$> stringLiteral gdsl

parseEmptyLiterals :: Parser Expr
parseEmptyLiterals = (reserved gdsl "emptyList" $> EmptyList)
                  <|> (reserved gdsl "emptyQueue" $> EmptyQueue)

parseVariable :: Parser Expr
parseVariable = Var <$> identifier gdsl

parseParens :: Parser Expr
parseParens = parens gdsl parseExpr

parseUnary :: Parser Expr
parseUnary = (reservedOp gdsl "-" >> UMinus <$> parseFactor)
          <|> (reservedOp gdsl "!" >> Not <$> parseFactor)

-- =============================================================
-- Llamadas a funciones

parseFunction :: Parser Expr
parseFunction = do
  funName <- identifier gdsl
  args <- parens gdsl (sepBy parseExpr (comma gdsl))
  case funName of
    -- Operaciones de grafos
    "addNode" -> return $ FunCall AddNode args
    "deleteNode" -> return $ FunCall DeleteNode args
    "addEdge" -> return $ FunCall AddEdge args
    "deleteEdge" -> return $ FunCall DeleteEdge args
    "graphComplement" -> return $ FunCall GraphComplement args
    "graphUnion" -> return $ FunCall GraphUnion args
    "graphIntersection" -> return $ FunCall GraphIntersection args
    "getEdges" -> return $ FunCall GetEdges args
    "adjacentNodes" -> return $ FunCall AdjacentNodes args
    "adjacentEdges" -> return $ FunCall AdjacentEdges args
    "metricClosure" -> return $ FunCall MetricClosure args
    "metricClosurePaths" -> return $ FunCall MetricClosurePaths args
    "getConnectedComponents" -> return $ FunCall GetConnectedComponents args
    
    -- Operaciones sobre aristas
    "getWeight" -> return $ FunCall GetWeight args
    "getNode1" -> return $ FunCall GetNode1 args
    "getNode2" -> return $ FunCall GetNode2 args
    
    -- Operaciones de List
    "lenList" -> return $ FunCall LenList args
    "appendList" -> return $ FunCall AppendList args
    "consList" -> return $ FunCall ConsList args
    "concatList" -> return $ FunCall ConcatList args
    "headList" -> return $ FunCall HeadList args
    "lastList" -> return $ FunCall LastList args
    "tailList" -> return $ FunCall TailList args
    "initList" -> return $ FunCall InitList args
    "reverseList" -> return $ FunCall ReverseList args
    "sortByWeight" -> return $ FunCall SortByWeight args
    
    -- Operaciones de Queue
    "queueLen" -> return $ FunCall QueueLen args
    "enqueue" -> return $ FunCall Enqueue args
    "dequeue" -> return $ FunCall Dequeue args
    "peek" -> return $ FunCall Peek args
    "isEmptyQueue" -> return $ FunCall IsEmptyQueue args
    
    -- Operaciones de UnionFind
    "union" -> return $ FunCall Union args
    "find" -> return $ FunCall Find args
    
    -- Booleanos
    "esCiclico" -> return $ FunCall EsCiclico args
    "esConexo" -> return $ FunCall EsConexo args
    "inList" -> return $ FunCall InList args
    "isEmptyList" -> return $ FunCall IsEmptyList args
    
    -- Operaciones de NodeMap
    "getNodeMap" -> return $ FunCall GetNodeMap args
    "getValue" -> return $ FunCall GetValue args
    "setValue" -> return $ FunCall SetValue args
    "getNodes" -> return $ FunCall GetNodes args 
    
    _ -> fail $ "Funcion no definida: " ++ funName

-- ====================
-- Constructores de grafos

parseValuedGraph :: Parser Expr
parseValuedGraph = do
  reserved gdsl "graph"
  nodes <- brackets gdsl $ sepBy parseNodeAssoc (comma gdsl)
  return $ ValuedGraph nodes
  where
    parseNodeAssoc = parens gdsl $ do
      node <- parseExpr
      comma gdsl
      edges <- brackets gdsl $ sepBy parseEdgeAssoc (comma gdsl)
      return (node, edges)
    parseEdgeAssoc = parens gdsl $ do
      target <- parseExpr
      comma gdsl
      weight <- parseExpr
      return (target, weight)

parseValuedEdge :: Parser Expr
parseValuedEdge = do
  reserved gdsl "edge"
  parens gdsl $ do
    n1 <- parseExpr
    comma gdsl
    n2 <- parseExpr
    comma gdsl
    w <- parseExpr
    return $ ValuedEdge n1 n2 w

parseList :: Parser Expr
parseList = brackets gdsl $ ListConstruct <$> sepBy parseExpr (comma gdsl)

parseQueue :: Parser Expr
parseQueue = do
  reserved gdsl "queue"
  elements <- brackets gdsl $ sepBy parseExpr (comma gdsl)
  return $ QueueConstruct elements

parseUnionFind :: Parser Expr
parseUnionFind = do
  reserved gdsl "unionfind"
  pairs <- brackets gdsl $ sepBy parseUFPair (comma gdsl)
  return $ UnionFindConstruct pairs
  where
    parseUFPair = parens gdsl $ do
      node <- parseExpr
      comma gdsl
      parent <- parseExpr
      return (node, parent)

-- ===============
-- Parsers de Comandos

parseSkip :: Parser Comm
parseSkip = reserved gdsl "skip" $> Skip

parseCommUntil :: [String] -> Parser Comm
parseCommUntil stops = do
    first <- parseSimpleComm
    rest  <- many (try parseSeq)
    return (foldSeq (first : rest))
  where
    parseSeq = do
        reservedOp gdsl ";"
        notFollowedBy stopMarker
        parseSimpleComm

    stopMarker = choice (map (reserved gdsl) stops)

    foldSeq [c]    = c
    foldSeq (c:cs) = Seq c (foldSeq cs)
    foldSeq []     = Skip

parseCond :: Parser Comm
parseCond = do
    reserved gdsl "cond"
    condExpr <- parseExpr
    reserved gdsl "then"
    trueBranch <- parseCommUntil ["else", "end"]
    parseElse condExpr trueBranch
  where
    parseElse condExpr trueBranch =
          try (do
                  reserved gdsl "else"
                  falseBranch <- parseCommUntil ["end"]
                  reserved gdsl "end"
                  return (Cond condExpr trueBranch falseBranch)
              )
          <|> (do
                  reserved gdsl "end"
                  return (Cond condExpr trueBranch Skip)
              )

parseWhile :: Parser Comm
parseWhile = do
  reserved gdsl "while"
  cond <- parseExpr
  reserved gdsl "do"
  body <- parseComm
  reserved gdsl "end"
  return $ While cond body

parseFor :: Parser Comm
parseFor = do
  reserved gdsl "for"
  var <- identifier gdsl
  reserved gdsl "in"
  list <- parseExpr
  reserved gdsl "do"
  body <- parseComm
  reserved gdsl "end"
  return $ For var list body

parsePrint :: Parser Comm
parsePrint = do
  reserved gdsl "print"
  expr <- parseExpr
  return $ Print expr

parseAssignment :: Parser Comm
parseAssignment = do
  var <- identifier gdsl
  reservedOp gdsl ":="
  expr <- parseExpr
  return $ AssignValue var expr

parseVisualize :: Parser Comm
parseVisualize = do
  reserved gdsl "visualize" 
  args <- parens gdsl $ sepBy parseExpr (comma gdsl) --2 argumentos
  case args of
    [graphExpr, fileNameExpr] -> return $ Visualize graphExpr fileNameExpr
    _ -> fail "visualize espera 2 argumentos: visualize(grafo, \"nombre.png\""

parseForNeighbors :: Parser Comm
parseForNeighbors = do
    reserved gdsl "forNeighbors"
    nodeVar <- identifier gdsl
    reserved gdsl "in"
    graphExpr <- parseExpr
    reserved gdsl "from"
    startNodeExpr <- parseExpr
    reserved gdsl "upto"
    limitExpr <- parseExpr
    reserved gdsl "do"
    bodyComm <- parseComm
    reserved gdsl "end"
    return $ ForNeighbors nodeVar graphExpr startNodeExpr limitExpr bodyComm

parseForNodes :: Parser Comm
parseForNodes = do
    reserved gdsl "forNodes"
    nodeVar <- identifier gdsl
    reserved gdsl "in"
    graphExpr <- parseExpr
    reserved gdsl "do"
    bodyComm <- parseComm
    reserved gdsl "end"
    return $ ForNodes nodeVar graphExpr bodyComm

parseForEdges :: Parser Comm
parseForEdges = do
    reserved gdsl "forEdges"
    edgeVar <- identifier gdsl
    reserved gdsl "in"
    graphExpr <- parseExpr
    reserved gdsl "do"
    bodyComm <- parseComm
    reserved gdsl "end"
    return $ ForEdges edgeVar graphExpr bodyComm

parseForIncident :: Parser Comm
parseForIncident = do
    reserved gdsl "forIncident"
    edgeVar <- identifier gdsl
    reserved gdsl "in"
    graphExpr <- parseExpr
    reserved gdsl "on"
    nodeExpr <- parseExpr
    reserved gdsl "do"
    bodyComm <- parseComm
    reserved gdsl "end"
    return $ ForIncident edgeVar graphExpr nodeExpr bodyComm

parseForComponent :: Parser Comm
parseForComponent = do
    reserved gdsl "forComponent"
    graphVar <- identifier gdsl
    reserved gdsl "in"
    graphExpr <- parseExpr
    reserved gdsl "do"
    bodyComm <- parseComm
    reserved gdsl "end"
    return $ ForComponent graphVar graphExpr bodyComm