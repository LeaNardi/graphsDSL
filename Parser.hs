module Parser (parseComm) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr (buildExpressionParser, Assoc(..), Operator(..))
import ASTGraphs

-- Analizador de Tokens
gdsl :: TokenParser u
gdsl = makeTokenParser (emptyDef {
    reservedNames = ["let", "letgraph", "if", "then", "else", "repeat", "until", "kruskal", "emptygraph", "addedge", "true", "false", "not", "and", "or", "skip"],
    reservedOpNames = ["=", ";", "+", "-", "*", "/", "%", "==", "<", ">", "not", "and", "or", "{", "}"]
})


intExp :: Parser IntExp
intExp = buildExpressionParser table term
  where
    table = [ [Prefix (reservedOp gdsl "-" >> return UMinus)]
            , [Infix  (reservedOp gdsl "*" >> return Times) AssocLeft]
            , [Infix  (reservedOp gdsl "/" >> return Div) AssocLeft]
            , [Infix  (reservedOp gdsl "%" >> return Mod) AssocLeft]
            , [Infix  (reservedOp gdsl "+" >> return Plus) AssocLeft]
            , [Infix  (reservedOp gdsl "-" >> return Minus) AssocLeft]
            ]
    term = parens gdsl intExp
        <|> fmap Var (identifier gdsl)
        <|> fmap (Const . toInteger) (natural gdsl)

boolExp :: Parser BoolExp
boolExp = buildExpressionParser table bterm
  where
    table = [ [Prefix (reservedOp gdsl "not" >> return Not)]
            , [Infix  (reservedOp gdsl "and" >> return And) AssocLeft]
            , [Infix  (reservedOp gdsl "or"  >> return Or)  AssocLeft]
            ]
    bterm = parens gdsl boolExp
        <|> (reserved gdsl "true" >> return BTrue)
        <|> (reserved gdsl "false" >> return BFalse)
        <|> try (do
                e1 <- intExp
                op <- relop
                e2 <- intExp
                return (op e1 e2)
            )

relop :: Parser (IntExp -> IntExp -> BoolExp)
relop =   (reservedOp gdsl "==" >> return Eq)
      <|> (reservedOp gdsl "<"  >> return Lt)
      <|> (reservedOp gdsl ">"  >> return Gt)

-- Parser para comandos
comm :: Parser Comm
comm = do
  cs <- sepBy1 comm2 (reservedOp gdsl ";")
  return (foldr1 Seq cs)


comm2 :: Parser Comm
comm2 =  try parseSet 
     <|> try parseLet
     <|> try parseLetGraph
     <|> try parseIf
     <|> try parseRepeat
     <|> try parseKruskal
     <|> try parseEmptyGraph
     <|> try parseAddEdge
     <|> try parseSkip
     <|> parseBlock

parseBlock :: Parser Comm
parseBlock = braces gdsl comm

parseSkip :: Parser Comm
parseSkip = reserved gdsl "skip" >> return Skip

parseLet :: Parser Comm
parseLet = do
    reserved gdsl "let"
    v <- identifier gdsl
    reservedOp gdsl "="
    e <- intExp
    return (Let v e)

parseLetGraph :: Parser Comm
parseLetGraph = do
    reserved gdsl "letgraph"
    v <- identifier gdsl
    reservedOp gdsl "="
    g <- graph
    return (LetGraph v g)

parseIf :: Parser Comm
parseIf = do
    reserved gdsl "if"
    b <- boolExp
    reserved gdsl "then"
    c1 <- comm
    reserved gdsl "else"
    c2 <- comm
    return (Cond b c1 c2)

parseRepeat :: Parser Comm
parseRepeat = do
    reserved gdsl "repeat"
    c <- braces gdsl comm
    reserved gdsl "until"
    b <- boolExp
    return (Repeat c b)

parseKruskal :: Parser Comm
parseKruskal = do
    reserved gdsl "kruskal"
    gvar <- identifier gdsl
    return (GraphOperation (Kruskal gvar))
      
parseComm :: String -> String -> Either ParseError Comm
parseComm = parse (whiteSpace gdsl >> (parseBlock <|> comm))

graph :: Parser Graph
graph = do
    pairs <- brackets gdsl (commaSep gdsl nodeAdj)
    return $ Graph pairs

nodeAdj :: Parser (Node, [(Node, Weight)])
nodeAdj = parens gdsl $ do
    n <- nodeId
    comma gdsl
    adjs <- brackets gdsl (commaSep gdsl edge)
    return (n, adjs)

edge :: Parser (Node, Weight)
edge = parens gdsl $ do
    n <- nodeId
    comma gdsl
    w <- float gdsl
    return (n, w)

nodeId :: Parser Node
nodeId = identifier gdsl <|> stringLiteral gdsl

-- Falla con el tema de nombres repetidos

parseEmptyGraph :: Parser Comm
parseEmptyGraph = do
    reserved gdsl "emptygraph"
    v <- identifier gdsl
    return (GraphOperation (EmptyGraph v))

parseAddEdge :: Parser Comm
parseAddEdge = do
    reserved gdsl "addedge"
    g <- identifier gdsl
    n1 <- intExp
    n2 <- intExp
    w  <- intExp
    return (GraphOperation (AddEdge g n1 n2 w))

parseSet :: Parser Comm
parseSet = do
    reserved gdsl "set"
    v <- identifier gdsl
    reservedOp gdsl "="
    e <- intExp
    return (Set v e)