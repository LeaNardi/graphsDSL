module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import ASTGraphs

-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do 
                  whiteSpace gdsl
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
gdsl :: TokenParser u
gdsl = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , opLetter      = char '='
                                  , reservedNames = ["true","false","skip","if",
                                                     "then","else","end",
                                                     "while","do", "repeat"]})
----------------------------------
--- Parser de expressiones enteras
-----------------------------------
intexp :: Parser IntExp
intexp  = chainl1 term addopp

term = chainl1 factor multopp

factor = try (parens gdsl intexp)
         <|> try (do reservedOp gdsl "-"
                     f <- factor
                     return (UMinus f))
         <|> (do n <- integer gdsl
                 return (Const n)
              <|> do str <- identifier gdsl
                     return (Var str))
                 
multopp = do try (reservedOp gdsl "*")
             return Times
          <|> do try (reservedOp gdsl "/")
                 return Div
 
addopp = do try (reservedOp gdsl "+")
            return Plus
         <|> do try (reservedOp gdsl "-")
                return Minus

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------
boolexp :: Parser BoolExp
boolexp  = chainl1 boolexp2 (try (do reservedOp gdsl "|"
                                     return Or))

boolexp2 = chainl1 boolexp3 (try (do reservedOp gdsl "&"
                                     return And))

boolexp3 = try (parens gdsl boolexp)
           <|> try (do reservedOp gdsl "~"
                       b <- boolexp3
                       return (Not b))
           <|> intcomp
           <|> boolvalue

intcomp = try (do i <- intexp
                  c <- compopp
                  j <- intexp
                  return (c i j))

compopp = try (do reservedOp gdsl "="
                  return Eq)
          <|> try (do reservedOp gdsl "<"
                      return Lt)
          <|> try (do reservedOp gdsl ">"
                      return Gt)

boolvalue = try (do reserved gdsl "true"
                    return BTrue)
            <|> try (do reserved gdsl "false"
                        return BFalse)

-----------------------------------
-- Parser para grafos
-----------------------------------
graph :: Parser Graph
graph = do
  reservedOp gdsl "Graph"
  nodes <- nodeList
  return (Graph nodes)

nodeList :: Parser [(Node, [(Node, Weight)])]
nodeList = do 
    reservedOp gdsl "["
    nodes <- nodeEntry `sepBy` (reservedOp gdsl ",")
    reservedOp gdsl "]"
    return nodes

nodeEntry :: Parser (Node, [(Node, Weight)])
nodeEntry = do
  reservedOp gdsl "("
  node <- integer gdsl
  reservedOp gdsl ","
  edges <- edgeList
  reservedOp gdsl ")"
  return (node, edges)

edgeList :: Parser [(Node, Weight)]
edgeList = do
  reservedOp gdsl "["
  edges <- edgeEntry `sepBy` (reservedOp gdsl ",")
  reservedOp gdsl "]"
  return edges

edgeEntry :: Parser (Node, Weight)
edgeEntry = do
  reservedOp gdsl "("
  destNode <- integer gdsl
  reservedOp gdsl ","
  weight <- float gdsl
  reservedOp gdsl ")"
  return (destNode, weight)


-----------------------------------
--- Parser de comandos
-----------------------------------
comm :: Parser Comm
comm = chainl1 comm2 (try (do reservedOp gdsl ";"
                              return Seq))

comm2 = try (do reserved gdsl "skip"
                return Skip)
        <|> try (do reserved gdsl "if"
                    cond <- boolexp
                    reserved gdsl "then"
                    case1 <- comm
                    reserved gdsl "else"
                    case2 <- comm
                    reserved gdsl "end"
                    return (Cond cond case1 case2))
        <|> try (do reserved gdsl "repeat"
                    c <- comm
                    reserved gdsl "until"
                    cond <- boolexp
                    reserved gdsl "end"
                    return (Repeat c cond))
        <|> try (do str <- identifier gdsl
                    reservedOp gdsl ":="
                    e <- graph
                    return (LetGraph str e))
        <|> try (do str <- identifier gdsl
                    reservedOp gdsl ":="
                    e <- intexp
                    return (Let str e))


------------------------------------
-- Funcion de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm) 