module Parser.Commands 
  ( parseComm
  , parseSimpleComm
  , parseLetValue
    , parseSkip
    , parseCond
    , parseWhile
    , parseFor
    , parsePrint
  ) where

import Text.ParserCombinators.Parsec ( chainl1, (<|>), try, Parser )
import Text.Parsec.Token ( GenTokenParser( reserved, identifier, reservedOp) )
import Data.Functor (($>))

import Parser.Lexer ( gdsl )
import ASTGraphs ( Comm(..), Variable, ValueExp(..) )
import Parser.BoolExp ( parseBoolExp )
import Parser.IntExp ( parseIntExp )
import Parser.GraphExp ( parseGraphExp )
import Parser.ValueExp ( parseValueExp )
import Parser.ListExp ( parseListExp )

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

parseLetValue :: Parser Comm
parseLetValue = do
  var <- identifier gdsl
  reservedOp gdsl ":="
  try (parseLetGraphVal var) <|> try (parseLetIntVal var)

parseLetIntVal :: Variable -> Parser Comm
parseLetIntVal var = LetValue var . IntVal <$> parseIntExp

parseLetGraphVal :: Variable -> Parser Comm
parseLetGraphVal var = LetValue var . GraphVal <$> parseGraphExp

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