module Parser.Commands 
  ( parseComm
  , parseCommWith
  , parseSimpleComm
  , parseLetValue
  ) where

import Text.ParserCombinators.Parsec ( chainl1, (<|>), try, Parser )
import Text.Parsec.Token ( GenTokenParser( reserved, identifier, reservedOp) )
import Data.Functor (($>))

import Parser.Lexer ( gdsl )
import ASTGraphs ( Comm(..), Variable, ValueExp(..), BoolExp(..), IntExp(..), GraphExp(..), ListExp(..) )
import Parser.GraphExp ( parseGraphExp )

-- Main parser for external use (will be wired up in Core.hs)
parseComm :: Parser Comm
parseComm = error "parseComm: should be defined in Core.hs with proper wiring"

-- Parameterized parser that takes other parsers as parameters
parseCommWith :: Parser BoolExp -> Parser IntExp -> Parser GraphExp -> Parser ValueExp -> Parser ListExp -> Parser Comm
parseCommWith boolParser intParser graphParser valueParser listParser = 
  chainl1 (parseSimpleCommWith boolParser intParser graphParser valueParser listParser) parseSeqOp

parseSimpleCommWith :: Parser BoolExp -> Parser IntExp -> Parser GraphExp -> Parser ValueExp -> Parser ListExp -> Parser Comm
parseSimpleCommWith boolParser intParser graphParser valueParser listParser = 
  try parseSkip
  <|> try (parseCondWith boolParser intParser graphParser valueParser listParser)
  <|> try (parseWhileWith boolParser intParser graphParser valueParser listParser)
  <|> try (parseForWith boolParser intParser graphParser valueParser listParser)
  <|> try (parsePrintWith valueParser)
  <|> try (parseLetValueWith intParser graphParser)

parseSeqOp :: Parser (Comm -> Comm -> Comm)
parseSeqOp = reservedOp gdsl ";" $> Seq

parseSkip :: Parser Comm
parseSkip = reserved gdsl "skip" $> Skip

parseCondWith :: Parser BoolExp -> Parser IntExp -> Parser GraphExp -> Parser ValueExp -> Parser ListExp -> Parser Comm
parseCondWith boolParser intParser graphParser valueParser listParser = do
  reserved gdsl "cond"
  cond <- boolParser
  reserved gdsl "then"
  trueBranch <- parseCommWith boolParser intParser graphParser valueParser listParser
  reserved gdsl "else"
  falseBranch <- parseCommWith boolParser intParser graphParser valueParser listParser
  reserved gdsl "end"
  return (Cond cond trueBranch falseBranch)

parseWhileWith :: Parser BoolExp -> Parser IntExp -> Parser GraphExp -> Parser ValueExp -> Parser ListExp -> Parser Comm
parseWhileWith boolParser intParser graphParser valueParser listParser = do
  reserved gdsl "while"
  cond <- boolParser
  reserved gdsl "do"
  body <- parseCommWith boolParser intParser graphParser valueParser listParser
  reserved gdsl "end"
  return (While cond body)

parseForWith :: Parser BoolExp -> Parser IntExp -> Parser GraphExp -> Parser ValueExp -> Parser ListExp -> Parser Comm
parseForWith boolParser intParser graphParser valueParser listParser = do
  reserved gdsl "for"
  var <- identifier gdsl
  reserved gdsl "in"
  list <- listParser
  reserved gdsl "do"
  body <- parseCommWith boolParser intParser graphParser valueParser listParser
  reserved gdsl "end"
  return (For var list body)

parsePrintWith :: Parser ValueExp -> Parser Comm
parsePrintWith valueParser = do
  reserved gdsl "print"
  expr <- valueParser
  return (Print expr)

parseLetValueWith :: Parser IntExp -> Parser GraphExp -> Parser Comm
parseLetValueWith intParser graphParser = do
  var <- identifier gdsl
  reservedOp gdsl ":="
  try (parseLetGraphVal var graphParser) <|> try (parseLetIntVal var intParser)

parseLetIntVal :: Variable -> Parser IntExp -> Parser Comm
parseLetIntVal var intParser = LetValue var . IntVal <$> intParser

parseLetGraphVal :: Variable -> Parser GraphExp -> Parser Comm
parseLetGraphVal var graphParser = LetValue var . GraphVal <$> graphParser

-- For backward compatibility (will be redefined in Core.hs)
parseSimpleComm :: Parser Comm
parseSimpleComm = error "parseSimpleComm: should be defined in Core.hs with proper wiring"

parseLetValue :: Parser Comm
parseLetValue = error "parseLetValue: should be defined in Core.hs with proper wiring"