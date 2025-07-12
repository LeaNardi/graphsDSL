module Parser.IntExp 
  ( parseIntExp
  , parseIntExpWith
  , parseAtomIntExp
  , parseQuestion
  ) where

import Text.ParserCombinators.Parsec ( chainl1, (<|>), try, Parser )
import Text.Parsec.Token ( GenTokenParser( integer, identifier, parens, reservedOp) )
import Data.Functor (($>))

import Parser.Lexer ( gdsl )
import ASTGraphs ( IntExp(..), BoolExp(..) )

-- Main parser for external use (will be wired up in Core.hs)
parseIntExp :: Parser IntExp
parseIntExp = error "parseIntExp: should be defined in Core.hs with proper wiring"

-- Parameterized parser that takes a BoolExp parser
parseIntExpWith :: Parser BoolExp -> Parser IntExp
parseIntExpWith boolParser = chainl1 (parseMulExpWith boolParser) parseAddOp

parseMulExpWith :: Parser BoolExp -> Parser IntExp
parseMulExpWith boolParser = chainl1 (parseAtomIntExpWith boolParser) parseMulOp

parseAtomIntExpWith :: Parser BoolExp -> Parser IntExp
parseAtomIntExpWith boolParser = 
  parens gdsl (parseIntExpWith boolParser)
  <|> try (UMinus <$> (reservedOp gdsl "-" *> parseAtomIntExpWith boolParser))
  <|> try (Const <$> integer gdsl)
  <|> try (VarInt <$> identifier gdsl)
  <|> try (parseQuestionWith boolParser)

parseQuestionWith :: Parser BoolExp -> Parser IntExp
parseQuestionWith boolParser = do
  cond <- boolParser
  reservedOp gdsl "?"
  e1 <- parseIntExpWith boolParser
  reservedOp gdsl ":"
  Question cond e1 <$> parseIntExpWith boolParser

-- For backward compatibility (will be redefined in Core.hs)
parseAtomIntExp :: Parser IntExp
parseAtomIntExp = error "parseAtomIntExp: should be defined in Core.hs with proper wiring"

parseQuestion :: Parser IntExp
parseQuestion = error "parseQuestion: should be defined in Core.hs with proper wiring"

-- Operators
parseAddOp :: Parser (IntExp -> IntExp -> IntExp)
parseAddOp = reservedOp gdsl "+" $> Plus
          <|> reservedOp gdsl "-" $> Minus

parseMulOp :: Parser (IntExp -> IntExp -> IntExp)
parseMulOp = reservedOp gdsl "*" $> Times
          <|> reservedOp gdsl "/" $> Div
          <|> reservedOp gdsl "%" $> Mod