module Parser.Expressions 
  ( parseIntExp
  , parseBoolExp
  , parseAtomIntExp
  , parseAtomBoolExp
    , parseAddOp
    , parseMulOp
    , parseOrOp
    , parseAndOp
    , parseComparisonOp
    , parseQuestion
    , parseComparisonIntExp
  ) where

import Text.ParserCombinators.Parsec ( chainl1, (<|>), try, Parser )
import Text.Parsec.Token ( GenTokenParser( integer, reserved, identifier, brackets, parens, stringLiteral, reservedOp) )
import Data.Functor (($>))

import Parser.Lexer ( gdsl )
import ASTGraphs ( IntExp(..), BoolExp(..) )

-- Integer expressions
parseIntExp :: Parser IntExp
parseIntExp = chainl1 parseMulExp parseAddOp

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

-- Boolean expressions
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

parseComparisonIntExp :: Parser BoolExp
parseComparisonIntExp = (\l op r -> op l r) <$> parseIntExp <*> parseComparisonOp <*> parseIntExp

-- Operators

parseOrOp :: Parser (BoolExp -> BoolExp -> BoolExp)
parseOrOp = reservedOp gdsl "|" $> Or

parseAndOp :: Parser (BoolExp -> BoolExp -> BoolExp)
parseAndOp = reservedOp gdsl "&" $> And

parseComparisonOp :: Parser (IntExp -> IntExp -> BoolExp)
parseComparisonOp =  (reservedOp gdsl "=" $> Eq)
                 <|> (reservedOp gdsl "<" $> Lt)
                 <|> (reservedOp gdsl ">" $> Gt)