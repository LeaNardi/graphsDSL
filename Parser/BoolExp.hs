module Parser.BoolExp 
  ( parseBoolExp
  , parseBoolExpWith
  , parseAtomBoolExp
  ) where

import Text.ParserCombinators.Parsec ( chainl1, (<|>), try, Parser )
import Text.Parsec.Token ( GenTokenParser( reserved, parens, reservedOp) )
import Data.Functor (($>))

import Parser.Lexer ( gdsl )
import ASTGraphs ( BoolExp(..), IntExp(..) )

-- Main parser for external use (will be wired up in Core.hs)
parseBoolExp :: Parser BoolExp
parseBoolExp = error "parseBoolExp: should be defined in Core.hs with proper wiring"

-- Parameterized parser that takes an IntExp parser
parseBoolExpWith :: Parser IntExp -> Parser BoolExp
parseBoolExpWith intParser = chainl1 (parseAndExpWith intParser) parseOrOp

parseAndExpWith :: Parser IntExp -> Parser BoolExp
parseAndExpWith intParser = chainl1 (parseAtomBoolExpWith intParser) parseAndOp

parseAtomBoolExpWith :: Parser IntExp -> Parser BoolExp
parseAtomBoolExpWith intParser = 
  parens gdsl (parseBoolExpWith intParser)
  <|> try (Not <$> (reservedOp gdsl "~" *> parseAtomBoolExpWith intParser))
  <|> try (reserved gdsl "true"  $> BTrue)
  <|> try (reserved gdsl "false" $> BFalse)
  <|> try (parseComparisonIntExpWith intParser)

parseComparisonIntExpWith :: Parser IntExp -> Parser BoolExp
parseComparisonIntExpWith intParser = 
  (\l op r -> op l r) <$> intParser <*> parseComparisonOp <*> intParser

-- For backward compatibility (will be redefined in Core.hs)
parseAtomBoolExp :: Parser BoolExp
parseAtomBoolExp = error "parseAtomBoolExp: should be defined in Core.hs with proper wiring"

-- Operators
parseOrOp :: Parser (BoolExp -> BoolExp -> BoolExp)
parseOrOp = reservedOp gdsl "|" $> Or

parseAndOp :: Parser (BoolExp -> BoolExp -> BoolExp)
parseAndOp = reservedOp gdsl "&" $> And

parseComparisonOp :: Parser (IntExp -> IntExp -> BoolExp)
parseComparisonOp =  (reservedOp gdsl "=" $> Eq)
                 <|> (reservedOp gdsl "<" $> Lt)
                 <|> (reservedOp gdsl ">" $> Gt)