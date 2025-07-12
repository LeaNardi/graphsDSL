module Parser.ListExp 
  ( parseListExp
  , parseListExpWith
  ) where

import Text.ParserCombinators.Parsec ( (<|>), try, Parser )
import Text.Parsec.Token ( GenTokenParser( reserved, identifier) )
import Data.Functor (($>))

import Parser.Lexer ( gdsl )
import ASTGraphs ( ListExp(..), NodeExp(..), GraphExp(..) )

-- Main parser for external use (will be wired up in Core.hs)
parseListExp :: Parser ListExp
parseListExp = error "parseListExp: should be defined in Core.hs with proper wiring"

-- Parameterized parser
parseListExpWith :: Parser GraphExp -> Parser NodeExp -> Parser ListExp
parseListExpWith graphParser nodeParser = 
  try parseNewList
  <|> try parseVarList
  <|> try (parseAdjacentNodesWith graphParser nodeParser)
  <|> try (parseAddListWith nodeParser)

parseNewList :: Parser ListExp
parseNewList = reserved gdsl "newList" $> NewList

parseVarList :: Parser ListExp
parseVarList = VarList <$> identifier gdsl

parseAdjacentNodesWith :: Parser GraphExp -> Parser NodeExp -> Parser ListExp
parseAdjacentNodesWith graphParser nodeParser = do
  reserved gdsl "adjacentNodes"
  graph <- graphParser
  node <- nodeParser
  return (AdjacentNodes graph node)

parseAddListWith :: Parser NodeExp -> Parser ListExp
parseAddListWith nodeParser = do
  reserved gdsl "addList"
  list <- parseListExp  -- This will be resolved in Core.hs
  node <- nodeParser
  return (AddList list node)