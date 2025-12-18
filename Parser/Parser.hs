module Parser.Parser ( parseGraphs ) where

import Text.ParserCombinators.Parsec ( parse, SourceName, ParseError, eof, Parser )
import Text.Parsec.Token ( GenTokenParser(whiteSpace) )

import Parser.Lexer ( gdsl )
import Parser.Core ( parseComm )
import ASTGraphs (Comm)


totParser :: Parser a -> Parser a
totParser p = do 
                whiteSpace gdsl
                t <- p
                eof
                return t

                  
parseGraphs :: SourceName -> String -> Either ParseError Comm
parseGraphs = parse (totParser parseComm)