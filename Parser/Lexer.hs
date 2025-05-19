module Parser.Lexer (gdsl) where

import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token ( makeTokenParser, GenLanguageDef(reservedOpNames, commentStart, commentEnd, commentLine, opStart, opLetter, reservedNames), TokenParser )
import Text.Parsec.Char (oneOf)


gdsl :: TokenParser u
gdsl = makeTokenParser (emptyDef {
    commentStart    = "/*",
    commentEnd      = "*/",
    commentLine     = "//",
    reservedNames   = ["true", "false", "skip", "cond", "then", "else", 
                       "end", "while", "do", "repeat", "kruskal", "addnode",
                       "addedge", "adduedge", "newgraph", "intersect", "with"],
    reservedOpNames = ["+", "-", "*", "/", "%", "?", 
                       ":", "=", "<", ">", ":=", "|",
                       "&", "~", ";", "->"]
  })
