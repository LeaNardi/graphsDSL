module Parser.Lexer (gdsl) where

import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token ( makeTokenParser, GenLanguageDef(commentStart, commentEnd, commentLine, reservedOpNames, reservedNames), TokenParser )


gdsl :: TokenParser u
gdsl = makeTokenParser (emptyDef {
    commentStart    = "/*",
    commentEnd      = "*/",
    commentLine     = "//",
    reservedNames   = ["true", "false", "skip", "cond", "then", "else", 
                       "end", "while", "do", "for", "in", "print", "visualize",
                       "forNeighbors", "forNodes", "forEdges", "forIncident", "forComponent",
                       "on", "from", "upto",
                       "graph", "edge", "queue", "unionfind",
                       "emptyList", "emptyQueue"],
    reservedOpNames = ["+", "-", "*", "/", "%", "?", 
                       ":", "=", "==", "<", ">", ":=", "|", "||",
                       "&", "&&", "~", "!", ";", "->"]
  })
