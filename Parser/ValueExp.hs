module Parser.ValueExp 
  ( parseValueExp
  ) where

import Text.ParserCombinators.Parsec ( (<|>), try, Parser )
import ASTGraphs ( ValueExp(..) )
import Parser.Expressions ( parseIntExp, parseAtomIntExp, parseAddOp, parseMulOp )
import Parser.GraphExp ( parseGraphExp, parseNodeExp, parseEdgeExp )
import Parser.NodeExp ( parseNodeExp, parseVarNode )
import Parser.EdgeExp ( parseEdgeExp, parseVarEdge, parseHeadEdge )
import Parser.ListExp ( parseListExp, parseVarList, parseAdjacentNodes, parseAddList )
import Parser.QueueExp ( parseQueueExp, parseNewQueue, parseVarQueue, parseEnqueue, parseDequeueNode )
import Parser.UnionFindExp ( parseUnionFindExp, parseNewUnionFind, parseVarUnionFind, parseUnion, parseUnionFindList )
import Parser.ListEdgeExp ( parseListEdgeExp, parseVarEdgeList, parseGetEdges, parseSortByWeight )

parseValueExp :: Parser ValueExp
parseValueExp = try (IntVal <$> parseIntExp)
             <|> try (GraphVal <$> parseGraphExp)
             <|> try (NodeVal <$> parseNodeExp)
             <|> try (EdgeVal <$> parseEdgeExp)
             <|> try (ListVal <$> parseListExp)
             <|> try (QueueVal <$> parseQueueExp)
             <|> try (UnionFindVal <$> parseUnionFindExp)
             <|> try (ListEdgeVal <$> parseListEdgeExp)