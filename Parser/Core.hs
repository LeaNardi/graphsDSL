module Parser.Core 
  ( parseComm
  , parseIntExp
  , parseBoolExp
  , parseGraphExp
  , parseValueExp
  , parseListExp
  , parseNodeExp
  , parseEdgeExp
  , parseQueueExp
  , parseUnionFindExp
  , parseListEdgeExp
  ) where

import Text.ParserCombinators.Parsec ( Parser )

-- Import parameterized parsers
import qualified Parser.IntExp as IntExp
import qualified Parser.BoolExp as BoolExp
import qualified Parser.GraphExp as GraphExp
import qualified Parser.Commands as Commands
import qualified Parser.ValueExp as ValueExp
import qualified Parser.ListExp as ListExp
import qualified Parser.NodeExp as NodeExp
import qualified Parser.EdgeExp as EdgeExp
import qualified Parser.QueueExp as QueueExp
import qualified Parser.UnionFindExp as UnionFindExp
import qualified Parser.ListEdgeExp as ListEdgeExp

import ASTGraphs ( IntExp, BoolExp, GraphExp, Comm, ValueExp, ListExp, NodeExp, EdgeExp, QueueExp, UnionFindExp, ListEdgeExp )

-- Wire up the circular dependencies
parseIntExp :: Parser IntExp
parseIntExp = IntExp.parseIntExpWith parseBoolExp

parseBoolExp :: Parser BoolExp
parseBoolExp = BoolExp.parseBoolExpWith parseIntExp

parseGraphExp :: Parser GraphExp
parseGraphExp = GraphExp.parseGraphExp

parseListExp :: Parser ListExp
parseListExp = ListExp.parseListExpWith parseGraphExp parseNodeExp

parseNodeExp :: Parser NodeExp
parseNodeExp = NodeExp.parseNodeExpWith parseQueueExp

parseEdgeExp :: Parser EdgeExp
parseEdgeExp = EdgeExp.parseEdgeExpWith parseListEdgeExp

parseQueueExp :: Parser QueueExp
parseQueueExp = QueueExp.parseQueueExpWith parseNodeExp

parseUnionFindExp :: Parser UnionFindExp
parseUnionFindExp = UnionFindExp.parseUnionFindExpWith parseNodeExp

parseListEdgeExp :: Parser ListEdgeExp
parseListEdgeExp = ListEdgeExp.parseListEdgeExpWith parseGraphExp

parseValueExp :: Parser ValueExp
parseValueExp = ValueExp.parseValueExpWith parseIntExp parseGraphExp parseNodeExp parseEdgeExp parseListExp parseQueueExp parseUnionFindExp parseListEdgeExp

parseComm :: Parser Comm
parseComm = Commands.parseCommWith parseBoolExp parseIntExp parseGraphExp parseValueExp parseListExp