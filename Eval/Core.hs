module Eval.Core ( evalComm, evalExpr ) where

import ASTGraphs ( Comm(..), Expr(..), BinOpType(..), CompOpType(..), FunctionType(..), Value(..), Graph(..), Node(..), Edge(..), Queue(..) )
import Eval.MonadClasses ( MonadError(..), MonadState(lookfor, update), MonadTick(..) )
import Utils ( addNode, addEdge )
import Control.Monad ( when )
import Data.List (intersect)


evalComm :: (MonadState m, MonadError m, MonadTick m) => Comm -> m ()
evalComm Skip = return ()
evalComm (AssignValue v expr) = do val <- evalExpr expr
                                   update v val
evalComm (Seq c1 c2) = do evalComm c1
                          evalComm c2
evalComm (Cond expr c1 c2) = do val <- evalExpr expr
                                case val of
                                  IntValue i -> if i /= 0 then evalComm c1 else evalComm c2
                                  _ -> throw
evalComm (While expr c) = do val <- evalExpr expr
                             case val of
                               IntValue i -> when (i /= 0) (evalComm (Seq c (While expr c)))
                               _ -> throw
evalComm (For v listExpr c) = do val <- evalExpr listExpr
                                 case val of
                                   ListValue nodes -> mapM_ (\node -> do update v (NodeValue node); evalComm c) nodes
                                   _ -> throw
evalComm (Print expr) = do evalExpr expr
                           return ()


evalExpr :: (MonadState m, MonadError m, MonadTick m) => Expr -> m Value
-- Literals
evalExpr (IntLit n) = return (IntValue n)
evalExpr (BoolLit True) = return (IntValue 1)  -- Using integers for booleans (1 = true, 0 = false)
evalExpr (BoolLit False) = return (IntValue 0)
evalExpr (NodeLit s) = return (NodeValue (Node s))
evalExpr EmptyList = return (ListValue [])
evalExpr EmptyQueue = return (QueueValue (Queue []))
evalExpr EmptyEdgeList = return (ListEdgeValue [])

-- Variables
evalExpr (Var v) = lookfor v

-- Arithmetic Operations
evalExpr (UMinus e) = do
  val <- evalExpr e
  case val of
    IntValue i -> return (IntValue (negate i))
    _ -> throw

evalExpr (BinOp op l r) = do
  lval <- evalExpr l
  rval <- evalExpr r
  case (lval, rval) of
    (IntValue l', IntValue r') -> do
      tick
      case op of
        Plus -> return (IntValue (l' + r'))
        Minus -> return (IntValue (l' - r'))
        Times -> return (IntValue (l' * r'))
        Div -> if r' == 0 then throw else return (IntValue (div l' r'))
        Mod -> if r' == 0 then throw else return (IntValue (mod l' r'))
    _ -> throw

-- Boolean Operations
evalExpr (Not e) = do
  val <- evalExpr e
  case val of
    IntValue 0 -> return (IntValue 1)
    IntValue _ -> return (IntValue 0)
    _ -> throw

evalExpr (Comparison op l r) = do
  lval <- evalExpr l
  rval <- evalExpr r
  case op of
    Eq -> case (lval, rval) of
      (IntValue l', IntValue r') -> return (IntValue (if l' == r' then 1 else 0))
      _ -> throw
    Lt -> case (lval, rval) of
      (IntValue l', IntValue r') -> return (IntValue (if l' < r' then 1 else 0))
      _ -> throw
    Gt -> case (lval, rval) of
      (IntValue l', IntValue r') -> return (IntValue (if l' > r' then 1 else 0))
      _ -> throw
    And -> case (lval, rval) of
      (IntValue l', IntValue r') -> return (IntValue (if l' /= 0 && r' /= 0 then 1 else 0))
      _ -> throw
    Or -> case (lval, rval) of
      (IntValue l', IntValue r') -> return (IntValue (if l' /= 0 || r' /= 0 then 1 else 0))
      _ -> throw
    EqNode -> case (lval, rval) of
      (NodeValue n1, NodeValue n2) -> return (IntValue (if n1 == n2 then 1 else 0))
      _ -> throw

-- Conditional Expression
evalExpr (Question cond thenE elseE) = do
  condVal <- evalExpr cond
  case condVal of
    IntValue 0 -> evalExpr elseE
    IntValue _ -> evalExpr thenE
    _ -> throw

-- Complex constructors
evalExpr (ValuedGraph nodeList) = do
  graph <- mapM evalNodeEntry nodeList
  return (GraphValue (Graph graph))
  where
    evalNodeEntry (nodeExpr, adjList) = do
      nodeVal <- evalExpr nodeExpr
      case nodeVal of
        NodeValue node -> do
          adjVals <- mapM evalAdjEntry adjList
          return (node, adjVals)
        _ -> throw
    evalAdjEntry (nodeExpr, weightExpr) = do
      nodeVal <- evalExpr nodeExpr
      weightVal <- evalExpr weightExpr
      case (nodeVal, weightVal) of
        (NodeValue node, IntValue weight) -> return (node, weight)
        _ -> throw

evalExpr (ValuedEdge n1Expr n2Expr wExpr) = do
  n1Val <- evalExpr n1Expr
  n2Val <- evalExpr n2Expr
  wVal <- evalExpr wExpr
  case (n1Val, n2Val, wVal) of
    (NodeValue n1, NodeValue n2, IntValue w) -> return (EdgeValue (Edge n1 n2 w))
    _ -> throw

-- Simple function calls (minimal implementation for academic purposes)
evalExpr (FunCall AddNode [graphExpr, nodeExpr]) = do
  graphVal <- evalExpr graphExpr
  nodeVal <- evalExpr nodeExpr
  case (graphVal, nodeVal) of
    (GraphValue graph, NodeValue node) -> return (GraphValue (addNode node graph))
    _ -> throw

evalExpr (FunCall AddEdge [graphExpr, node1Expr, node2Expr, weightExpr]) = do
  graphVal <- evalExpr graphExpr
  node1Val <- evalExpr node1Expr
  node2Val <- evalExpr node2Expr
  weightVal <- evalExpr weightExpr
  case (graphVal, node1Val, node2Val, weightVal) of
    (GraphValue graph, NodeValue node1, NodeValue node2, IntValue weight) -> 
      return (GraphValue (addEdge node1 node2 weight graph))
    _ -> throw

-- Lists
evalExpr (ListConstruct exprs) = do
  vals <- mapM evalExpr exprs
  -- Assume all elements are nodes for simplicity
  nodes <- mapM extractNode vals
  return (ListValue nodes)
  where
    extractNode (NodeValue node) = return node
    extractNode _ = throw

-- Default case for unimplemented function calls
evalExpr (FunCall _ _) = throw