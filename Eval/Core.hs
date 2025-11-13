module Eval.Core ( evalComm, evalExpr ) where

import ASTGraphs ( Comm(..), Expr(..), BinOpType(..), CompOpType(..), FunctionType(..), Value(..), Graph(..), Node(..), Edge(..), Queue(..) )
import Eval.MonadClasses ( MonadError(..), MonadState(lookfor, update), MonadTick(..) )
import Eval.Utils ( addNode, addEdge )
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
                                  BoolValue True -> evalComm c1
                                  BoolValue False -> evalComm c2
                                  IntValue i -> if i /= 0 then evalComm c1 else evalComm c2  -- Backward compatibility
                                  _ -> throw
evalComm (While expr c) = do val <- evalExpr expr
                             case val of
                               BoolValue True -> evalComm (Seq c (While expr c))
                               BoolValue False -> return ()
                               IntValue i -> when (i /= 0) (evalComm (Seq c (While expr c)))  -- Backward compatibility
                               _ -> throw
evalComm (For v listExpr c) = do val <- evalExpr listExpr
                                 case val of
                                   ListValue values -> mapM_ (\value -> do update v value; evalComm c) values
                                   _ -> throw
evalComm (Print expr) = do evalExpr expr
                           return ()


evalExpr :: (MonadState m, MonadError m, MonadTick m) => Expr -> m Value
-- Literals
evalExpr (IntLit n) = return (IntValue n)
evalExpr (FloatLit f) = return (FloatValue f)
evalExpr (BoolLit b) = return (BoolValue b)  -- Now using proper Bool values
evalExpr (StringLit s) = return (StringValue s)
evalExpr (NodeLit s) = return (NodeValue (Node s))
evalExpr EmptyList = return (ListValue [])
evalExpr EmptyQueue = return (QueueValue (Queue []))

-- Variables
evalExpr (Var v) = lookfor v

-- Arithmetic Operations
evalExpr (UMinus e) = do
  val <- evalExpr e
  case val of
    IntValue i -> return (IntValue (negate i))
    FloatValue f -> return (FloatValue (negate f))
    _ -> throw

evalExpr (BinOp op l r) = do
  lval <- evalExpr l
  rval <- evalExpr r
  tick
  case (lval, rval) of
    -- Integer operations
    (IntValue l', IntValue r') -> case op of
      Plus -> return (IntValue (l' + r'))
      Minus -> return (IntValue (l' - r'))
      Times -> return (IntValue (l' * r'))
      Div -> if r' == 0 then throw else return (IntValue (div l' r'))
      Mod -> if r' == 0 then throw else return (IntValue (mod l' r'))
    -- Float operations  
    (FloatValue l', FloatValue r') -> case op of
      Plus -> return (FloatValue (l' + r'))
      Minus -> return (FloatValue (l' - r'))
      Times -> return (FloatValue (l' * r'))
      Div -> if r' == 0.0 then throw else return (FloatValue (l' / r'))
      Mod -> throw  -- Modulo not defined for floats
    -- Mixed operations (promote to Float)
    (IntValue l', FloatValue r') -> case op of
      Plus -> return (FloatValue (fromInteger l' + r'))
      Minus -> return (FloatValue (fromInteger l' - r'))
      Times -> return (FloatValue (fromInteger l' * r'))
      Div -> if r' == 0.0 then throw else return (FloatValue (fromInteger l' / r'))
      Mod -> throw
    (FloatValue l', IntValue r') -> case op of
      Plus -> return (FloatValue (l' + fromInteger r'))
      Minus -> return (FloatValue (l' - fromInteger r'))
      Times -> return (FloatValue (l' * fromInteger r'))
      Div -> if r' == 0 then throw else return (FloatValue (l' / fromInteger r'))
      Mod -> throw
    _ -> throw

-- Boolean Operations
evalExpr (Not e) = do
  val <- evalExpr e
  case val of
    BoolValue b -> return (BoolValue (not b))
    IntValue 0 -> return (BoolValue True)   -- For backward compatibility
    IntValue _ -> return (BoolValue False)  -- For backward compatibility
    _ -> throw

evalExpr (Comparison op l r) = do
  lval <- evalExpr l
  rval <- evalExpr r
  case op of
    Eq -> case (lval, rval) of
      (IntValue l', IntValue r') -> return (BoolValue (l' == r'))
      (FloatValue l', FloatValue r') -> return (BoolValue (l' == r'))
      (BoolValue l', BoolValue r') -> return (BoolValue (l' == r'))
      (StringValue l', StringValue r') -> return (BoolValue (l' == r'))
      _ -> throw
    Lt -> case (lval, rval) of
      (IntValue l', IntValue r') -> return (BoolValue (l' < r'))
      (FloatValue l', FloatValue r') -> return (BoolValue (l' < r'))
      (IntValue l', FloatValue r') -> return (BoolValue (fromInteger l' < r'))
      (FloatValue l', IntValue r') -> return (BoolValue (l' < fromInteger r'))
      _ -> throw
    Gt -> case (lval, rval) of
      (IntValue l', IntValue r') -> return (BoolValue (l' > r'))
      (FloatValue l', FloatValue r') -> return (BoolValue (l' > r'))
      (IntValue l', FloatValue r') -> return (BoolValue (fromInteger l' > r'))
      (FloatValue l', IntValue r') -> return (BoolValue (l' > fromInteger r'))
      _ -> throw
    And -> case (lval, rval) of
      (BoolValue l', BoolValue r') -> return (BoolValue (l' && r'))
      (IntValue l', IntValue r') -> return (BoolValue (l' /= 0 && r' /= 0))  -- Backward compatibility
      _ -> throw
    Or -> case (lval, rval) of
      (BoolValue l', BoolValue r') -> return (BoolValue (l' || r'))
      (IntValue l', IntValue r') -> return (BoolValue (l' /= 0 || r' /= 0))  -- Backward compatibility
      _ -> throw
    EqNode -> case (lval, rval) of
      (NodeValue n1, NodeValue n2) -> return (BoolValue (n1 == n2))
      _ -> throw

-- Conditional Expression
evalExpr (Question cond thenE elseE) = do
  condVal <- evalExpr cond
  case condVal of
    BoolValue False -> evalExpr elseE
    BoolValue True -> evalExpr thenE
    IntValue 0 -> evalExpr elseE       -- Backward compatibility
    IntValue _ -> evalExpr thenE       -- Backward compatibility
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
        (NodeValue node, FloatValue weight) -> return (node, weight)
        (NodeValue node, IntValue weight) -> return (node, fromInteger weight)  -- Convert Int to Float
        _ -> throw

evalExpr (ValuedEdge n1Expr n2Expr wExpr) = do
  n1Val <- evalExpr n1Expr
  n2Val <- evalExpr n2Expr
  wVal <- evalExpr wExpr
  case (n1Val, n2Val, wVal) of
    (NodeValue n1, NodeValue n2, FloatValue w) -> return (EdgeValue (Edge n1 n2 w))
    (NodeValue n1, NodeValue n2, IntValue w) -> return (EdgeValue (Edge n1 n2 (fromInteger w)))  -- Convert Int to Float
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
    (GraphValue graph, NodeValue node1, NodeValue node2, FloatValue weight) -> 
      return (GraphValue (addEdge node1 node2 weight graph))
    (GraphValue graph, NodeValue node1, NodeValue node2, IntValue weight) -> 
      return (GraphValue (addEdge node1 node2 (fromInteger weight) graph))
    _ -> throw

-- Lists and Queues
evalExpr (ListConstruct exprs) = do
  vals <- mapM evalExpr exprs
  return (ListValue vals)  -- Now supports any type of values

evalExpr (QueueConstruct exprs) = do
  vals <- mapM evalExpr exprs
  return (QueueValue (Queue vals))

-- Default case for unimplemented function calls
evalExpr (FunCall _ _) = throw