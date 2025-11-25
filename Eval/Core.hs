module Eval.Core ( evalComm, evalExpr ) where

import ASTGraphs ( Comm(..), Expr(..), BinOpType(..), CompOpType(..), FunctionType(..), Value(..), Graph(..), Node, Edge(..), Queue(..), UnionFind(..), Weight )
import Eval.MonadClasses ( MonadError(..), MonadState(lookfor, update), MonadTick(..) )
import Control.Monad ( when )
import Data.List (intersect)

-- Helper function to validate undirected graphs have symmetric edges
-- Checks that for every edge (n1, n2, w), there exists a reverse edge (n2, n1, w)
checkUndirectedGraph :: [(Node, [(Node, Float)])] -> Bool
checkUndirectedGraph adjList = all checkEdge allEdges
  where
    allEdges = [(n1, n2, w) | (n1, neighbors) <- adjList, (n2, w) <- neighbors]
    checkEdge (n1, n2, w) = 
      case lookup n2 adjList of
        Nothing -> False  -- n2 not in graph
        Just neighbors -> (n1, w) `elem` neighbors  -- Check if reverse edge exists with same weight

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
evalExpr (NodeLit s) = return (NodeValue s)  -- Node is now just String
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
      (NodeValue l', NodeValue r') -> return (BoolValue (l' == r'))
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
  -- Automatically validate that the graph has symmetric edges (undirected graph)
  if not (checkUndirectedGraph graph)
    then error "Invalid undirected graph: edges must be symmetric (if A->B exists, B->A must exist with same weight)"
    else return (GraphValue (Graph graph))
  where
    evalNodeEntry (nodeExpr, adjList) = do
      nodeVal <- evalExpr nodeExpr
      node <- case nodeVal of
        NodeValue n -> return n
        StringValue s -> return s  -- Auto-convert strings to nodes
        _ -> throw
      adjVals <- mapM evalAdjEntry adjList
      return (node, adjVals)
    evalAdjEntry (nodeExpr, weightExpr) = do
      nodeVal <- evalExpr nodeExpr
      weightVal <- evalExpr weightExpr
      node <- case nodeVal of
        NodeValue n -> return n
        StringValue s -> return s  -- Auto-convert strings to nodes
        _ -> throw
      weight <- case weightVal of
        FloatValue w -> return w
        IntValue w -> return (fromInteger w)  -- Convert Int to Float
        _ -> throw
      return (node, weight)

evalExpr (ValuedEdge n1Expr n2Expr wExpr) = do
  n1Val <- evalExpr n1Expr
  n2Val <- evalExpr n2Expr
  wVal <- evalExpr wExpr
  n1 <- case n1Val of
    NodeValue n -> return n
    StringValue s -> return s  -- Auto-convert strings to nodes
    _ -> throw
  n2 <- case n2Val of
    NodeValue n -> return n
    StringValue s -> return s  -- Auto-convert strings to nodes
    _ -> throw
  w <- case wVal of
    FloatValue f -> return f
    IntValue i -> return (fromInteger i)  -- Convert Int to Float
    _ -> throw
  return (EdgeValue (Edge n1 n2 w))

-- Simple function calls (minimal implementation for academic purposes)
evalExpr (FunCall AddNode [graphExpr, nodeExpr]) = do
  graphVal <- evalExpr graphExpr
  nodeVal <- evalExpr nodeExpr
  case graphVal of
    GraphValue graph -> do
      node <- case nodeVal of
        NodeValue n -> return n
        StringValue s -> return s  -- Auto-convert strings to nodes
        _ -> throw
      return (GraphValue (addNode node graph))
    _ -> throw

evalExpr (FunCall AddEdge [graphExpr, node1Expr, node2Expr, weightExpr]) = do
  graphVal <- evalExpr graphExpr
  node1Val <- evalExpr node1Expr
  node2Val <- evalExpr node2Expr
  weightVal <- evalExpr weightExpr
  case graphVal of
    GraphValue graph -> do
      node1 <- case node1Val of
        NodeValue n -> return n
        StringValue s -> return s  -- Auto-convert strings to nodes
        _ -> throw
      node2 <- case node2Val of
        NodeValue n -> return n
        StringValue s -> return s  -- Auto-convert strings to nodes
        _ -> throw
      weight <- case weightVal of
        FloatValue w -> return w
        IntValue i -> return (fromInteger i)  -- Convert Int to Float
        _ -> throw
      return (GraphValue (addEdge node1 node2 weight graph))
    _ -> throw

-- Lists and Queues
evalExpr (ListConstruct exprs) = do
  vals <- mapM evalExpr exprs
  return (ListValue vals)  -- Now supports any type of values

evalExpr (QueueConstruct exprs) = do
  vals <- mapM evalExpr exprs
  return (QueueValue (Queue vals))

evalExpr (UnionFindConstruct pairs) = do
  ufPairs <- mapM evalPair pairs
  return (UnionFindValue (UnionFind ufPairs))
  where
    evalPair (nodeExpr, parentExpr) = do
      nodeVal <- evalExpr nodeExpr
      parentVal <- evalExpr parentExpr
      -- Convert to nodes (strings)
      let node = case nodeVal of
                   NodeValue n -> n
                   StringValue s -> s
                   _ -> error "UnionFind elements must be nodes or strings"
      let parent = case parentVal of
                     NodeValue n -> n
                     StringValue s -> s
                     _ -> error "UnionFind parents must be nodes or strings"
      return (node, parent)

-- Edge operations
evalExpr (FunCall GetNode1 [edgeExpr]) = do
  edgeVal <- evalExpr edgeExpr
  case edgeVal of
    EdgeValue (Edge n1 _ _) -> return (NodeValue n1)
    _ -> throw

evalExpr (FunCall GetNode2 [edgeExpr]) = do
  edgeVal <- evalExpr edgeExpr
  case edgeVal of
    EdgeValue (Edge _ n2 _) -> return (NodeValue n2)
    _ -> throw

evalExpr (FunCall GetWeight [edgeExpr]) = do
  edgeVal <- evalExpr edgeExpr
  case edgeVal of
    EdgeValue (Edge _ _ w) -> return (FloatValue w)
    _ -> throw

-- Graph operations
evalExpr (FunCall GetEdges [graphExpr]) = do
  graphVal <- evalExpr graphExpr
  case graphVal of
    GraphValue (Graph adjList) -> do
      -- Get all edges from adjacency list
      let allEdges = [(n1, n2, w) | (n1, neighbors) <- adjList, (n2, w) <- neighbors]
      -- For undirected graphs, remove duplicates (keep only n1 < n2)
      let uniqueEdges = [(n1, n2, w) | (n1, n2, w) <- allEdges, n1 < n2]
      let edgeValues = [EdgeValue (Edge n1 n2 w) | (n1, n2, w) <- uniqueEdges]
      return (ListValue edgeValues)
    _ -> throw

-- List operations
evalExpr (FunCall HeadList [listExpr]) = do
  listVal <- evalExpr listExpr
  case listVal of
    ListValue (x:_) -> return x
    ListValue [] -> throw  -- Error: head of empty list
    _ -> throw

evalExpr (FunCall TailList [listExpr]) = do
  listVal <- evalExpr listExpr
  case listVal of
    ListValue (_:xs) -> return (ListValue xs)
    ListValue [] -> throw  -- Error: tail of empty list
    _ -> throw

evalExpr (FunCall Len [listExpr]) = do
  listVal <- evalExpr listExpr
  case listVal of
    ListValue xs -> return (IntValue (fromIntegral (length xs)))
    _ -> throw

evalExpr (FunCall SortByWeight [listExpr]) = do
  listVal <- evalExpr listExpr
  case listVal of
    ListValue edges -> do
      let sortedEdges = sortByEdgeWeight edges
      return (ListValue sortedEdges)
    _ -> throw
  where
    sortByEdgeWeight :: [Value] -> [Value]
    sortByEdgeWeight values = 
      let edges = [(e, w) | EdgeValue e@(Edge _ _ w) <- values]
          sorted = map fst $ sortBy (\(_, w1) (_, w2) -> compare w1 w2) edges
      in map EdgeValue sorted
    sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    sortBy _ [] = []
    sortBy cmp (x:xs) = 
      let smaller = sortBy cmp [a | a <- xs, cmp a x == LT]
          larger = sortBy cmp [a | a <- xs, cmp a x /= LT]
      in smaller ++ [x] ++ larger

-- UnionFind operations
evalExpr (FunCall Find [nodeExpr, ufExpr]) = do
  nodeVal <- evalExpr nodeExpr
  ufVal <- evalExpr ufExpr
  case (nodeVal, ufVal) of
    (_, UnionFindValue (UnionFind pairs)) -> do
      let node = case nodeVal of
                   NodeValue n -> n
                   StringValue s -> s
                   _ -> error "Find requires a node or string"
      let root = findRoot node pairs
      return (NodeValue root)
    _ -> throw
  where
    findRoot :: Node -> [(Node, Node)] -> Node
    findRoot node pairs =
      case lookup node pairs of
        Just parent | parent == node -> node  -- Found root
        Just parent -> findRoot parent pairs  -- Keep searching
        Nothing -> error $ "Node not found in UnionFind: " ++ node

evalExpr (FunCall Union [node1Expr, node2Expr, ufExpr]) = do
  node1Val <- evalExpr node1Expr
  node2Val <- evalExpr node2Expr
  ufVal <- evalExpr ufExpr
  case (node1Val, node2Val, ufVal) of
    (_, _, UnionFindValue (UnionFind pairs)) -> do
      let node1 = case node1Val of
                    NodeValue n -> n
                    StringValue s -> s
                    _ -> error "Union requires nodes or strings"
      let node2 = case node2Val of
                    NodeValue n -> n
                    StringValue s -> s
                    _ -> error "Union requires nodes or strings"
      let root1 = findRoot node1 pairs
      let root2 = findRoot node2 pairs
      if root1 == root2
        then return (UnionFindValue (UnionFind pairs))  -- Already in same set
        else do
          -- Union by making root1 point to root2
          let newPairs = map (\(n, p) -> if n == root1 then (n, root2) else (n, p)) pairs
          return (UnionFindValue (UnionFind newPairs))
    _ -> throw
  where
    findRoot :: Node -> [(Node, Node)] -> Node
    findRoot node pairs =
      case lookup node pairs of
        Just parent | parent == node -> node
        Just parent -> findRoot parent pairs
        Nothing -> error $ "Node not found in UnionFind: " ++ node

-- Default case for unimplemented function calls
evalExpr (FunCall _ _) = throw



addNode :: Node -> Graph -> Graph
addNode n (Graph g)
  | n `elem` map fst g = Graph g
  | otherwise = Graph ((n, []) : g)

addEdge :: Node -> Node -> Weight -> Graph -> Graph
addEdge u v w g = addDirectedEdge v u w (addDirectedEdge u v w g)

addDirectedEdge :: Node -> Node -> Weight -> Graph -> Graph
addDirectedEdge u v w (Graph []) = Graph [(u, [(v , w)])]
addDirectedEdge u v w (Graph ((node, nodesWeights) : otros))
  | u == node = Graph ((node, (v, w) : nodesWeights) : otros)
  | otherwise = case addDirectedEdge u v w (Graph otros) of
                  Graph otros' -> Graph ((node, nodesWeights) : otros')