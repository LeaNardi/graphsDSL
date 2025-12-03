module Eval.Core ( evalComm, evalExpr ) where

import ASTGraphs ( Comm(..), Expr(..), BinOpType(..), CompOpType(..), FunctionType(..), Value(..), Graph(..), Node, Edge(..), Queue(..), UnionFind(..), Weight )
import Eval.MonadClasses ( MonadError(..), MonadState(lookfor, update), MonadTick(..) )
import Control.Monad ( when )
import Data.List (intersect)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (callCommand)
import Visualization.GraphvizExporter (writeGraphToDotFile)

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
                                  IntValue i -> if i /= 0 then evalComm c1 else evalComm c2 -- Admite enteros como expresion condicional
                                  _ -> throw "Conditional expression must be Bool or Int"
evalComm (While expr c) = do val <- evalExpr expr
                             case val of
                               BoolValue True -> evalComm (Seq c (While expr c))
                               BoolValue False -> return ()
                               IntValue i -> when (i /= 0) (evalComm (Seq c (While expr c))) -- Admite enteros como expresion condicional
                               _ -> throw "While condition must be Bool or Int"
evalComm (For v listExpr c) = do val <- evalExpr listExpr
                                 case val of
                                   ListValue values -> mapM_ (\value -> do update v value; evalComm c) values
                                   _ -> throw "For loop requires a List"
evalComm (Visualize graphExpr fileExpr) = do 
  graphVal <- evalExpr graphExpr
  fileVal <- evalExpr fileExpr
  case (graphVal, fileVal) of
    (GraphValue _, StringValue fileName) -> do
      -- Se usa unsafePerformIO para permitir efectos secundarios dentro de la monada
      let !_ = unsafePerformIO $ do
            writeGraphToDotFile fileName graphVal
            let pngFile = fileName ++ ".png"
            -- Se llama a un comando externo para generar la imagen PNG desde el archivo DOT
            callCommand $ "dot -Tpng " ++ fileName ++ " -o " ++ pngFile
      return ()
    (GraphValue _, _) -> throw "Visualize filename must be a String"
    _ -> throw "Visualize requires a Graph value"
evalComm (Print expr) = do evalExpr expr
                           return ()

-- Funciones auxiliares
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

-- Verifica que para cada arista (n1, n2, w), exista la arista (n2, n1, w)
checkUndirectedGraph :: [(Node, [(Node, Float)])] -> Bool
checkUndirectedGraph adjList = all checkEdge allEdges
  where
    allEdges = [(n1, n2, w) | (n1, neighbors) <- adjList, (n2, w) <- neighbors]
    checkEdge (n1, n2, w) = 
      case lookup n2 adjList of
        Nothing -> False
        Just neighbors -> (n1, w) `elem` neighbors

evalExpr :: (MonadState m, MonadError m, MonadTick m) => Expr -> m Value
-- Literales
evalExpr (IntLit n) = return (IntValue n)
evalExpr (FloatLit f) = return (FloatValue f)
evalExpr (BoolLit b) = return (BoolValue b)
evalExpr (StringLit s) = return (StringValue s)
evalExpr EmptyList = return (ListValue [])
evalExpr EmptyQueue = return (QueueValue (Queue []))

-- Variables
evalExpr (Var v) = lookfor v

-- Operaciones aritmeticas binarias
evalExpr (UMinus e) = do
  val <- evalExpr e
  case val of
    IntValue i -> return (IntValue (negate i))
    FloatValue f -> return (FloatValue (negate f))
    _ -> throw "Unary minus requires Int or Float"

evalExpr (BinOp op l r) = do
  lval <- evalExpr l
  rval <- evalExpr r
  tick
  case (lval, rval) of
    -- Operaciones Int
    (IntValue l', IntValue r') -> case op of
      Plus -> return (IntValue (l' + r'))
      Minus -> return (IntValue (l' - r'))
      Times -> return (IntValue (l' * r'))
      Div -> if r' == 0 then throw "Division by zero" else return (IntValue (div l' r'))
      Mod -> if r' == 0 then throw "Modulo by zero" else return (IntValue (mod l' r'))
    -- Operaciones Float 
    (FloatValue l', FloatValue r') -> case op of
      Plus -> return (FloatValue (l' + r'))
      Minus -> return (FloatValue (l' - r'))
      Times -> return (FloatValue (l' * r'))
      Div -> if r' == 0.0 then throw "Division by zero" else return (FloatValue (l' / r'))
      Mod -> throw "Modulo operation not supported for Float types"
    -- Operaciones Mixtas (resultado Float)
    (IntValue l', FloatValue r') -> case op of
      Plus -> return (FloatValue (fromInteger l' + r'))
      Minus -> return (FloatValue (fromInteger l' - r'))
      Times -> return (FloatValue (fromInteger l' * r'))
      Div -> if r' == 0.0 then throw "Division by zero" else return (FloatValue (fromInteger l' / r'))
      Mod -> throw "Modulo operation not supported for mixed Int/Float types"
    (FloatValue l', IntValue r') -> case op of
      Plus -> return (FloatValue (l' + fromInteger r'))
      Minus -> return (FloatValue (l' - fromInteger r'))
      Times -> return (FloatValue (l' * fromInteger r'))
      Div -> if r' == 0 then throw "Division by zero" else return (FloatValue (l' / fromInteger r'))
      Mod -> throw "Modulo operation not supported for mixed Float/Int types"
    _ -> throw "Invalid operand types for arithmetic operation"

-- Operaciones Booleanas
evalExpr (Not e) = do
  val <- evalExpr e
  case val of
    BoolValue b -> return (BoolValue (not b))
    IntValue 0 -> return (BoolValue True)
    IntValue _ -> return (BoolValue False)
    _ -> throw "Not operation requires Bool or Int"

evalExpr (Comparison op l r) = do
  lval <- evalExpr l
  rval <- evalExpr r
  case op of
    Eq -> case (lval, rval) of
      (IntValue l', IntValue r') -> return (BoolValue (l' == r'))
      (FloatValue l', FloatValue r') -> return (BoolValue (l' == r'))
      (BoolValue l', BoolValue r') -> return (BoolValue (l' == r'))
      (StringValue l', StringValue r') -> return (BoolValue (l' == r'))
      _ -> throw "Equality comparison requires matching types (Int, Float, Bool, or String)"
    Lt -> case (lval, rval) of
      (IntValue l', IntValue r') -> return (BoolValue (l' < r'))
      (FloatValue l', FloatValue r') -> return (BoolValue (l' < r'))
      (IntValue l', FloatValue r') -> return (BoolValue (fromInteger l' < r'))
      (FloatValue l', IntValue r') -> return (BoolValue (l' < fromInteger r'))
      _ -> throw "Less than comparison requires numeric types (Int or Float)"
    Gt -> case (lval, rval) of
      (IntValue l', IntValue r') -> return (BoolValue (l' > r'))
      (FloatValue l', FloatValue r') -> return (BoolValue (l' > r'))
      (IntValue l', FloatValue r') -> return (BoolValue (fromInteger l' > r'))
      (FloatValue l', IntValue r') -> return (BoolValue (l' > fromInteger r'))
      _ -> throw "Greater than comparison requires numeric types (Int or Float)"
    And -> case (lval, rval) of
      (BoolValue l', BoolValue r') -> return (BoolValue (l' && r'))
      (IntValue l', IntValue r') -> return (BoolValue (l' /= 0 && r' /= 0))
      _ -> throw "And operation requires Bool or Int types"
    Or -> case (lval, rval) of
      (BoolValue l', BoolValue r') -> return (BoolValue (l' || r'))
      (IntValue l', IntValue r') -> return (BoolValue (l' /= 0 || r' /= 0))
      _ -> throw "Or operation requires Bool or Int types"

-- Expresiones Condicionales
evalExpr (Question cond thenE elseE) = do
  condVal <- evalExpr cond
  case condVal of
    BoolValue False -> evalExpr elseE
    BoolValue True -> evalExpr thenE
    IntValue 0 -> evalExpr elseE
    IntValue _ -> evalExpr thenE
    _ -> throw "Ternary operator condition must be Bool or Int"

-- Constructores de grafos
evalExpr (ValuedGraph nodeList) = do
  graph <- mapM evalNodeEntry nodeList
  -- validamos que el grafo sea no dirigido
  if not (checkUndirectedGraph graph)
    then throw "Invalid undirected graph: edges must be symmetric (if A->B exists, B->A must exist with same weight)"
    else return (GraphValue (Graph graph))
  where
    evalNodeEntry (nodeExpr, adjList) = do
      nodeVal <- evalExpr nodeExpr
      node <- case nodeVal of
        StringValue s -> return s
        _ -> throw "Graph node must be String type"
      adjVals <- mapM evalAdjEntry adjList
      return (node, adjVals)
    evalAdjEntry (nodeExpr, weightExpr) = do
      nodeVal <- evalExpr nodeExpr
      weightVal <- evalExpr weightExpr
      node <- case nodeVal of
        StringValue s -> return s
        _ -> throw "Graph adjacent node must be String type"
      weight <- case weightVal of
        FloatValue w -> return w
        IntValue w -> return (fromInteger w)
        _ -> throw "Graph edge weight must be Float or Int type"
      return (node, weight)

evalExpr (ValuedEdge n1Expr n2Expr wExpr) = do
  n1Val <- evalExpr n1Expr
  n2Val <- evalExpr n2Expr
  wVal <- evalExpr wExpr
  n1 <- case n1Val of
    StringValue s -> return s
    _ -> throw "Edge node1 must be String type"
  n2 <- case n2Val of
    StringValue s -> return s
    _ -> throw "Edge node2 must be String type"
  w <- case wVal of
    FloatValue f -> return f
    IntValue i -> return (fromInteger i)
    _ -> throw "Edge weight must be Float or Int type"
  return (EdgeValue (Edge n1 n2 w))

-- Colecciones
evalExpr (ListConstruct exprs) = do
  vals <- mapM evalExpr exprs
  return (ListValue vals)

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
      node <- case nodeVal of
                StringValue s -> return s
                _ -> throw "UnionFind elements must be String type"
      parent <- case parentVal of
                  StringValue s -> return s
                  _ -> throw "UnionFind parents must be String type"
      return (node, parent)

-- Operaciones de Edge
evalExpr (FunCall GetNode1 [edgeExpr]) = do
  edgeVal <- evalExpr edgeExpr
  case edgeVal of
    EdgeValue (Edge n1 _ _) -> return (StringValue n1)
    _ -> throw "GetNode1 requires an Edge type"

evalExpr (FunCall GetNode2 [edgeExpr]) = do
  edgeVal <- evalExpr edgeExpr
  case edgeVal of
    EdgeValue (Edge _ n2 _) -> return (StringValue n2)
    _ -> throw "GetNode2 requires an Edge type"

evalExpr (FunCall GetWeight [edgeExpr]) = do
  edgeVal <- evalExpr edgeExpr
  case edgeVal of
    EdgeValue (Edge _ _ w) -> return (FloatValue w)
    _ -> throw "GetWeight requires an Edge type"

-- Operaciones de Graph
evalExpr (FunCall AddNode [graphExpr, nodeExpr]) = do
  graphVal <- evalExpr graphExpr
  nodeVal <- evalExpr nodeExpr
  case graphVal of
    GraphValue graph -> do
      node <- case nodeVal of
        StringValue s -> return s
        _ -> throw "AddNode requires node to be String type"
      return (GraphValue (addNode node graph))
    _ -> throw "AddNode requires a Graph type"

evalExpr (FunCall AddEdge [graphExpr, node1Expr, node2Expr, weightExpr]) = do
  graphVal <- evalExpr graphExpr
  node1Val <- evalExpr node1Expr
  node2Val <- evalExpr node2Expr
  weightVal <- evalExpr weightExpr
  case graphVal of
    GraphValue graph -> do
      node1 <- case node1Val of
        StringValue s -> return s
        _ -> throw "AddEdge requires node1 to be String type"
      node2 <- case node2Val of
        StringValue s -> return s
        _ -> throw "AddEdge requires node2 to be String type"
      weight <- case weightVal of
        FloatValue w -> return w
        IntValue i -> return (fromInteger i)
        _ -> throw "AddEdge requires weight to be Float or Int type"
      return (GraphValue (addEdge node1 node2 weight graph))
    _ -> throw "AddEdge requires a Graph type"

evalExpr (FunCall GetEdges [graphExpr]) = do
  graphVal <- evalExpr graphExpr
  case graphVal of
    GraphValue (Graph adjList) -> do
      -- traemos todas las aristas de la lista de adyacencia
      let allEdges = [(n1, n2, w) | (n1, neighbors) <- adjList, (n2, w) <- neighbors]
      -- para grafos no dirigidos eliminamos las aristas repetidas (dejamos las que sean n1 < n2)
      let uniqueEdges = [(n1, n2, w) | (n1, n2, w) <- allEdges, n1 < n2]
      let edgeValues = [EdgeValue (Edge n1 n2 w) | (n1, n2, w) <- uniqueEdges]
      return (ListValue edgeValues)
    _ -> throw "GetEdges requires a Graph type"

evalExpr (FunCall DeleteNode [graphExpr, nodeExpr]) = do
  graphVal <- evalExpr graphExpr
  nodeVal <- evalExpr nodeExpr
  case graphVal of
    GraphValue (Graph adjList) -> do
      node <- case nodeVal of
        StringValue s -> return s
        _ -> throw "DeleteNode requires node to be String type"
      -- Remove node from adjacency list and remove edges pointing to it
      let adjList' = [(n, [(n2, w) | (n2, w) <- neighbors, n2 /= node]) 
                     | (n, neighbors) <- adjList, n /= node]
      return (GraphValue (Graph adjList'))
    _ -> throw "DeleteNode requires a Graph type"

evalExpr (FunCall AdjacentNodes [graphExpr, nodeExpr]) = do
  graphVal <- evalExpr graphExpr
  nodeVal <- evalExpr nodeExpr
  case graphVal of
    GraphValue (Graph adjList) -> do
      node <- case nodeVal of
        StringValue s -> return s
        _ -> throw "AdjacentNodes requires node to be String type"
      case lookup node adjList of
        Just neighbors -> return (ListValue [StringValue n | (n, _) <- neighbors])
        Nothing -> throw $ "Node '" ++ node ++ "' not found in graph"
    _ -> throw "AdjacentNodes requires a Graph type"

evalExpr (FunCall GraphComplement [graphExpr]) = do
  graphVal <- evalExpr graphExpr
  case graphVal of
    GraphValue (Graph adjList) -> do
      let nodes = map fst adjList
      -- For each node, create edges to all nodes it's NOT connected to
      let complement = [(n, [(n2, 1.0) | n2 <- nodes, n2 /= n, 
                             not (any (\(neighbor, _) -> neighbor == n2) neighbors)])
                       | (n, neighbors) <- adjList]
      return (GraphValue (Graph complement))
    _ -> throw "GraphComplement requires a Graph type"

evalExpr (FunCall GraphUnion [graph1Expr, graph2Expr]) = do
  graph1Val <- evalExpr graph1Expr
  graph2Val <- evalExpr graph2Expr
  case (graph1Val, graph2Val) of
    (GraphValue (Graph adj1), GraphValue (Graph adj2)) -> do
      -- Combine nodes from both graphs
      let allNodes = map fst adj1 ++ [n | n <- map fst adj2, n `notElem` map fst adj1]
      -- Union of edges for each node
      let unionAdj = [(n, neighbors1 ++ [e | e <- neighbors2, e `notElem` neighbors1])
                     | n <- allNodes
                     , let neighbors1 = maybe [] id (lookup n adj1)
                     , let neighbors2 = maybe [] id (lookup n adj2)]
      return (GraphValue (Graph unionAdj))
    _ -> throw "GraphUnion requires two Graph types"

evalExpr (FunCall GraphIntersection [graph1Expr, graph2Expr]) = do
  graph1Val <- evalExpr graph1Expr
  graph2Val <- evalExpr graph2Expr
  case (graph1Val, graph2Val) of
    (GraphValue (Graph adj1), GraphValue (Graph adj2)) -> do
      -- Only nodes present in both graphs
      let nodes1 = map fst adj1
      let nodes2 = map fst adj2
      let commonNodes = nodes1 `intersect` nodes2
      -- Only edges present in both graphs
      let intersectAdj = [(n, neighbors1 `intersect` neighbors2)
                         | n <- commonNodes
                         , let neighbors1 = maybe [] id (lookup n adj1)
                         , let neighbors2 = maybe [] id (lookup n adj2)]
      return (GraphValue (Graph intersectAdj))
    _ -> throw "GraphIntersection requires two Graph types"

evalExpr (FunCall EsCiclico [graphExpr]) = do
  graphVal <- evalExpr graphExpr
  case graphVal of
    GraphValue (Graph adjList) -> do
      -- Check if graph has a cycle using DFS
      let hasCycle = detectCycle adjList
      return (BoolValue hasCycle)
    _ -> throw "EsCiclico requires a Graph type"
  where
    detectCycle :: [(Node, [(Node, Weight)])] -> Bool
    detectCycle adjList = any (dfs [] []) (map fst adjList)
      where
        dfs visited path node
          | node `elem` path = True  -- Cycle detected
          | node `elem` visited = False
          | otherwise = 
              let neighbors = maybe [] (map fst) (lookup node adjList)
                  newPath = node : path
                  newVisited = node : visited
              in any (dfs newVisited newPath) neighbors

evalExpr (FunCall EsConexo [graphExpr]) = do
  graphVal <- evalExpr graphExpr
  case graphVal of
    GraphValue (Graph adjList) -> do
      -- Check if graph is connected using BFS
      case adjList of
        [] -> return (BoolValue True)
        ((firstNode, _):_) -> do
          let allNodes = map fst adjList
          let reachable = bfsReachable adjList [firstNode] [firstNode]
          return (BoolValue (length reachable == length allNodes))
    _ -> throw "EsConexo requires a Graph type"
  where
    bfsReachable :: [(Node, [(Node, Weight)])] -> [Node] -> [Node] -> [Node]
    bfsReachable _ [] visited = visited
    bfsReachable adjList (node:queue) visited =
      let neighbors = maybe [] (map fst) (lookup node adjList)
          newNeighbors = [n | n <- neighbors, n `notElem` visited]
          newVisited = visited ++ newNeighbors
          newQueue = queue ++ newNeighbors
      in bfsReachable adjList newQueue newVisited

-- Operaciones de List
evalExpr (FunCall HeadList [listExpr]) = do
  listVal <- evalExpr listExpr
  case listVal of
    ListValue (x:_) -> return x
    ListValue [] -> throw "HeadList called on empty list"
    _ -> throw "HeadList requires a List type"

evalExpr (FunCall TailList [listExpr]) = do
  listVal <- evalExpr listExpr
  case listVal of
    ListValue (_:xs) -> return (ListValue xs)
    ListValue [] -> throw "TailList called on empty list"
    _ -> throw "TailList requires a List type"

evalExpr (FunCall Len [listExpr]) = do
  listVal <- evalExpr listExpr
  case listVal of
    ListValue xs -> return (IntValue (fromIntegral (length xs)))
    _ -> throw "Len requires a List type"

evalExpr (FunCall SortByWeight [listExpr]) = do
  listVal <- evalExpr listExpr
  case listVal of
    ListValue edges -> do
      let sortedEdges = sortByEdgeWeight edges
      return (ListValue sortedEdges)
    _ -> throw "SortByWeight requires a List type"
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

evalExpr (FunCall InList [elemExpr, listExpr]) = do
  elemVal <- evalExpr elemExpr
  listVal <- evalExpr listExpr
  case listVal of
    ListValue xs -> return (BoolValue (elemVal `elem` xs))
    _ -> throw "InList requires a List type"

evalExpr (FunCall IsEmptyList [listExpr]) = do
  listVal <- evalExpr listExpr
  case listVal of
    ListValue [] -> return (BoolValue True)
    ListValue _ -> return (BoolValue False)
    _ -> throw "IsEmptyList requires a List type"

-- Operaciones de Queue
evalExpr (FunCall QueueLen [queueExpr]) = do
  queueVal <- evalExpr queueExpr
  case queueVal of
    QueueValue (Queue xs) -> return (IntValue (fromIntegral (length xs)))
    _ -> throw "QueueLen requires a Queue type"

evalExpr (FunCall Enqueue [queueExpr, elemExpr]) = do
  queueVal <- evalExpr queueExpr
  elemVal <- evalExpr elemExpr
  case queueVal of
    QueueValue (Queue xs) -> return (QueueValue (Queue (xs ++ [elemVal])))
    _ -> throw "Enqueue requires a Queue type"

evalExpr (FunCall Dequeue [queueExpr]) = do
  queueVal <- evalExpr queueExpr
  case queueVal of
    QueueValue (Queue (_:xs)) -> return (QueueValue (Queue xs))
    QueueValue (Queue []) -> throw "Dequeue called on empty queue"
    _ -> throw "Dequeue requires a Queue type"

evalExpr (FunCall IsEmptyQueue [queueExpr]) = do
  queueVal <- evalExpr queueExpr
  case queueVal of
    QueueValue (Queue []) -> return (BoolValue True)
    QueueValue (Queue _) -> return (BoolValue False)
    _ -> throw "IsEmptyQueue requires a Queue type"

-- Operaciones de UnionFind
evalExpr (FunCall Find [nodeExpr, ufExpr]) = do
  nodeVal <- evalExpr nodeExpr
  ufVal <- evalExpr ufExpr
  case (nodeVal, ufVal) of
    (_, UnionFindValue (UnionFind pairs)) -> do
      node <- case nodeVal of
                StringValue s -> return s
                _ -> throw "Find requires node to be String type"
      let root = findRoot node pairs
      return (StringValue root)
    _ -> throw "Find requires a UnionFind type"
  where
    findRoot :: Node -> [(Node, Node)] -> Node
    findRoot node pairs =
      case lookup node pairs of
        Just parent | parent == node -> node
        Just parent -> findRoot parent pairs
        Nothing -> error $ "Node not found in UnionFind: " ++ node

evalExpr (FunCall Union [node1Expr, node2Expr, ufExpr]) = do
  node1Val <- evalExpr node1Expr
  node2Val <- evalExpr node2Expr
  ufVal <- evalExpr ufExpr
  case (node1Val, node2Val, ufVal) of
    (_, _, UnionFindValue (UnionFind pairs)) -> do
      node1 <- case node1Val of
                 StringValue s -> return s
                 _ -> throw "Union requires node1 to be String type"
      node2 <- case node2Val of
                 StringValue s -> return s
                 _ -> throw "Union requires node2 to be String type"
      let root1 = findRoot node1 pairs
      let root2 = findRoot node2 pairs
      if root1 == root2
        -- mismo conjunto
        then return (UnionFindValue (UnionFind pairs))
        else do
          -- unimos haciendo que root1 apunte a root2
          let newPairs = map (\(n, p) -> if n == root1 then (n, root2) else (n, p)) pairs
          return (UnionFindValue (UnionFind newPairs))
    _ -> throw "Union requires a UnionFind type"
  where
    findRoot :: Node -> [(Node, Node)] -> Node
    findRoot node pairs =
      case lookup node pairs of
        Just parent | parent == node -> node
        Just parent -> findRoot parent pairs
        Nothing -> error $ "Node '" ++ node ++ "' not found in UnionFind"

-- Las no implementadas tiran error
evalExpr (FunCall _ _) = throw "Function not implemented or invalid arguments"