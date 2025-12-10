module Eval.Core ( evalComm, evalExpr ) where

import ASTGraphs ( Comm(..), Expr(..), BinOpType(..), CompOpType(..), FunctionType(..), Value(..), Graph(..), Node, Edge(..), Queue(..), UnionFind(..), Weight )
import Eval.MonadClasses ( MonadError(..), MonadState(lookfor, update), MonadTick(..), MonadOutput(..) )
import Control.Monad ( when )
import Data.List (intersect, intercalate)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (callCommand)
import Visualization.GraphvizExporter (writeGraphToDotFile)
import System.Directory (createDirectory, createDirectoryIfMissing)

import Eval.MetricClosure ( metricClosure )

evalComm :: (MonadState m, MonadError m, MonadTick m, MonadOutput m) => Comm -> m ()
evalComm Skip = return ()
evalComm (AssignValue v expr) = do val <- evalExpr expr
                                   update v val
evalComm (Seq c1 c2) = do evalComm c1
                          evalComm c2
evalComm (Cond expr c1 c2) = do val <- evalExpr expr
                                case val of
                                  BoolValue True -> evalComm c1
                                  BoolValue False -> evalComm c2
                                  _ -> throw "La expresion condicional tiene que ser Bool"
evalComm (While expr c) = do val <- evalExpr expr
                             case val of
                               BoolValue True -> evalComm (Seq c (While expr c))
                               BoolValue False -> return ()
                               _ -> throw "La condicion del While tiene que ser Bool"
evalComm (For v listExpr c) = do val <- evalExpr listExpr
                                 case val of
                                   ListValue values -> mapM_ (\value -> do update v value; evalComm c) values
                                   _ -> throw "El bucle For requiere una Lista"
evalComm (Visualize graphExpr fileExpr) = do 
  graphVal <- evalExpr graphExpr
  fileVal <- evalExpr fileExpr
  let defaultPath = "Outputs"
  case (graphVal, fileVal) of
    (GraphValue _, StringValue fileName) -> do
      -- Usamos unsafePerformIO para permitir efectos secundarios dentro de la monada
      let !_ = unsafePerformIO $ do
            createDirectoryIfMissing False defaultPath
            let dotFile = defaultPath ++ "/" ++ fileName ++ ".dot"
            let pngFile = defaultPath ++ "/" ++ fileName ++ ".png"
            writeGraphToDotFile dotFile graphVal
            -- Llamamos a un comando externo para generar la imagen PNG desde el archivo DOT
            callCommand $ "dot -Tpng " ++ dotFile ++ " -o " ++ pngFile
      return ()
    (GraphValue _, _) -> throw "el nombre del archivo tiene que ser de tipo String"
    _ -> throw "Visualize necesita un grafo como primer argumento"
evalComm (Print expr) = do 
  val <- evalExpr expr
  appendOutput (formatValue val)
  return ()
  where 
    formatValue (IntValue i)    = show i
    formatValue (FloatValue f)  = show f
    formatValue (BoolValue b)   = show b
    formatValue (StringValue s) = s
    formatValue (NodeValue n)   = n
    formatValue (EdgeValue (Edge n1 n2 w)) = "(" ++ n1 ++ ", " ++ n2 ++ ", " ++ show w ++ ")"
    formatValue (GraphValue g)  = show g
    formatValue (ListValue vs)  = "[" ++ intercalate ", " (map formatValue vs) ++ "]"
    formatValue (QueueValue (Queue vs)) = "Queue: " ++ show (map formatValue vs)
    formatValue (UnionFindValue uf) = show uf

-- Funciones auxiliares
addSimpleEdge :: Graph -> Edge -> Graph
addSimpleEdge (Graph []) (Edge u v w)  = Graph [(u, [(v , w)])]
addSimpleEdge (Graph ((node, nodesWeights) : remaining)) (Edge u v w) 
  | u == node = let listOfNodes = map fst nodesWeights
                in if v `elem` listOfNodes
                   then let updatedWeights = [(n, if n == v then w else wt) | (n, wt) <- nodesWeights]
                        in Graph ((node, updatedWeights) : remaining)
                   else Graph ((node, (v, w) : nodesWeights) : remaining)
  | otherwise = let Graph remaining' = addSimpleEdge (Graph remaining) (Edge u v w)
                in Graph ((node, nodesWeights) : remaining')
                    
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
    _ -> throw "U-minus requiere Int o Float"

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
      Div -> if r' == 0 then throw "Division por zero" else return (IntValue (div l' r'))
      Mod -> if r' == 0 then throw "Modulo por zero" else return (IntValue (mod l' r'))
    -- Operaciones Float 
    (FloatValue l', FloatValue r') -> case op of
      Plus -> return (FloatValue (l' + r'))
      Minus -> return (FloatValue (l' - r'))
      Times -> return (FloatValue (l' * r'))
      Div -> if r' == 0.0 then throw "Division por zero" else return (FloatValue (l' / r'))
      Mod -> throw "La operacion Modulo no es soportada para tipos Float"
    -- Operaciones Mixtas (resultado Float)
    (IntValue l', FloatValue r') -> case op of
      Plus -> return (FloatValue (fromInteger l' + r'))
      Minus -> return (FloatValue (fromInteger l' - r'))
      Times -> return (FloatValue (fromInteger l' * r'))
      Div -> if r' == 0.0 then throw "Division por zero" else return (FloatValue (fromInteger l' / r'))
      Mod -> throw "La operacion Modulo no es soportada para tipos mixtos Int/Float"
    (FloatValue l', IntValue r') -> case op of
      Plus -> return (FloatValue (l' + fromInteger r'))
      Minus -> return (FloatValue (l' - fromInteger r'))
      Times -> return (FloatValue (l' * fromInteger r'))
      Div -> if r' == 0 then throw "Division por zero" else return (FloatValue (l' / fromInteger r'))
      Mod -> throw "La operacion Modulo no es soportada para tipos mixtos Float/Int"
    _ -> throw "Operandos invalidos para operacion aritmetica"

-- Operaciones Booleanas
evalExpr (Not e) = do
  val <- evalExpr e
  case val of
    BoolValue b -> return (BoolValue (not b))
    _ -> throw "La operacion Not requiere que todos sean Bool"

evalExpr (Comparison op l r) = do
  lval <- evalExpr l
  rval <- evalExpr r
  case op of
    Eq -> case (lval, rval) of
      (IntValue l', IntValue r') -> return (BoolValue (l' == r'))
      (FloatValue l', FloatValue r') -> return (BoolValue (l' == r'))
      (BoolValue l', BoolValue r') -> return (BoolValue (l' == r'))
      (StringValue l', StringValue r') -> return (BoolValue (l' == r'))
      _ -> throw "La comparacion de igualdad requiere tipos coincidentes (Int, Float, Bool, o String)"
    Lt -> case (lval, rval) of
      (IntValue l', IntValue r') -> return (BoolValue (l' < r'))
      (FloatValue l', FloatValue r') -> return (BoolValue (l' < r'))
      (IntValue l', FloatValue r') -> return (BoolValue (fromInteger l' < r'))
      (FloatValue l', IntValue r') -> return (BoolValue (l' < fromInteger r'))
      _ -> throw "La comparacion Less than requiere tipos numericos (Int o Float)"
    Gt -> case (lval, rval) of
      (IntValue l', IntValue r') -> return (BoolValue (l' > r'))
      (FloatValue l', FloatValue r') -> return (BoolValue (l' > r'))
      (IntValue l', FloatValue r') -> return (BoolValue (fromInteger l' > r'))
      (FloatValue l', IntValue r') -> return (BoolValue (l' > fromInteger r'))
      _ -> throw "La comparacion Greater than requiere tipos numericos (Int o Float)"
    And -> case (lval, rval) of
      (BoolValue l', BoolValue r') -> return (BoolValue (l' && r'))
      _ -> throw "La operacion And requiere que todos sean Bool"
    Or -> case (lval, rval) of
      (BoolValue l', BoolValue r') -> return (BoolValue (l' || r'))
      _ -> throw "La operacion Or requiere que todos sean Bool"
-- Expresiones Condicionales
evalExpr (Question cond thenE elseE) = do
  condVal <- evalExpr cond
  case condVal of
    BoolValue False -> evalExpr elseE
    BoolValue True -> evalExpr thenE
    _ -> throw "El operador ternario requiere que la condicion sea Bool"

-- Constructores de grafos
evalExpr (ValuedGraph nodeList) = do
  graph <- mapM evalNodeEntry nodeList
  -- validamos que el grafo sea no dirigido
  if not (checkUndirectedGraph graph)
    then throw "Grafo no dirigido invalido: las aristas deben ser simetricas (si existe A->B, debe existir B->A con el mismo peso)"
    else return (GraphValue (Graph graph))
  where
    evalNodeEntry (nodeExpr, adjList) = do
      nodeVal <- evalExpr nodeExpr
      node <- case nodeVal of
        StringValue s -> return s
        _ -> throw "El nodo del grafo debe ser de tipo String"
      adjVals <- mapM evalAdjEntry adjList
      return (node, adjVals)
    evalAdjEntry (nodeExpr, weightExpr) = do
      nodeVal <- evalExpr nodeExpr
      weightVal <- evalExpr weightExpr
      node <- case nodeVal of
        StringValue s -> return s
        _ -> throw "El nodo adyacente del grafo debe ser de tipo String"
      weight <- case weightVal of
        FloatValue w -> return w
        IntValue w -> return (fromInteger w)
        _ -> throw "El peso de la arista del grafo debe ser de tipo Float o Int"
      return (node, weight)

evalExpr (ValuedEdge n1Expr n2Expr wExpr) = do
  n1Val <- evalExpr n1Expr
  n2Val <- evalExpr n2Expr
  wVal <- evalExpr wExpr
  n1 <- case n1Val of
    StringValue s -> return s
    _ -> throw "El nodo1 de la arista debe ser de tipo String"
  n2 <- case n2Val of
    StringValue s -> return s
    _ -> throw "El nodo2 de la arista debe ser de tipo String"
  w <- case wVal of
    FloatValue f -> return f
    IntValue i -> return (fromInteger i)
    _ -> throw "El peso de la arista debe ser de tipo Float o Int"
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
                _ -> throw "Los elementos de UnionFind deben ser de tipo String"
      parent <- case parentVal of
                  StringValue s -> return s
                  _ -> throw "Los padres de UnionFind deben ser de tipo String"
      return (node, parent)

-- Operaciones de Edge
evalExpr (FunCall GetNode1 [edgeExpr]) = do
  edgeVal <- evalExpr edgeExpr
  case edgeVal of
    EdgeValue (Edge n1 _ _) -> return (StringValue n1)
    _ -> throw "GetNode1 requiere un tipo Edge"
evalExpr (FunCall GetNode2 [edgeExpr]) = do
  edgeVal <- evalExpr edgeExpr
  case edgeVal of
    EdgeValue (Edge _ n2 _) -> return (StringValue n2)
    _ -> throw "GetNode2 requiere un tipo Edge"

evalExpr (FunCall GetWeight [edgeExpr]) = do
  edgeVal <- evalExpr edgeExpr
  case edgeVal of
    EdgeValue (Edge _ _ w) -> return (FloatValue w)
    _ -> throw "GetWeight requiere un tipo Edge"

-- Operaciones de Graph
evalExpr (FunCall AddNode [graphExpr, nodeExpr]) = do
  graphVal <- evalExpr graphExpr
  nodeVal <- evalExpr nodeExpr
  case graphVal of
    GraphValue (Graph adjList) -> do
      node <- case nodeVal of
        StringValue s -> return s
        _ -> throw "AddNode requiere que el nodo sea de tipo String"
      if node `elem` map fst adjList
        then return (GraphValue (Graph adjList))
        else return (GraphValue (Graph ((node, []) : adjList)))
    _ -> throw "AddNode requiere un tipo Graph"

evalExpr (FunCall AddEdge [graphExpr, edgeExpr]) = do
  graphVal <- evalExpr graphExpr
  edgeVal <- evalExpr edgeExpr
  case graphVal of
    GraphValue graph -> do
      case edgeVal of
        EdgeValue e -> do
          let (Edge node1 node2 weight) = e
          return (GraphValue (addSimpleEdge (addSimpleEdge graph (Edge node1 node2 weight)) (Edge node2 node1 weight)))
        _ -> throw "AddEdge requiere que la arista sea de tipo Edge"
    _ -> throw "AddEdge requiere un tipo Graph"

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
    _ -> throw "GetEdges requiere un tipo Graph"

evalExpr (FunCall DeleteNode [graphExpr, nodeExpr]) = do
  graphVal <- evalExpr graphExpr
  nodeVal <- evalExpr nodeExpr
  case graphVal of
    GraphValue (Graph adjList) -> do
      node <- case nodeVal of
        StringValue s -> return s
        _ -> throw "DeleteNode requiere que el nodo sea de tipo String"
      -- Eliminar nodo de la lista de adyacencia y eliminar aristas que apuntan a él
      let adjList' = [(n, [(n2, w) | (n2, w) <- neighbors, n2 /= node]) 
                     | (n, neighbors) <- adjList, n /= node]
      return (GraphValue (Graph adjList'))
    _ -> throw "DeleteNode requiere un tipo Graph"

evalExpr (FunCall DeleteEdge [graphExpr, edgeExpr]) = do
  graphVal <- evalExpr graphExpr
  edgeVal <- evalExpr edgeExpr
  case graphVal of
    GraphValue (Graph adjList) -> do
      edge <- case edgeVal of
        EdgeValue e -> return e
        _ -> throw "DeleteEdge requiere que la arista sea de tipo Edge"
      -- Eliminar arista de la lista de adyacencia
      let (Edge node1 node2 _) = edge
      let adjList' = [(n1, [(n2, w) | (n2, w) <- neighbors, not (n1 == node1 && n2 == node2) && not (n1 == node2 && n2 == node1)])
                     | (n1, neighbors) <- adjList]
      return (GraphValue (Graph adjList'))
    _ -> throw "DeleteEdge requiere un tipo Graph"


evalExpr (FunCall AdjacentNodes [graphExpr, nodeExpr]) = do
  graphVal <- evalExpr graphExpr
  nodeVal <- evalExpr nodeExpr
  case graphVal of
    GraphValue (Graph adjList) -> do
      node <- case nodeVal of
        StringValue s -> return s
        _ -> throw "AdjacentNodes requiere que el nodo sea de tipo String"
      case lookup node adjList of
        Just neighbors -> return (ListValue [StringValue n | (n, _) <- neighbors])
        Nothing -> throw $ "El Nodo '" ++ node ++ "' no se encontró en el grafo"
    _ -> throw "AdjacentNodes requiere un tipo Graph"
evalExpr (FunCall AdjacentEdges [graphExpr, nodeExpr]) = do
  graphVal <- evalExpr graphExpr
  nodeVal <- evalExpr nodeExpr
  case graphVal of
    GraphValue (Graph adjList) -> do
      node <- case nodeVal of
        StringValue s -> return s
        _ -> throw "AdjacentEdges requiere que el nodo sea de tipo String"
      case lookup node adjList of
        Just neighbors -> return (ListValue [EdgeValue (Edge node n w) | (n, w) <- neighbors]) -- Muestra aristas en orden nodo -> n (salientes)
        Nothing -> throw $ "El Nodo '" ++ node ++ "' no se encontró en el grafo"
    _ -> throw "AdjacentEdges requiere un tipo Graph"
evalExpr (FunCall GraphComplement [graphExpr]) = do
  graphVal <- evalExpr graphExpr
  case graphVal of
    GraphValue (Graph adjList) -> do
      let nodes = map fst adjList
      -- Para cada nodo, crea aristas a todos los nodos a los que NO está conectado
      let complement = [(n, [(n2, 0.0) | n2 <- nodes, n2 /= n, 
                             not (any (\(neighbor, _) -> neighbor == n2) neighbors)])
                       | (n, neighbors) <- adjList]
      return (GraphValue (Graph complement))
    _ -> throw "GraphComplement requiere un tipo Graph"
evalExpr (FunCall GraphUnion [graph1Expr, graph2Expr]) = do
  graph1Val <- evalExpr graph1Expr
  graph2Val <- evalExpr graph2Expr
  case (graph1Val, graph2Val) of
    (GraphValue (Graph adj1), GraphValue (Graph adj2)) -> do
      -- Combina nodos de ambos grafos
      let allNodes = map fst adj1 ++ [n | n <- map fst adj2, n `notElem` map fst adj1]
      -- Unión de aristas para cada nodo
      let unionAdj = [(n, neighbors1 ++ [e | e <- neighbors2, e `notElem` neighbors1])
                     | n <- allNodes
                     , let neighbors1 = maybe [] id (lookup n adj1)
                     , let neighbors2 = maybe [] id (lookup n adj2)]
      return (GraphValue (Graph unionAdj))
    _ -> throw "GraphUnion requiere dos tipos Graph"

evalExpr (FunCall GraphIntersection [graph1Expr, graph2Expr]) = do
  graph1Val <- evalExpr graph1Expr
  graph2Val <- evalExpr graph2Expr
  case (graph1Val, graph2Val) of
    (GraphValue (Graph adj1), GraphValue (Graph adj2)) -> do
      -- Solo nodos presentes en ambos grafos
      let nodes1 = map fst adj1
      let nodes2 = map fst adj2
      let commonNodes = nodes1 `intersect` nodes2
      -- Solo aristas presentes en ambos grafos
      let intersectAdj = [(n, neighbors1 `intersect` neighbors2)
                         | n <- commonNodes
                         , let neighbors1 = maybe [] id (lookup n adj1)
                         , let neighbors2 = maybe [] id (lookup n adj2)]
      return (GraphValue (Graph intersectAdj))
    _ -> throw "GraphIntersection requiere dos tipos Graph"

evalExpr (FunCall EsCiclico [graphExpr]) = do
  graphVal <- evalExpr graphExpr
  case graphVal of
    GraphValue (Graph adjList) -> do
      -- Verificar si el grafo tiene un ciclo usando DFS
      let hasCycle = detectCycle adjList
      return (BoolValue hasCycle)
    _ -> throw "EsCiclico requiere un tipo Graph"
  where
    detectCycle :: [(Node, [(Node, Weight)])] -> Bool
    detectCycle adjList = any (dfs [] []) (map fst adjList)
      where
        dfs visited path node
          | node `elem` path = True  -- Ciclo detectado
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
      -- Verificacion si el grafo es conexo usando BFS
      case adjList of
        [] -> return (BoolValue True)
        ((firstNode, _):_) -> do
          let allNodes = map fst adjList
          let reachable = bfsReachable adjList [firstNode] [firstNode]
          return (BoolValue (length reachable == length allNodes))
    _ -> throw "EsConexo requiere un tipo Graph"
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
evalExpr (FunCall LenList [listExpr]) = do
  listVal <- evalExpr listExpr
  case listVal of
    ListValue xs -> return (IntValue (fromIntegral (length xs)))
    _ -> throw "LenList requiere un tipo List"

evalExpr (FunCall AppendList [listExpr, elemExpr]) = do
  listVal <- evalExpr listExpr
  elemVal <- evalExpr elemExpr
  case listVal of
    ListValue xs -> return (ListValue (xs ++ [elemVal]))
    _ -> throw "AppendList requiere un tipo List"

evalExpr (FunCall ConsList [listExpr, elemExpr]) = do
  listVal <- evalExpr listExpr
  elemVal <- evalExpr elemExpr
  case listVal of
    ListValue xs -> return (ListValue (elemVal:xs))
    _ -> throw "ConsList requiere un tipo List"

evalExpr (FunCall ConcatList [listExpr1, listExpr2]) = do
  listVal1 <- evalExpr listExpr1
  listVal2 <- evalExpr listExpr2
  case (listVal1, listVal2) of
    (ListValue xs, ListValue ys) -> return (ListValue (xs ++ ys))
    _ -> throw "ConcatList requiere de tipos List"

evalExpr (FunCall HeadList [listExpr]) = do
  listVal <- evalExpr listExpr
  case listVal of
    ListValue (x:_) -> return x
    ListValue [] -> throw "HeadList llamado en una lista vacía"
    _ -> throw "HeadList requiere un tipo List"

evalExpr (FunCall LastList [listExpr]) = do
  listVal <- evalExpr listExpr
  case listVal of
    ListValue [] -> throw "LastList llamado en una lista vacía"
    ListValue (xs) -> return (last xs)
    _ -> throw "LastList requiere un tipo List"

evalExpr (FunCall TailList [listExpr]) = do
  listVal <- evalExpr listExpr
  case listVal of
    ListValue (_:xs) -> return (ListValue xs)
    ListValue [] -> throw "TailList llamado en una lista vacía"
    _ -> throw "TailList requiere un tipo List"

evalExpr (FunCall InitList [listExpr]) = do
  listVal <- evalExpr listExpr
  case listVal of
    ListValue [] -> throw "InitList llamado en una lista vacía"
    ListValue xs -> return (ListValue (init xs))
    _ -> throw "InitList requiere un tipo List"

evalExpr (FunCall ReverseList [listExpr]) = do
  listVal <- evalExpr listExpr
  case listVal of
    ListValue xs -> return (ListValue (reverse xs))
    _ -> throw "ReverseList requiere un tipo List"

evalExpr (FunCall SortByWeight [listExpr]) = do
  listVal <- evalExpr listExpr
  case listVal of
    ListValue edges -> do
      let sortedEdges = sortByEdgeWeight edges
      return (ListValue sortedEdges)
    _ -> throw "SortByWeight requiere un tipo List de Edges"
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
    _ -> throw "InList requiere un tipo List"

evalExpr (FunCall IsEmptyList [listExpr]) = do
  listVal <- evalExpr listExpr
  case listVal of
    ListValue [] -> return (BoolValue True)
    ListValue _ -> return (BoolValue False)
    _ -> throw "IsEmptyList requiere un tipo List"

-- Operaciones de Queue
evalExpr (FunCall QueueLen [queueExpr]) = do
  queueVal <- evalExpr queueExpr
  case queueVal of
    QueueValue (Queue xs) -> return (IntValue (fromIntegral (length xs)))
    _ -> throw "QueueLen requiere un tipo Queue"

evalExpr (FunCall Enqueue [queueExpr, elemExpr]) = do
  queueVal <- evalExpr queueExpr
  elemVal <- evalExpr elemExpr
  case queueVal of
    QueueValue (Queue xs) -> return (QueueValue (Queue (xs ++ [elemVal])))
    _ -> throw "Enqueue requiere un tipo Queue"

evalExpr (FunCall Dequeue [queueExpr]) = do
  queueVal <- evalExpr queueExpr
  case queueVal of
    QueueValue (Queue (_:xs)) -> return (QueueValue (Queue xs))
    QueueValue (Queue []) -> throw "Dequeue llamado en una cola vacía"
    _ -> throw "Dequeue requiere un tipo Queue"

evalExpr (FunCall Peek [queueExpr]) = do
  queueVal <- evalExpr queueExpr
  case queueVal of
    QueueValue (Queue (x:_)) -> return x
    QueueValue (Queue []) -> throw "Peek llamado en una cola vacía"
    _ -> throw "Peek requiere un tipo Queue"

evalExpr (FunCall IsEmptyQueue [queueExpr]) = do
  queueVal <- evalExpr queueExpr
  case queueVal of
    QueueValue (Queue []) -> return (BoolValue True)
    QueueValue (Queue _) -> return (BoolValue False)
    _ -> throw "IsEmptyQueue requiere un tipo Queue"

-- Operaciones de UnionFind
evalExpr (FunCall Find [nodeExpr, ufExpr]) = do
  nodeVal <- evalExpr nodeExpr
  ufVal <- evalExpr ufExpr
  case (nodeVal, ufVal) of
    (_, UnionFindValue (UnionFind pairs)) -> do
      node <- case nodeVal of
                StringValue s -> return s
                _ -> throw "Find requiere que node sea de tipo String"
      case findRoot node pairs of
        Just root -> return (StringValue root)
        Nothing -> throw ("El Nodo '" ++ node ++ "' no se encontró en UnionFind")
    _ -> throw "Find requiere un tipo UnionFind"
  where
    findRoot :: Node -> [(Node, Node)] -> Maybe Node
    findRoot node pairs =
      case lookup node pairs of
        Just parent | parent == node -> Just node
        Just parent -> findRoot parent pairs
        Nothing -> Nothing
evalExpr (FunCall Union [node1Expr, node2Expr, ufExpr]) = do
  node1Val <- evalExpr node1Expr
  node2Val <- evalExpr node2Expr
  ufVal <- evalExpr ufExpr
  case (node1Val, node2Val, ufVal) of
    (_, _, UnionFindValue (UnionFind pairs)) -> do
      node1 <- case node1Val of
                 StringValue s -> return s
                 _ -> throw "Union requiere que node1 sea de tipo String"
      node2 <- case node2Val of
                 StringValue s -> return s
                 _ -> throw "Union requiere que node2 sea de tipo String"
      root1 <- case findRoot node1 pairs of
        Just root -> return root
        Nothing -> throw ("El Nodo '" ++ node1 ++ "' no se encontró en UnionFind")
      root2 <- case findRoot node2 pairs of
        Just root -> return root
        Nothing -> throw ("El Nodo '" ++ node2 ++ "' no se encontró en UnionFind")
      if root1 == root2
        -- mismo conjunto
        then return (UnionFindValue (UnionFind pairs))
        else do
          -- unimos haciendo que root1 apunte a root2
          let newPairs = map (\(n, p) -> if n == root1 then (n, root2) else (n, p)) pairs
          return (UnionFindValue (UnionFind newPairs))
    _ -> throw "Union requiere un tipo UnionFind"
  where
    findRoot :: Node -> [(Node, Node)] -> Maybe Node
    findRoot node pairs =
      case lookup node pairs of
        Just parent | parent == node -> Just node
        Just parent -> findRoot parent pairs
        Nothing -> Nothing

evalExpr (FunCall MetricClosure [graphExpr]) = do
  graphVal <- evalExpr graphExpr
  case graphVal of
    GraphValue g ->
      return (GraphValue (fst (metricClosure g)))
    _ -> throw "MetricClosure requiere un tipo Graph"

evalExpr (FunCall MetricClosurePaths [graphExpr]) = do
  graphVal <- evalExpr graphExpr
  case graphVal of
    GraphValue g ->
      let paths = snd (metricClosure g)
          -- Convert to list of lists: [[source, target, [intermediate nodes]]]
          pathsAsList = map (\(src, tgt, path) -> 
                              ListValue [StringValue src, 
                                        StringValue tgt, 
                                        ListValue (map StringValue path)]
                            ) paths
      in return (ListValue pathsAsList)
    _ -> throw "MetricClosurePaths requiere un tipo Graph"

-- Las no implementadas tiran error, aunque si el parser esta bien hecho, evita que lleguemos a este punto
evalExpr (FunCall f args) = throw ("La funcion " ++ show f ++ "(" ++ show args ++ ")" ++ " no fue implementada o los argumentos son invalidos")