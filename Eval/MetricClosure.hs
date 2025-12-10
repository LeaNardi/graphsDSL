module Eval.MetricClosure where
import ASTGraphs ( Graph(..), Node, Weight )
import Data.Map.Strict ( Map, fromList, findWithDefault )

metricClosure :: Graph -> (Graph, [(Node, Node, [Node])])
metricClosure (Graph adjList) =
  let nodes = map fst adjList
      infinity :: Float
      infinity = 1/0
      
      -- Busca el peso entre dos nodos; si no hay arista devuelve Nothing
      initialDist :: Node -> Node -> Float
      initialDist n1 n2
        | n1 == n2  = 0
        | otherwise = case lookup n1 adjList of
                        Just neighbors -> case lookup n2 neighbors of
                            Just w  -> realToFrac w
                            Nothing -> infinity
                        Nothing        -> infinity

      -- map inicial de distancias y caminos
      -- Para cada par (i,j), guardamos (distancia, lista de nodos en el camino)
      dist0 :: Map (Node, Node) (Float, [Node])
      dist0 = fromList
        [ ((n1, n2), if initialDist n1 n2 < infinity 
                     then (initialDist n1 n2, [n1, n2])
                     else (infinity, []))
        | n1 <- nodes, n2 <- nodes 
        ]
      
      -- Obtener distancia y camino
      getDistPath :: Map (Node, Node) (Float, [Node]) -> Node -> Node -> (Float, [Node])
      getDistPath dist from to = findWithDefault (infinity, []) (from, to) dist
      
      -- Floyd-Warshall con reconstrucción de caminos
      stepNode :: Map (Node, Node) (Float, [Node]) -> Node -> Map (Node, Node) (Float, [Node])
      stepNode dist k = fromList
        [ let (dij, pathij) = getDistPath dist i j
              (dik, pathik) = getDistPath dist i k
              (dkj, pathkj) = getDistPath dist k j
              dikj = dik + dkj
              -- Si el camino a través de k es mejor, concatenamos los caminos
              newPath = if dikj < dij && not (null pathik) && not (null pathkj)
                        then pathik ++ tail pathkj  -- tail para no duplicar el nodo k
                        else pathij
              newDist = min dij dikj
          in ((i, j), (newDist, newPath))
        | i <- nodes, j <- nodes
        ]
      
      distFinal :: Map (Node, Node) (Float, [Node])
      distFinal = foldl stepNode dist0 nodes
      
      newAdj :: [(Node, [(Node, Weight)])]
      newAdj =
        [ (node,
            [ (target, fst (getDistPath distFinal node target))
            | target <- nodes, node /= target
            ]
          )
        | node <- nodes
        ]
      
      allPaths :: [(Node, Node, [Node])]
      allPaths = 
        [ (i, j, path) 
        | i <- nodes
        , j <- nodes
        , i /= j
        , let (dist, path) = getDistPath distFinal i j
        , dist < infinity
        , not (null path)
        ]
        
  in (Graph newAdj, allPaths)