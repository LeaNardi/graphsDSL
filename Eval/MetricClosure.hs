module Eval.MetricClosure where
import ASTGraphs ( Graph(..), Node, Weight )
import Data.Map.Strict ( Map, fromList, findWithDefault )

metricClosure :: Graph -> Graph
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

      -- map inicial de distancias
      dist0 :: Map (Node, Node) Float
      dist0 = fromList
        [ ((n1, n2), initialDist n1 n2) | n1 <- nodes, n2 <- nodes ]
      
      
      -- Floyd-Warshall: iterate through each intermediate node
      getDist :: Map (Node, Node) Float -> Node -> Node -> Float
      getDist dist from to = findWithDefault infinity (from, to) dist
      
      stepNode :: Map (Node, Node) Float -> Node -> Map (Node, Node) Float
      stepNode dist k = fromList
        [ ((i, j), min (getDist dist i j) (getDist dist i k + getDist dist k j))
        | i <- nodes, j <- nodes
        ]
      
      distFinal :: Map (Node, Node) Float
      distFinal = foldl stepNode dist0 nodes
      
      -- Build new adjacency list
      newAdj :: [(Node, [(Node, Weight)])]
      newAdj =
        [ (node,
            [ (target, getDist distFinal node target)
            | target <- nodes, node /= target
            ]
          )
        | node <- nodes
        ]
  in Graph newAdj