module Eval.Utils where

import ASTGraphs ( Edge(..), Graph(..), Node, Weight )
import Data.List (sortBy)


isUndirected :: Graph -> Bool
isUndirected (Graph g) = all (\(u, nodesWeights) -> all (\(v, w) -> lookupWeight v u (Graph g) == Just w) nodesWeights) g


lookupWeight :: Node -> Node -> Graph -> Maybe Weight
lookupWeight u v (Graph g) = lookup u g >>= lookup v


kruskal :: Graph -> Graph
kruskal g = wEdgesToGraph (kruskalHelper (sortWEdges (graphToWEdges g)) [] [])


graphToWEdges :: Graph -> [Edge]
graphToWEdges (Graph g) = [ Edge u v w | (u, vw) <- g, (v, w) <- vw, u < v ]


sortWEdges :: [Edge] -> [Edge]
sortWEdges = sortBy (\(Edge _ _ w1) (Edge _ _ w2) -> compare w1 w2)


kruskalHelper :: [Edge] -> [(Node, Node)] -> [Edge] -> [Edge]
kruskalHelper [] _ mst = mst
kruskalHelper (Edge u v w : wedges) edges mst
  | find u edges /= find v edges = kruskalHelper wedges (union u v edges) (Edge u v w : mst)
  | otherwise = kruskalHelper wedges edges mst


find :: Node -> [(Node, Node)] -> Node
find x edges = case lookup x edges of
  Just y  -> find y edges
  Nothing -> x


union :: Node -> Node -> [(Node, Node)] -> [(Node, Node)]
union u v edges = (find u edges, find v edges) : edges


wEdgesToGraph :: [Edge] -> Graph
wEdgesToGraph = foldr (\(Edge u v w) -> addEdge u v w) (Graph [])