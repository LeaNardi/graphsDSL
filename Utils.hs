module Utils where

import ASTGraphs ( Edge, Graph, Node, WEdge, Weight )
import Data.List (sortBy)


addNode :: Node -> Graph -> Graph
addNode n g
  | n `elem` map fst g = g
  | otherwise = (n, []) : g


addDirectedEdge :: Node -> Node -> Weight -> Graph -> Graph
addDirectedEdge u v w [] = [(u, [(v , w)])]
addDirectedEdge u v w ((node, nodesWeights) : otros)
  | u == node = (node, (v, w) : nodesWeights) : otros
  | otherwise = (node, nodesWeights) : addDirectedEdge u v w otros


addEdge :: Node -> Node -> Weight -> Graph -> Graph
addEdge u v w g = addDirectedEdge v u w (addDirectedEdge u v w g)


isUndirected :: Graph -> Bool
isUndirected g = all (\(u, nodesWeights) -> all (\(v, w) -> lookupWeight v u g == Just w) nodesWeights) g


lookupWeight :: Node -> Node -> Graph -> Maybe Weight
lookupWeight u v g = lookup u g >>= lookup v


kruskal :: Graph -> Graph
kruskal g = wEdgesToGraph (kruskalHelper (sortWEdges (graphToWEdges g)) [] [])


graphToWEdges :: Graph -> [WEdge]
graphToWEdges g = [ (u, v, w) | (u, vw) <- g, (v, w) <- vw, u < v ]


sortWEdges :: [WEdge] -> [WEdge]
sortWEdges = sortBy (\(_, _, w1) (_, _, w2) -> compare w1 w2)


kruskalHelper :: [WEdge] -> [Edge] -> [WEdge] -> [WEdge]
kruskalHelper [] _ mst = mst
kruskalHelper ((u, v, w) : wedges) edges mst
  | find u edges /= find v edges = kruskalHelper wedges (union u v edges) ((u, v, w) : mst)
  | otherwise = kruskalHelper wedges edges mst


find :: Node -> [Edge] -> Node
find x edges = case lookup x edges of
  Just y  -> find y edges
  Nothing -> x


union :: Node -> Node -> [Edge] -> [Edge]
union u v edges = (find u edges, find v edges) : edges


wEdgesToGraph :: [WEdge] -> Graph
wEdgesToGraph = foldr (\(u, v, w) -> addEdge u v w) []