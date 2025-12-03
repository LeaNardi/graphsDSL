module Visualization.GraphvizExporter where
import ASTGraphs (Graph(..), Value(..), Node(..), Weight)

nodeDeclarations :: [(String, [(String, Float)])] -> String
nodeDeclarations adjList = 
    unlines ["  \"" ++ node ++ "\";" | (node, _) <- adjList]


edgeDeclarations :: [(String, [(String, Float)])] -> String
edgeDeclarations adjList = 
    unlines [formatEdge n1 n2 w | (n1, n2, w) <- uniqueEdges]
  where
    allEdges = [(n1, n2, w) | (n1, neighbors) <- adjList, (n2, w) <- neighbors]
    uniqueEdges = [ (u, v, w) | (u, v, w) <- allEdges, u < v ]
    formatEdge n1 n2 w = "  \"" ++ n1 ++ "\" -- \"" ++ n2 ++ "\" [label=\"" ++ formatWeight w ++ "\"];"
    formatWeight w = if w == fromInteger (round w) 
                     then show (round w :: Integer)
                     else show w

graphToDot :: Graph -> String
-- Documentacion de graphviz.org:
-- neato attempts to minimize a global energy function, which is equivalent to statistical multi-dimensional scaling.
-- overlap – Determines if and how node overlaps should be removed. Valid on: Graphs.
graphToDot (Graph adjList) = 
    "graph G {\n" ++
    "  layout=neato;\n" ++
    "  overlap=false;\n" ++
    "  node [shape=circle, style=filled, fillcolor=lightblue];\n" ++
    "  edge [color=gray];\n" ++
    "  \n" ++
    "  // Nodes\n" ++
    nodeDeclarations adjList ++
    "  \n" ++
    "  // Edges\n" ++
    edgeDeclarations adjList ++
    "}\n"


exportGraphToDot :: Value -> Maybe String
exportGraphToDot (GraphValue graph) = Just (graphToDot graph)
exportGraphToDot _ = Nothing


writeGraphToDotFile :: FilePath -> Value -> IO ()
writeGraphToDotFile filepath graphValue = 
    case exportGraphToDot graphValue of
        Just dotContent -> writeFile filepath dotContent
        Nothing -> putStrLn "Error: No es un valor de grafo valido"
