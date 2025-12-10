module Visualization.GraphvizExporter where
import ASTGraphs (Graph(..), Value(..), Node(..), Weight)

-- Arma la lista de nodos en formato DOT
nodeDeclarations :: [(String, [(String, Float)])] -> String
nodeDeclarations adjList = 
    unlines ["  \"" ++ node ++ "\";" | (node, _) <- adjList]

-- Arma la lista de aristas en formato DOT
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


-- Convierte un grafo a formato DOT y le da formato
graphToDot :: Graph -> String
-- Documentacion de graphviz.org:
-- neato attempts to minimize a global energy function, which is equivalent to statistical multi-dimensional scaling.
-- overlap – Determines if and how node overlaps should be removed. Valid on: Graphs.
graphToDot (Graph adjList) = 
    "graph G {\n" ++
    "  label= \"Grafo:\";\n" ++
    "  layout=neato;\n" ++
    "  overlap=false;\n" ++
    "  node [shape=circle, style=filled, fillcolor=lightgrey];\n" ++
    "  edge [color=gray];\n" ++
    "  \n" ++
    "  // Nodes\n" ++
    nodeDeclarations adjList ++
    "  \n" ++
    "  // Edges\n" ++
    edgeDeclarations adjList ++
    "}\n"

-- Seguridad de tipos para exportar solo grafos
exportGraphToDot :: Value -> Maybe String
exportGraphToDot (GraphValue graph) = Just (graphToDot graph)
exportGraphToDot _ = Nothing

-- Arma el archivo DOT
writeGraphToDotFile :: FilePath -> Value -> IO ()
writeGraphToDotFile filepath graphValue = 
    case exportGraphToDot graphValue of
        Just dotContent -> writeFile filepath dotContent
        Nothing -> putStrLn "Error: No es un valor de grafo valido para exportar a dot"
