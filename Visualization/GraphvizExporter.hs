module GraphvizExporter where

--   traduce a formato graphviz los grafos (graphviz solamente entiende DOT) y exporta 

import qualified Data.List as List
import ASTGraphs (Graph(..), Value(..), Node(..), Weight)

-- Export a graph value to DOT format (Graphviz)
exportGraphToDot :: Value -> Maybe String
exportGraphToDot (GraphValue graph) = Just (graphToDot graph)
exportGraphToDot _ = Nothing

-- Convert a Graph to DOT format
graphToDot :: Graph -> String
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

-- Generate node declarations
nodeDeclarations :: [(String, [(String, Float)])] -> String
nodeDeclarations adjList = 
    unlines ["  \"" ++ node ++ "\";" | (node, _) <- adjList]

-- Generate edge declarations (avoiding duplicates for undirected graphs)
edgeDeclarations :: [(String, [(String, Float)])] -> String
edgeDeclarations adjList = 
    unlines [formatEdge n1 n2 w | (n1, n2, w) <- uniqueEdges]
  where
    allEdges = [(n1, n2, w) | (n1, neighbors) <- adjList, (n2, w) <- neighbors]
    -- For undirected graphs, keep only edges where n1 < n2 to avoid duplicates
    --uniqueEdges = List.nub [(min n1 n2, max n1 n2, w) | (n1, n2, w) <- allEdges]
    uniqueEdges = [ (u, v, w) | (u, v, w) <- allEdges, u < v ]
    formatEdge n1 n2 w = "  \"" ++ n1 ++ "\" -- \"" ++ n2 ++ "\" [label=\"" ++ formatWeight w ++ "\"];"
    formatWeight w = if w == fromInteger (round w) 
                     then show (round w :: Integer)
                     else show w

-- Export graph to a .dot file
writeGraphToDotFile :: FilePath -> Value -> IO ()
writeGraphToDotFile filepath graphValue = 
    case exportGraphToDot graphValue of
        Just dotContent -> writeFile filepath dotContent
        Nothing -> putStrLn "Error: No es un valor de grafo valido"

-- Generate a simple example
exampleUsage :: IO ()
exampleUsage = do
    -- Create a simple graph for demonstration
    let graph = GraphValue $ Graph 
            [ ("A", [("B", 1.0), ("C", 2.0)])
            , ("B", [("A", 1.0), ("C", 3.0), ("D", 4.0)])
            , ("C", [("A", 2.0), ("B", 3.0), ("D", 5.0)])
            , ("D", [("B", 4.0), ("C", 5.0)])
            ]
    
    writeGraphToDotFile "example_graph.dot" graph
    putStrLn "Graph exported to example_graph.dot"
    putStrLn "\nTo visualize, run:"
    putStrLn "  dot -Tpng example_graph.dot -o example_graph.png"
    putStrLn "  dot -Tsvg example_graph.dot -o example_graph.svg"
    putStrLn "  dot -Tpdf example_graph.dot -o example_graph.pdf"
