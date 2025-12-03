module Examples where

import GraphvizExporter

-- Example 1: Simple triangle graph
triangleGraph :: Value
triangleGraph = GraphValue $ Graph 
    [ ("A", [("B", 1.0), ("C", 2.0)])
    , ("B", [("A", 1.0), ("C", 3.0)])
    , ("C", [("A", 2.0), ("B", 3.0)])
    ]

-- Example 2: Complete graph K4
completeGraph4 :: Value
completeGraph4 = GraphValue $ Graph 
    [ ("v1", [("v2", 1.0), ("v3", 1.0), ("v4", 1.0)])
    , ("v2", [("v1", 1.0), ("v3", 1.0), ("v4", 1.0)])
    , ("v3", [("v1", 1.0), ("v2", 1.0), ("v4", 1.0)])
    , ("v4", [("v1", 1.0), ("v2", 1.0), ("v3", 1.0)])
    ]

-- Example 3: Weighted graph for MST algorithms
weightedGraph :: Value
weightedGraph = GraphValue $ Graph 
    [ ("A", [("B", 4.0), ("H", 8.0)])
    , ("B", [("A", 4.0), ("C", 8.0), ("H", 11.0)])
    , ("C", [("B", 8.0), ("D", 7.0), ("F", 4.0), ("I", 2.0)])
    , ("D", [("C", 7.0), ("E", 9.0), ("F", 14.0)])
    , ("E", [("D", 9.0), ("F", 10.0)])
    , ("F", [("C", 4.0), ("D", 14.0), ("E", 10.0), ("G", 2.0)])
    , ("G", [("F", 2.0), ("H", 1.0), ("I", 6.0)])
    , ("H", [("A", 8.0), ("B", 11.0), ("G", 1.0), ("I", 7.0)])
    , ("I", [("C", 2.0), ("G", 6.0), ("H", 7.0)])
    ]

-- Example 4: Simple path graph
pathGraph :: Value
pathGraph = GraphValue $ Graph 
    [ ("Start", [("A", 1.0)])
    , ("A", [("Start", 1.0), ("B", 2.0)])
    , ("B", [("A", 2.0), ("C", 3.0)])
    , ("C", [("B", 3.0), ("End", 4.0)])
    , ("End", [("C", 4.0)])
    ]

-- Example 5: Star graph
starGraph :: Value
starGraph = GraphValue $ Graph 
    [ ("Center", [("A", 1.0), ("B", 1.0), ("C", 1.0), ("D", 1.0), ("E", 1.0)])
    , ("A", [("Center", 1.0)])
    , ("B", [("Center", 1.0)])
    , ("C", [("Center", 1.0)])
    , ("D", [("Center", 1.0)])
    , ("E", [("Center", 1.0)])
    ]

-- Generate all examples
generateAllExamples :: IO ()
generateAllExamples = do
    putStrLn "Generating example graphs..."
    
    writeGraphToDotFile "output/triangle.dot" triangleGraph
    putStrLn "[OK] triangle.dot"
    
    writeGraphToDotFile "output/complete_k4.dot" completeGraph4
    putStrLn "[OK] complete_k4.dot"
    
    writeGraphToDotFile "output/weighted.dot" weightedGraph
    putStrLn "[OK] weighted.dot"
    
    writeGraphToDotFile "output/path.dot" pathGraph
    putStrLn "[OK] path.dot"
    
    writeGraphToDotFile "output/star.dot" starGraph
    putStrLn "[OK] star.dot"
    
    putStrLn "\nTo generate images, run:"
    putStrLn "  cd output"
    putStrLn "  dot -Tpng triangle.dot -o triangle.png"
    putStrLn "  dot -Tpng complete_k4.dot -o complete_k4.png"
    putStrLn "  dot -Tpng weighted.dot -o weighted.png"
    putStrLn "  dot -Tpng path.dot -o path.png"
    putStrLn "  dot -Tpng star.dot -o star.png"
    putStrLn "\nOr generate all at once:"
    putStrLn "  for f in *.dot; do dot -Tpng $f -o ${f%.dot}.png; done"
