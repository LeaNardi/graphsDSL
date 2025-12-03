module Main where

import System.Directory (createDirectoryIfMissing)
import GraphvizExporter
import Examples

main :: IO ()
main = do
    putStrLn "=== GraphsDSL Visualization Demo ===\n"
    
    -- Create output directory
    createDirectoryIfMissing True "output"
    
    -- Generate example graphs
    generateAllExamples
    