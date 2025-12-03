module Main where

import System.Directory (createDirectoryIfMissing)
import GraphvizExporter
import Examples

main :: IO ()
main = do
    putStrLn "=== 1, 2, 3 probando GraphsDSL ==\n"
    createDirectoryIfMissing True "output" --directorio de outptut
    generateAllExamples --generar los ejemplos
    