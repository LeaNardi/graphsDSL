module Main where

import Parser
import Eval
import System.Environment (getArgs)
import System.IO
import ASTGraphs (Graph(..))  -- Importa el tipo y su constructor

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            content <- readFile fileName
            case parseComm "entrada" content of
                Left err  -> print err
                Right ast -> let (env, ticks) = eval ast in
                            case lookup "g" env of
                                Just (Right gr) -> do
                                    putStrLn "Grafo resultante (MST):"
                                    printGraph gr
                                    putStrLn $ "Ticks: " ++ show ticks
                                _ -> print env
        _ -> putStrLn "Uso: ./Main <archivo.dsl>"


-- ./Main Programas/ejemplo_kruskal.dsl
-- ([("g",Right (Graph [(2,[(1,1.0),(3,1.5)]),(1,[(2,1.0)]),(3,[(2,1.5)])]))],1)


printGraph :: Graph -> IO ()
printGraph (Graph adj) = mapM_ printAdj adj
  where
    printAdj (n, edges) = do
        putStr (show n ++ " -> ")
        putStrLn (show edges)

-- {}
-- reglas de ;
-- antes de } no va ;
-- al finalizar no va ;