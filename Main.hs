module Main where
import System.Environment (getArgs)
import Parser (parseComm)

import ASTGraphs (Graph(..)) 
import Eval
---------------------------------------------------------

main :: IO ()
main = do   args <- getArgs
            case args of
                [fileName] ->  run fileName
                _          ->  putStrLn "Formato esperado: runghc Main.hs Programas/ejemplo_kruskal_1.dsl"

    -- Para compilar:
    -- ghc Main.hs
    -- .\Main Programas/ejemplo_kruskal_1.dsl
    

-- Ejecuta un programa a partir de su archivo fuente
run :: [Char] -> IO ()
run ifile =
    do
    s <- readFile ifile
    case parseComm ifile s of
      Left error -> print error
      --Right t    -> print t        --imprimir sin evaluar (para testear Parser)
    --   Right t    -> print (eval t) --imprimir el resultado de evaluar.
      Right ast -> let (env, ticks) = eval ast in
                            case lookup "g" env of
                                Just (Right gr) -> do
                                    putStrLn "Grafo resultante (MST):"
                                    printGraph gr
                                    putStrLn $ "Ticks: " ++ show ticks
                                _ -> print env


printGraph :: Graph -> IO ()
printGraph (Graph adj) = mapM_ printAdj adj
  where
    printAdj (n, edges) = do
        putStr (show n ++ " -> ")
        putStrLn (show edges)


