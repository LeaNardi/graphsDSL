module Main where

import System.Environment (getArgs)
import Parser.Parser (parseGraphs)
import Eval.Eval (eval)


-- Para compilar:
-- ghc Main.hs
-- Para ejecutar:
-- ./Main Tests/Programas/ejemplo_1.gph

-- Para ejecutar sin compilar:
-- runghc Main.hs Tests/Programas/ejemplo_1.gph

main :: IO ()
main = do args <- getArgs
          case args of
            [fileName] -> do source <- readFile fileName
                             case parseGraphs fileName source of
                                Left err  -> putStrLn "Error de parseo:" >> print err
                                Right ast -> do
                                    putStrLn "AST parseado:"
                                    print ast
                                    putStrLn "\nResultado de la evaluación:"
                                    -- print (eval ast)
            _          ->  putStrLn "Formato esperado: Main.hs Programas/ejemplo.gph"



-- Imprimir el resultado de forma más legible por pantalla
--run :: [Char] -> IO ()
--run ifile = do s <- readFile ifile
--               case parseGraphs ifile s of
--                 Left error -> print error
--                 Right ast -> let (env, ticks) = eval ast in
--                   case lookup "g" env of
--                      Just (Right gr) -> do putStrLn "Grafo resultante (MST):"
--                                            printGraph gr
--                                            putStrLn $ "Ticks: " ++ show ticks
--                      _ -> print env

--printGraph :: Graph -> IO ()
--printGraph (Graph adj) = mapM_ printAdj adj
--  where
--    printAdj (n, edges) = do
--        putStr (show n ++ " -> ")
--        putStrLn (show edges)


-- Hacer tests del AST...
-- Skip
-- Output

