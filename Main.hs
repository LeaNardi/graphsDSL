module Main where

import System.Environment (getArgs)
import Parser (parseComm)
--import Eval (eval)

main :: IO ()
main = do args <- getArgs
          case args of
            [fileName] ->  run fileName
            _          ->  putStrLn "Formato esperado: Main.hs Programas/ejemplo.gph"

-- Para compilar:
-- ghc Main.hs
-- .\Main Programas/ejemplo.gph

run :: [Char] -> IO ()
run ifile =
    do
      s <- readFile ifile
      case parseComm ifile s of
        Left error -> print error
        Right ast    -> print ast        --imprimir sin evaluar (para testear Parser)
        --Right ast    -> print (eval ast) --imprimir el resultado de evaluar (para testear Eval)
--        Right ast -> let (env, ticks) = eval ast in
--                        case lookup "g" env of
--                          Just (Right gr) -> do
--                                              putStrLn "Grafo resultante (MST):"
--                                              printGraph gr
--                                              putStrLn $ "Ticks: " ++ show ticks
--                          _ -> print env

--printGraph :: Graph -> IO ()
--printGraph (Graph adj) = mapM_ printAdj adj
--  where
--    printAdj (n, edges) = do
--        putStr (show n ++ " -> ")
--        putStrLn (show edges)
