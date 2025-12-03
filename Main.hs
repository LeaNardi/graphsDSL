module Main where

import System.Environment (getArgs)
import Parser.Parser (parseGraphs)
import Parser.Formatter (formatAST)
import Eval.Eval (eval)
import Eval.Formatter (formatEval)


main :: IO ()
main = do args <- getArgs
          case args of
            [fileName] -> do source <- readFile fileName
                             case parseGraphs fileName source of
                                Left err  -> putStrLn "Error de parseo:" >> print err
                                Right ast -> do
                                    putStrLn "AST parseado:"
                                    putStrLn (formatAST ast)
                                    putStrLn "\nResultado de la evaluación:"
                                    putStrLn (formatEval (eval ast))
            _          ->  putStrLn "Formato esperado: Main.hs Programas/ejemplo.gph"


-- Para compilar:
-- ghc Main.hs
-- Para ejecutar:
-- ./Main Programas/01_arithmetic_simple.gph
-- 


-- Para ejecutar sin compilar:
-- runghc Main.hs Programas/01_arithmetic_simple.gph 