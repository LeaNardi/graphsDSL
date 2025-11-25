module Main where

import System.Environment (getArgs)
import Parser.Parser (parseGraphs)
import Parser.Formatter (formatAST)
-- import Eval.Eval (eval)
-- import Eval.Formatter (formatEval)


main :: IO ()
main = do args <- getArgs
          case args of
            [fileName] -> do source <- readFile fileName
                             case parseGraphs fileName source of
                                Left err  -> putStrLn "Error de parseo:" >> print err
                                Right ast -> do
                                    putStrLn "AST parseado:"
                                    -- print ast
                                    putStrLn (formatAST ast)
                                    putStrLn "\nResultado de la evaluación:"
                                    -- print (eval ast)
                                    -- putStrLn (formatEval (eval ast))
            _          ->  putStrLn "Formato esperado: Main.hs Programas/ejemplo.gph"


-- Para compilar:
-- ghc Main.hs
-- Para ejecutar:
-- ./Main Tests/Programas/ejemplo_1.gph

-- Para ejecutar sin compilar:
-- runghc Main.hs Tests/Programas/ejemplo_1.gph