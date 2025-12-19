module Main where

import System.Environment (getArgs)
import Parser.Parser (parseGraphs)
import Parser.Formatter (formatAST)
import Eval.Eval (eval)
import Eval.Formatter (formatEval, formatOutput)

main :: IO ()
main = do args <- getArgs
          case args of
            [fileName] -> do source <- readFile fileName
                             case parseGraphs fileName source of
                                Left err  -> putStrLn "Error de parseo:" >> print err

                                -- Para mostrar AST y resultado de Eval:
                                Right ast -> do
                                    putStrLn "AST parseado:"
                                    -- putStrLn (formatAST ast)
                                    print ast
                                    case eval ast of
                                        Left err -> putStrLn "Error de evaluación:" >> putStrLn err
                                        Right (env, ticks, output) -> do
                                            putStrLn "\nResultado de la evaluación:"
                                            putStrLn (formatEval (Right (env, ticks, output)))
                                            putStrLn "\nSalida por pantalla:"
                                            putStrLn (formatOutput (Right (env, ticks, output)))

                                -- Para procesar sin mostrar AST y resutado Eval
                                -- Right ast -> case eval ast of
                                --     Left err -> putStrLn "Error de evaluación:" >> putStrLn err
                                --     Right _ -> return ()
                                    
            _          ->  putStrLn "Formato esperado: Main.hs Programas/ejemplo.gph"


-- Para compilar:
-- ghc Main.hs
-- Para ejecutar:
-- ./Main Programas/01_arithmetic_simple.gph
-- 


-- Para ejecutar sin compilar:
-- runghc Main.hs Programas/01_arithmetic_simple.gph 