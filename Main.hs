module Main where

import System.Environment (getArgs)
import Parser (parseComm)

-- Evaluador
import Eval
---------------------------------------------------------

main :: IO ()
main = do   args <- getArgs
            case args of
                (arg:_) ->  run arg
                []      ->  putStrLn "Formato esperado: runghc Main.hs Programas/fact.lis"
    

-- Ejecuta un programa a partir de su archivo fuente
run :: [Char] -> IO ()
run ifile =
    do
    s <- readFile ifile
    case parseComm ifile s of
      Left error -> print error
    --   Right t    -> print (eval t) --imprimir el resultado de evaluar.
      Right t    -> print t        --imprimir sin evaluar (para testear Parser)


