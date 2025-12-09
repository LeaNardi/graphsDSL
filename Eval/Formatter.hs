module Eval.Formatter where

import ASTGraphs (Env, Ticks, Value(..), Output)

-- Solo agrega saltos de linea e indentacion
formatEval :: Either String (Env, Ticks, Output) -> String
formatEval (Left err) = "Runtime error: " ++ err
formatEval (Right (env, ticks, output)) = 
    "Output:\n" ++ unlines ["  " ++ line | line <- output] ++
    "\nEnvironment:\n" ++ formatEnv env ++ "\n" ++
    "Ticks: " ++ show ticks

formatEnv :: Env -> String
formatEnv [] = "  (empty)"
formatEnv env = unlines ["  " ++ show x| x <- env]
