module Eval.Formatter where

import ASTGraphs (Env, Ticks, Value(..))

-- Solo agrega saltos de linea e indentacion
formatEval :: (Env, Ticks) -> String
formatEval (env, ticks) = 
    "Environment:\n" ++ formatEnv env ++ "\n" ++
    "Ticks: " ++ show ticks

formatEnv :: Env -> String
formatEnv [] = "  (empty)"
formatEnv env = unlines ["  " ++ show x| x <- env]
