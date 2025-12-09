module Eval.Eval where

import ASTGraphs ( Comm, Env, Ticks, Output )
import Eval.StateErrorTick ( StateErrorTick(runStateErrorTick) )
import Eval.Core ( evalComm )

initState :: Env
initState = []

eval :: Comm -> Either String (Env, Ticks, Output)
eval p = case runStateErrorTick (evalComm p) initState of
    Right (_, s, t, o) -> Right (s, t, o)
    Left err           -> Left err