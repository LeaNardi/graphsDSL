module Eval.Eval where

import ASTGraphs ( Comm, Env, Ticks )
import Eval.StateErrorTick ( StateErrorTick(runStateErrorTick) )
import Eval.Core ( evalComm )

initState :: Env
initState = []

eval :: Comm -> Either String (Env, Ticks)
eval p = case runStateErrorTick (evalComm p) initState of
    Right (_, s, t) -> Right (s, t)
    Left err        -> Left err