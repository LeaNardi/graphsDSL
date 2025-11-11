module Eval.Eval where

import ASTGraphs ( Comm, Env, Ticks )
import Eval.StateErrorTick ( StateErrorTick(runStateErrorTick) )
import Eval.Core ( evalComm )

initState :: Env
initState = []

eval :: Comm -> (Env, Ticks)
eval p = case runStateErrorTick (evalComm p) initState of
    Just (_, s, t) -> (s, t)
    Nothing        -> error "ERROR!"