module Eval where

import ASTGraphs ( Comm, Env, Ticks )
import StateErrorTick ( StateErrorTick(runStateErrorTick), initState )
import EvalCore ( evalComm )


eval :: Comm -> (Env, Ticks)
eval p = case runStateErrorTick (evalComm p) initState of
    Just (_, s, t) -> (s, t)
    Nothing        -> error "ERROR!"