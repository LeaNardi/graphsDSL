module Eval.Eval where

import ASTGraphs ( Comm, Env, Ticks )
import Eval.StateErrorTick ( StateErrorTick(runStateErrorTick), initState )
import Eval.Core ( evalComm )


eval :: Comm -> (Env, Ticks)
eval p = case runStateErrorTick (evalComm p) initState of
    Just (_, s, t) -> (s, t)
    Nothing        -> error "ERROR!"