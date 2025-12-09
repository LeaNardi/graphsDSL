module Eval.MonadClasses where

import ASTGraphs ( Variable, Value, Env )


class Monad m => MonadState m where

    lookfor :: Variable -> m Value
    update :: Variable -> Value -> m ()
    saveState :: m Env
    putState :: Env -> m ()


class Monad m => MonadError m where

    throw :: String -> m a


class Monad m => MonadTick m where
    
    tick :: m ()


class Monad m => MonadOutput m where
    
    appendOutput :: String -> m ()