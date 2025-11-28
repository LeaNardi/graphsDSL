{-# LANGUAGE InstanceSigs #-}

module Eval.StateErrorTick where

import ASTGraphs ( Env, Ticks, Variable, Value )
import Eval.MonadClasses ( MonadTick(..), MonadError(..), MonadState(..) )
import Control.Monad ( ap )


newtype StateErrorTick a = StateErrorTick { runStateErrorTick :: Env -> Maybe (a, Env, Ticks) }


instance Functor StateErrorTick where

    fmap :: (a -> b) -> StateErrorTick a -> StateErrorTick b
    fmap f (StateErrorTick g) = StateErrorTick { runStateErrorTick = fmap (\ (a, s', t) -> (f a, s', t)) . g }


instance Applicative StateErrorTick where

    pure :: a -> StateErrorTick a
    pure x = StateErrorTick { runStateErrorTick = \ s -> Just (x, s, 0) }

    (<*>) :: StateErrorTick (a -> b) -> StateErrorTick a -> StateErrorTick b
    (<*>) = ap


instance Monad StateErrorTick where

    return :: a -> StateErrorTick a
    return = pure

    (>>=) :: StateErrorTick a -> (a -> StateErrorTick b) -> StateErrorTick b
    m >>= f = StateErrorTick { runStateErrorTick = \ s
                                                      -> do (v, s', t) <- runStateErrorTick m s
                                                            (v', s'', t') <- runStateErrorTick (f v) s'
                                                            return (v', s'', t + t') }


instance MonadState StateErrorTick where

    lookfor :: ASTGraphs.Variable -> StateErrorTick ASTGraphs.Value
    lookfor var = StateErrorTick { runStateErrorTick = \ s -> (\ v -> Just (v, s, 0)) =<< lookup var s }
    
    update :: ASTGraphs.Variable -> ASTGraphs.Value -> StateErrorTick ()
    update var val = StateErrorTick { runStateErrorTick = \ s -> Just ((), (var, val) : filter ((/= var) . fst) s, 0) }
    
    saveState :: StateErrorTick Env
    saveState = StateErrorTick { runStateErrorTick = \ s -> Just (s, s, 0) }
    
    putState :: Env -> StateErrorTick ()
    putState r = StateErrorTick { runStateErrorTick = \ _ -> Just ((), r, 0) }


instance MonadError StateErrorTick where
    
    throw :: StateErrorTick a
    throw = StateErrorTick { runStateErrorTick = const Nothing }


instance MonadTick StateErrorTick where
    
    tick :: StateErrorTick ()
    tick = StateErrorTick { runStateErrorTick = \ s -> Just ((), s, 1) }