{-# LANGUAGE InstanceSigs #-}

module Eval.StateErrorTick (StateErrorTick(..)) where

import ASTGraphs ( Env, Ticks, Variable, Value, Output )
import Eval.MonadClasses ( MonadTick(..), MonadError(..), MonadState(..), MonadOutput(..) )
import Control.Monad ( ap )


newtype StateErrorTick a = StateErrorTick { runStateErrorTick :: Env -> Either String (a, Env, Ticks, Output) }


instance Functor StateErrorTick where

    fmap :: (a -> b) -> StateErrorTick a -> StateErrorTick b
    fmap f (StateErrorTick g) = StateErrorTick { runStateErrorTick = \s -> 
        case g s of
            Left err -> Left err
            Right (a, s', t, o) -> Right (f a, s', t, o) }


instance Applicative StateErrorTick where

    pure :: a -> StateErrorTick a
    pure x = StateErrorTick { runStateErrorTick = \ s -> Right (x, s, 0, []) }

    (<*>) :: StateErrorTick (a -> b) -> StateErrorTick a -> StateErrorTick b
    (<*>) = ap


instance Monad StateErrorTick where

    return :: a -> StateErrorTick a
    return = pure

    (>>=) :: StateErrorTick a -> (a -> StateErrorTick b) -> StateErrorTick b
    m >>= f = StateErrorTick { runStateErrorTick = \ s
                                                      -> do (v, s', t, o) <- runStateErrorTick m s
                                                            (v', s'', t', o') <- runStateErrorTick (f v) s'
                                                            return (v', s'', t + t', o ++ o') }


instance MonadState StateErrorTick where

    lookfor :: ASTGraphs.Variable -> StateErrorTick ASTGraphs.Value
    lookfor var = StateErrorTick { runStateErrorTick = \ s -> 
        case lookup var s of
            Just v -> Right (v, s, 0, [])
            Nothing -> Left $ "Variable '" ++ var ++ "' not found" }
    
    update :: ASTGraphs.Variable -> ASTGraphs.Value -> StateErrorTick ()
    update var val = StateErrorTick { runStateErrorTick = \ s -> Right ((), (var, val) : filter ((/= var) . fst) s, 0, []) }
    
    saveState :: StateErrorTick Env
    saveState = StateErrorTick { runStateErrorTick = \ s -> Right (s, s, 0, []) }
    
    putState :: Env -> StateErrorTick ()
    putState r = StateErrorTick { runStateErrorTick = \ _ -> Right ((), r, 0, []) }


instance MonadError StateErrorTick where
    
    throw :: String -> StateErrorTick a
    throw msg = StateErrorTick { runStateErrorTick = const (Left msg) }


instance MonadTick StateErrorTick where
    
    tick :: StateErrorTick ()
    tick = StateErrorTick { runStateErrorTick = \ s -> Right ((), s, 1, []) }


instance MonadOutput StateErrorTick where
    
    appendOutput :: String -> StateErrorTick ()
    appendOutput msg = StateErrorTick { runStateErrorTick = \ s -> Right ((), s, 0, [msg]) }