module Eval.Core ( evalComm ) where

import ASTGraphs ( Comm(..), BoolExp(..), IntExp(..), GraphExp(..), ValueExp(..), Value, Ticks, Env, Graph, Edge )
import Eval.MonadClasses ( MonadError(..), MonadState(lookfor, update), MonadTick(..) )
import Utils ( addNode, addEdge, kruskal, isUndirected )
import Control.Monad ( when )
import Data.List (intersect)


evalComm :: (MonadState m, MonadError m, MonadTick m) => Comm -> m ()
evalComm Skip = return ()
evalComm (LetValue v valExp) = do val <- evalValueExp valExp
                                  update v val
evalComm (Seq c1 c2) = do evalComm c1
                          evalComm c2
evalComm (Cond b c1 c2) = do cond <- evalBoolExp b
                             if cond then evalComm c1 else evalComm c2
evalComm (Repeat b c) = do cond <- evalBoolExp b
                           when cond (evalComm (Seq c (Repeat b c)))


evalBoolExp :: (MonadState m, MonadError m, MonadTick m) => BoolExp -> m Bool
evalBoolExp BTrue     = return True
evalBoolExp BFalse    = return False
evalBoolExp (Eq l r)  = do lval <- evalIntExp l
                           rval <- evalIntExp r
                           return (lval == rval)
evalBoolExp (Lt l r)  = do lval <- evalIntExp l
                           rval <- evalIntExp r
                           return (lval < rval)
evalBoolExp (Gt l r)  = do lval <- evalIntExp l
                           rval <- evalIntExp r
                           return (lval > rval)
evalBoolExp (And l r) = do lval <- evalBoolExp l
                           rval <- evalBoolExp r
                           return (lval && rval)
evalBoolExp (Or l r)  = do lval <- evalBoolExp l
                           rval <- evalBoolExp r
                           return (lval || rval)
evalBoolExp (Not b)   = do bval <- evalBoolExp b
                           return (not bval)


evalValueExp :: (MonadState m, MonadError m, MonadTick m) => ValueExp -> m Value
evalValueExp (IntVal i) = Left <$> evalIntExp i
evalValueExp (GraphVal g) = Right <$> evalGraphExp g


evalIntExp :: (MonadState m, MonadError m, MonadTick m) => IntExp -> m Integer
evalIntExp (Const n)   = return n
evalIntExp (VarInt v)  = do val <- lookfor v
                            case val of
                              Left i  -> return i
                              Right _ -> throw
evalIntExp (UMinus e)  = do val <- evalIntExp e
                            return (negate val)
evalIntExp (Plus l r)  = do lval <- evalIntExp l
                            rval <- evalIntExp r
                            tick
                            return (lval + rval)
evalIntExp (Minus l r) = do lval <- evalIntExp l
                            rval <- evalIntExp r
                            tick
                            return (lval - rval)
evalIntExp (Times l r) = do lval <- evalIntExp l
                            rval <- evalIntExp r
                            tick
                            return (lval * rval)
evalIntExp (Div l r)   = do lval <- evalIntExp l
                            rval <- evalIntExp r
                            if rval == 0 then throw
                            else do tick
                                    return (div lval rval)
evalIntExp (Mod l r)   = do lval <- evalIntExp l
                            rval <- evalIntExp r
                            if rval == 0 then throw
                            else do tick
                                    return (mod lval rval)
evalIntExp (Question b l r) = do cond <- evalBoolExp b
                                 lval <- evalIntExp l
                                 rval <- evalIntExp r
                                 if cond then return lval else return rval


evalGraphExp :: (MonadState m, MonadError m, MonadTick m) => GraphExp -> m Graph
evalGraphExp (ValuedGraph g) = return g
evalGraphExp (VarGraph v) = do val <- lookfor v
                               case val of
                                 Right g -> return g
                                 Left _  -> throw
evalGraphExp (AddNode g n) = do gval <- evalGraphExp g
                                return (addNode n gval)
-- evalGraphExp (AddDirectedEdge g (u, v) w) = do gval <- evalGraphExp g
--                                                let nodes = map fst gval
--                                                if u `elem` nodes && v `elem` nodes
--                                                  then return (addDirectedEdge u v w gval)
--                                                  else throw
evalGraphExp (AddEdge g (u, v) w) = do  gval <- evalGraphExp g
                                        let nodes = map fst gval
                                        if u `elem` nodes && v `elem` nodes
                                            then return (addEdge u v w gval)
                                            else throw
evalGraphExp (GraphIntersection g1 g2) = do
  
  gval1 <- evalGraphExp g1
  gval2 <- evalGraphExp g2

  let nodes1 = map fst gval1
      nodes2 = map fst gval2
      nodes = nodes1 `intersect` nodes2

      getEdges g = [ (u, v, w) | (u, vw) <- g, (v, w) <- vw ]
      edges1 = getEdges gval1
      edges2 = getEdges gval2

      edges = [ (u, v, w) 
              | (u, v, w) <- edges1
              , (u', v', w') <- edges2
              , w == w'
              , (u == u' && v == v') || (u == v' && v == u')
              ]

      finalGraphWithOnlyNodes = foldr addNode [] nodes
      finalGraph = foldr (\(u, v, w) g -> addEdge u v w g) finalGraphWithOnlyNodes edges

  return finalGraph

-- evalGraphExp (Kruskal g) = do gval <- evalGraphExp g
--                               if isUndirected gval
--                                 then return (kruskal gval)
--                                 else throw

evalGraphExp (GraphComplement g ) = do
  
  gval <- evalGraphExp g

  let nodes = map fst gval

  return finalGraph