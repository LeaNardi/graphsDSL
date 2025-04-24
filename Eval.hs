module Eval (eval) where

import ASTGraphs
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Data.List (sortBy)
import Data.Function (on)
import Debug.Trace

-- Estado
type Env = [(Variable, Either Integer Graph)]

-- Estado inicial vacío
initState :: Env
initState = []

-- Mónada Estado-Error-Tick
newtype StateErrorTick a = StateErrorTick { runStateErrorTick :: Env -> Maybe (a, Env, Integer) }

instance Monad StateErrorTick where
    return x = StateErrorTick (\s -> Just (x, s, 0))
    m >>= f  = StateErrorTick (\s -> do
        (v, s', t)    <- runStateErrorTick m s
        (v', s'', t') <- runStateErrorTick (f v) s'
        return (v', s'', t + t'))

instance Functor StateErrorTick where
    fmap = liftM

instance Applicative StateErrorTick where
    pure = return
    (<*>) = ap

-- Clases monádicas

class Monad m => MonadState m where
    lookfor :: Variable -> m (Either Integer Graph)
    update :: Variable -> Either Integer Graph -> m ()
    saveState :: m Env
    putState :: Env -> m ()

instance MonadState StateErrorTick where
    lookfor var = StateErrorTick (\s -> maybe Nothing (\v -> Just (v, s, 0)) (lookup var s))
    update var val = StateErrorTick (\s -> Just ((), (var, val) : filter ((/= var) . fst) s, 0))
    --update var val = StateErrorTick (\s -> 
    --    let newEnv = (var, val) : filter ((/= var) . fst) s
    --    in seq (trace ("[update] " ++ var ++ " = " ++ show val) ()) 
    --       (Just ((), newEnv, 0)))
    saveState = StateErrorTick (\s -> Just (s, s, 0))
    putState r = StateErrorTick (\_ -> Just ((), r, 0))

class Monad m => MonadError m where
    throw :: m a

instance MonadError StateErrorTick where
    throw = StateErrorTick (\_ -> Nothing)

class Monad m => MonadTick m where
    tick :: m ()

instance MonadTick StateErrorTick where
    tick = StateErrorTick (\s -> Just ((), s, 1))

-- Helpers para grafos

lookforGraph :: (MonadState m, MonadError m) => Variable -> m Graph
lookforGraph v = do
    val <- lookfor v
    case val of
        Right g -> return g
        _       -> throw

updateGraph :: (MonadState m) => Variable -> Graph -> m ()
updateGraph v g = update v (Right g)

-- Evaluador

eval :: Comm -> (Env, Integer)
eval p = case runStateErrorTick (evalComm p) initState of
    Just (_, s, t) -> (s, t)
    Nothing        -> error "ERROR!"

evalComm :: (MonadState m, MonadError m, MonadTick m) => Comm -> m ()
evalComm Skip           = return ()
evalComm (Let v e)      = do val <- evalIntExp e
                             update v (Left val)
evalComm (Set v e) = do
    val <- evalIntExp e
    update v (Left val)
evalComm (LetGraph v g) = update v (Right g)
evalComm (Seq l r)      = evalComm l >> evalComm r
evalComm (Repeat c b)   = do
    evalComm c
    cond <- evalBoolExp b
    if cond then return () else evalComm (Repeat c b)
evalComm (Cond b c1 c2) = do
    cond <- evalBoolExp b
    if cond then evalComm c1 else evalComm c2
evalComm (GraphOperation op) = case op of
    Kruskal v -> do
        g <- lookforGraph v
        mst <- kruskal g
        updateGraph v mst
    EmptyGraph v -> updateGraph v (Graph [])
    AddEdge g n1 n2 wexp -> do
        Graph g' <- lookforGraph g
        w  <- fromInteger <$> evalIntExp wexp
        n1s <- show <$> evalIntExp n1
        n2s <- show <$> evalIntExp n2
        let g'' = addUndirectedEdge g' n1s n2s w
        updateGraph g (Graph g'')

evalBoolExp :: (MonadState m, MonadError m, MonadTick m) => BoolExp -> m Bool
evalBoolExp BTrue       = return True
evalBoolExp BFalse      = return False
evalBoolExp (Eq a b)    = (==) <$> evalIntExp a <*> evalIntExp b
evalBoolExp (Lt a b)    = (<)  <$> evalIntExp a <*> evalIntExp b
evalBoolExp (Gt a b)    = (>)  <$> evalIntExp a <*> evalIntExp b
--evalBoolExp (Gt a b) = do
--    av <- evalIntExp a
--    bv <- evalIntExp b
--    let res = av > bv
--    trace ("[evalBoolExp] " ++ show av ++ " > " ++ show bv ++ " = " ++ show res) (return res)
evalBoolExp (And x y)   = (&&) <$> evalBoolExp x <*> evalBoolExp y
evalBoolExp (Or x y)    = (||) <$> evalBoolExp x <*> evalBoolExp y
evalBoolExp (Not x)     = not  <$> evalBoolExp x

-- Expresiones aritméticas

evalIntExp :: (MonadState m, MonadError m, MonadTick m) => IntExp -> m Integer
evalIntExp (Const n)   = return n
evalIntExp (Var v)     = do val <- lookfor v
                            case val of
                                Left i  -> return i
                                Right _ -> throw
evalIntExp (UMinus e)  = negate <$> evalIntExp e
evalIntExp (Plus l r)  = binOp (+) l r
evalIntExp (Minus l r) = binOp (-) l r
evalIntExp (Times l r) = binOp (*) l r
evalIntExp (Div l r)   = do
    lv <- evalIntExp l
    rv <- evalIntExp r
    if rv == 0 then throw else tick >> return (lv `div` rv)
evalIntExp (Mod l r)   = do
    lv <- evalIntExp l
    rv <- evalIntExp r
    if rv == 0 then throw else tick >> return (lv `mod` rv)

binOp :: (MonadState m, MonadError m, MonadTick m) => (Integer -> Integer -> Integer) -> IntExp -> IntExp -> m Integer
binOp op l r = do
    lv <- evalIntExp l
    rv <- evalIntExp r
    tick
    return (op lv rv)

type Subset = (Node, Node)
type Edge = (Node, Node, Weight)


kruskal :: (MonadState m, MonadError m, MonadTick m) => Graph -> m Graph
kruskal (Graph edges) = do
    tick
    return $ edgesToGraph (kruskalHelper (sortEdges edges) [] [])

sortEdges :: [(Node, [(Node, Weight)])] -> [Edge]
sortEdges graph = sortBy (compare `on` (\(_,_,w) -> w)) $
                  concatMap (\(u, vs) -> [(u, v, w) | (v, w) <- vs]) graph

kruskalHelper :: [Edge] -> [Subset] -> [Edge] -> [Edge]
kruskalHelper [] _ mst = mst
kruskalHelper ((u,v,w):es) subsets mst
  | not (formsCycle u v subsets) = kruskalHelper es (union u v subsets) ((u,v,w):mst)
  | otherwise                    = kruskalHelper es subsets mst

find :: Node -> [Subset] -> Node
find node subsets = case lookup node subsets of
                      Just parent -> find parent subsets
                      Nothing     -> node

formsCycle :: Node -> Node -> [Subset] -> Bool
formsCycle u v subsets = find u subsets == find v subsets

union :: Node -> Node -> [Subset] -> [Subset]
union u v subsets = (find u subsets, find v subsets) : subsets

edgesToGraph :: [Edge] -> Graph
edgesToGraph es = Graph $ mergeAdjacents $ foldr insertEdge [] es
  where
    insertEdge (u, v, w) acc =
        add u (v, w) $ add v (u, w) acc

    add n vw [] = [(n, [vw])]
    add n vw ((x, vs):rest)
        | n == x    = (x, vw : vs) : rest
        | otherwise = (x, vs) : add n vw rest

    mergeAdjacents = map (\(n, ws) -> (n, reverse ws))  -- opcional: mantener orden

addUndirectedEdge :: [(Node, [(Node, Weight)])] -> Node -> Node -> Weight -> [(Node, [(Node, Weight)])]
addUndirectedEdge g u v w = add u (v, w) $ add v (u, w) g
  where
    add :: Node -> (Node, Weight) -> [(Node, [(Node, Weight)])] -> [(Node, [(Node, Weight)])]
    add n vw [] = [(n, [vw])]
    add n vw ((x, vs):rest)
        | n == x    = (x, vw : vs) : rest
        | otherwise = (x, vs) : add n vw rest
