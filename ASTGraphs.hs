module ASTGraphs where

-- Alias
type Variable = String
type Env = [(Variable, Value)] -- Podria moverse a Eval/StateErrorTick.hs
type Weight = Float
type Ticks = Integer -- Podria moverse a Eval/StateErrorTick.hs

-- Runtime Values -- No se usa en Parser, se usa en Eval
data Value = IntValue Integer
           | FloatValue Float
           | BoolValue Bool
           | StringValue String
           | NodeValue Node
           | EdgeValue Edge
           | GraphValue Graph
           | ListValue [Value]
           | QueueValue Queue
           | UnionFindValue UnionFind
 deriving (Show, Eq)

-- Tipos de datos especificos
data Graph = Graph [(Node, [(Node, Weight)])] deriving (Show, Eq)
type Node = String
data Edge = Edge Node Node Weight deriving (Show, Eq)
data Queue = Queue [Value] deriving (Show, Eq)
data UnionFind = UnionFind [(Node, Node)] deriving (Show, Eq)  -- (element, parent)

-- Expresion generica
data Expr = 
  -- Literales
    IntLit Integer
  | FloatLit Float
  | BoolLit Bool
  | StringLit String
  | NodeLit String
  | EmptyList
  | EmptyQueue
  
  -- Variables
  | Var Variable
  
  -- Operaciones Aritmeticas
  | UMinus Expr
  | BinOp BinOpType Expr Expr
  
  -- Operaciones Booleanas
  | Not Expr
  | Comparison CompOpType Expr Expr
  
  -- Expresiones Condicionales
  | Question Expr Expr Expr  -- condition ? then : else
  
  -- Llamadas a Funciones
  | FunCall FunctionType [Expr]
  
  -- Constructores de grafos
  | ValuedGraph [(Expr, [(Expr, Expr)])]  -- [(node, [(node, weight)])]
  | ValuedEdge Expr Expr Expr             -- node1 node2 weight
  
  -- Colecciones
  | ListConstruct [Expr]
  | QueueConstruct [Expr]
  | UnionFindConstruct [(Expr, Expr)]
  
 deriving (Show, Eq)

-- Operaciones aritmeticas binarias
data BinOpType = Plus | Minus | Times | Div | Mod
 deriving (Show, Eq)

-- Operaciones de comparacion
data CompOpType = Eq | Lt | Gt | And | Or | EqNode
 deriving (Show, Eq)

-- Funciones
data FunctionType = 
  -- Operaciones de Graph
    AddNode | DeleteNode | AddEdge | DeleteEdge
  | GraphComplement | GraphUnion | GraphIntersection
  | GetEdges | AdjacentNodes | EsCiclico | EsConexo 
  | MetricClosure
  
  -- Operaciones de Edge
  | GetWeight | GetNode1 | GetNode2
  
  -- Operaciones de List
  | Len | TailList | AddList | HeadList
  | SortByWeight | TailEdges | HeadEdge
  | InList | IsEmptyList
  
  -- Operaciones de Queue
  | QueueLen | Enqueue | Dequeue | DequeueNode
  | IsEmptyQueue
  
  -- Operaciones de UnionFind
  | Union | Find    
  
 deriving (Show, Eq)

-- Commandos
data Comm = Skip
          | Seq Comm Comm
          | AssignValue Variable Expr
          | Cond Expr Comm Comm
          | While Expr Comm
          | For Variable Expr Comm
          | Print Expr
 deriving (Show, Eq)