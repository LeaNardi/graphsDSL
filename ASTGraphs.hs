module ASTGraphs where

-- Alias
type Variable = String
type Env = [(Variable, Value)]
type Weight = Integer
type Ticks = Integer

-- Runtime Values
data Value = IntValue Integer
           | GraphValue Graph
           | EdgeValue Edge
           | NodeValue Node
           | ListEdgeValue [Edge]
           | UnionFindValue UnionFind
           | QueueValue Queue
           | ListValue [Node]
 deriving (Show, Eq)

-- Core data types for runtime values (unchanged)
data Graph = Graph [(Node, [(Node, Weight)])] deriving (Show, Eq)
data Edge = Edge Node Node Weight deriving (Show, Eq)
data Node = Node String deriving (Show, Eq, Ord)
data UnionFind = UnionFind [(Node, Node)] deriving (Show, Eq)
data Queue = Queue [Node] deriving (Show, Eq)

-- UNTYPED AST - Single Expression type
data Expr = 
    -- Literals
    IntLit Integer
  | BoolLit Bool
  | NodeLit String
  | EmptyList
  | EmptyQueue
  | EmptyEdgeList
  
  -- Variables
  | Var Variable
  
  -- Arithmetic Operations
  | UMinus Expr
  | BinOp BinOpType Expr Expr
  
  -- Boolean Operations
  | Not Expr
  | Comparison CompOpType Expr Expr
  
  -- Conditional Expression
  | Question Expr Expr Expr  -- condition ? then : else
  
  -- Function/Method calls (covers all operations)
  | FunCall FunctionType [Expr]
  
  -- Complex constructors
  | ValuedGraph [(Expr, [(Expr, Expr)])]  -- [(node, [(node, weight)])]
  | ValuedEdge Expr Expr Expr             -- node1 node2 weight
  | ValuedUnionFind [(Expr, Expr)]        -- [(node, node)]
  
  -- Lists
  | ListConstruct [Expr]
  
 deriving (Show, Eq)

-- Binary arithmetic operations
data BinOpType = Plus | Minus | Times | Div | Mod
 deriving (Show, Eq)

-- Comparison operations
data CompOpType = Eq | Lt | Gt | And | Or | EqNode
 deriving (Show, Eq)

-- Function/Method types - represents all operations as function calls
data FunctionType = 
    -- Graph operations
    AddNode | DeleteNode | AddEdge | DeleteEdge
  | GraphComplement | GraphUnion | GraphIntersection
  | GetEdges | AdjacentNodes
  
  -- Edge operations  
  | GetWeight | GetNode1 | GetNode2
  
  -- List operations
  | Len | TailList | AddList | HeadList
  | SortByWeight | TailEdges | HeadEdge
  
  -- Queue operations
  | QueueLen | Enqueue | Dequeue | DequeueNode
  | IsEmptyQueue
  
  -- UnionFind operations
  | Union | Find
  
  -- Boolean predicates
  | EsCiclico | EsConexo | InList | IsEmptyList
  
 deriving (Show, Eq)

-- Commands (largely unchanged, but using untyped expressions)
data Comm = Skip
          | Seq Comm Comm
          | AssignValue Variable Expr  -- Now takes any expression
          | Cond Expr Comm Comm     -- Condition is just an expression
          | While Expr Comm         -- Condition is just an expression
          | For Variable Expr Comm  -- List expression is just an expression
          | Print Expr              -- Any expression can be printed
 deriving (Show, Eq)