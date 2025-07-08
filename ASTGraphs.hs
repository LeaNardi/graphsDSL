module ASTGraphs where

-- Alias
type Variable = String
type Env = [(Variable, Value)]
type Ticks = Integer
type Weight = Integer

-- type Value = Either Integer GraphExp
-- Nueva estructura para type Value
data Value = IntValue Integer
           | GraphValue Graph
           | EdgeValue Edge
           | NodeValue Node
           | ListEdgeValue [Edge]
           | UnionFindValue UnionFind
           | QueueValue Queue
           | ListValue [Node]
 deriving (Show, Eq)

-- Core data types for runtime values
data Graph = Graph [(Node, [(Node, Weight)])] deriving (Show, Eq)
data Edge = Edge Node Node Weight deriving (Show, Eq)
data Node = Node String deriving (Show, Eq, Ord)
data UnionFind = UnionFind [(Node, Node)] deriving (Show, Eq)
data Queue = Queue [Node] deriving (Show, Eq)

-- Expresiones Aritmeticas
data IntExp = Const Integer
            | VarInt Variable
            | UMinus IntExp
            | Plus IntExp IntExp
            | Minus IntExp IntExp
            | Times IntExp IntExp
            | Div IntExp IntExp
            | Mod IntExp IntExp
            | Question BoolExp IntExp IntExp
            | GetWeight EdgeExp
            | Len ListEdgeExp
            | QueueLen QueueExp
 deriving (Show, Eq)

-- Expresiones Booleanas
data BoolExp = BTrue
             | BFalse
             | Eq IntExp IntExp
             | Lt IntExp IntExp
             | Gt IntExp IntExp
             | And BoolExp BoolExp
             | Or BoolExp BoolExp
             | Not BoolExp
             | EqNode NodeExp NodeExp
             | EsCiclico GraphExp
             | EsConexo GraphExp
             | InList ListExp NodeExp
             | IsEmptyQueue QueueExp
             | IsEmptyList ListExp
 deriving (Show, Eq)

-- Expresiones Grafos No Dirigidos
data GraphExp = ValuedGraph [(NodeExp, [(NodeExp, IntExp)])]
              | VarGraph Variable
              | AddNode GraphExp NodeExp
              | DeleteNode GraphExp NodeExp
              | AddEdge GraphExp EdgeExp
              | DeleteEdge GraphExp EdgeExp
              | GraphComplement GraphExp
              | GraphUnion GraphExp GraphExp
              | GraphIntersection GraphExp GraphExp
 deriving (Show, Eq)


-- Comandos
data Comm = Skip
          | Seq Comm Comm
          | LetValue Variable ValueExp
          | Cond BoolExp Comm Comm
          | While BoolExp Comm -- Ver el Repeat desarrollado previamente
          | For Variable ListExp Comm
          | Print ValueExp
 deriving (Show, Eq)

data ValueExp = IntVal IntExp
              | GraphVal GraphExp
              | EdgeVal EdgeExp
              | NodeVal NodeExp
              | ListEdgeVal ListEdgeExp
              | UnionFindVal UnionFindExp
              | QueueVal QueueExp
              | ListVal ListExp
 deriving (Show, Eq)

-- Operaciones de Lista de Edges
data ListEdgeExp = EmptyEdgeList
                 | VarEdgeList Variable
                 | GetEdges GraphExp
                 | SortByWeight ListEdgeExp
                 | TailEdges ListEdgeExp
 deriving (Show, Eq)

-- New list type for nodes
data ListExp = EmptyList
             | VarList Variable
             | NewList
             | AddList ListExp NodeExp
             | TailList ListExp
             | AdjacentNodes GraphExp NodeExp
 deriving (Show, Eq)

-- Enhanced queue operations
data QueueExp = EmptyQueue
              | VarQueue Variable
              | NewQueue
              | Enqueue QueueExp NodeExp
              | Dequeue QueueExp
 deriving (Show, Eq)

data EdgeExp = ValuedEdge (NodeExp, NodeExp, IntExp)
             | VarEdge Variable
             | HeadEdge ListEdgeExp
 deriving (Show, Eq)

data NodeExp = NodeLit String
             | VarNode Variable
             | GetNode1 EdgeExp
             | GetNode2 EdgeExp
             | Find NodeExp UnionFindExp
             | HeadList ListExp
             | DequeueNode QueueExp
 deriving (Show, Eq)

data UnionFindExp = ValuedUnionFind [(NodeExp, NodeExp)]
                  | VarUnionFind Variable
                  | Union NodeExp NodeExp UnionFindExp
 deriving (Show, Eq)