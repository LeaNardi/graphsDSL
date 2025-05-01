module ASTGraphs where


-- Alias
type Variable = String
type Value = Either Integer Graph
type Ticks = Integer
type Node = String
type Weight = Integer
type Edge = (Node, Node)
type WEdge = (Node, Node, Weight)
type Graph = [(Node, [(Node, Weight)])]
type Env = [(Variable, Value)]


-- Posibles expresiones de valores para el LetValue
data ValueExp = IntVal IntExp
              | GraphVal GraphExp
 deriving (Show, Eq)


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
 deriving (Show,Eq)


-- Expresiones Booleanas
data BoolExp = BTrue
             | BFalse
             | Eq IntExp IntExp
             | Lt IntExp IntExp
             | Gt IntExp IntExp
             | And BoolExp BoolExp
             | Or BoolExp BoolExp
             | Not BoolExp
 deriving (Show,Eq)


-- Expresiones Grafos
data GraphExp = ValuedGraph Graph
            | VarGraph Variable
            | AddNode GraphExp Node
            | AddDirectedEdge GraphExp Edge Weight
            | AddUndirectedEdge GraphExp Edge Weight
            | Kruskal GraphExp
 deriving (Show,Eq)
 

-- Comandos
data Comm = Skip
          | LetValue Variable ValueExp
          | Seq Comm Comm
          | Cond BoolExp Comm Comm
          | Repeat BoolExp Comm
 deriving (Show,Eq)