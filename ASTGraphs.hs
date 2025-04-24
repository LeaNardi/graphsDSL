module ASTGraphs where

-- Identificadores de Variable
type Variable = String

-- Expresiones Aritméticas
data IntExp = Const Integer
            | Var Variable
            | UMinus IntExp
            | Plus IntExp IntExp
            | Minus IntExp IntExp
            | Times IntExp IntExp
            | Div IntExp IntExp
            | Mod IntExp IntExp
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
             deriving (Show, Eq)

-- Grafos
type Node = String
type Weight = Double
newtype Graph = Graph [(Node, [(Node, Weight)])] deriving (Show, Eq)

-- Operaciones sobre Grafos
data GraphOp = Kruskal Variable
             | EmptyGraph Variable
             | AddEdge Variable IntExp IntExp IntExp
             deriving (Show, Eq)

-- Comandos (sentencias)
data Comm = Skip
          | Let Variable IntExp
          | LetGraph Variable Graph
          | Seq Comm Comm
          | Cond BoolExp Comm Comm
          | Repeat Comm BoolExp
          | GraphOperation GraphOp
          | Set Variable IntExp
          deriving (Show, Eq)
