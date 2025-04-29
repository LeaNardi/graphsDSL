module ASTGraphs where

-- Alias
type Variable = String
type Node = String
type Weight = IntExp

-- Expresiones Aritmeticas
data IntExp = Const Integer
            | Var Variable
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
data GraphExp = EmptyGraph
            | NewGraph Variable
            | AddNode GraphExp Node
            | AddEdge GraphExp Node Node Weight
            | Kruskal GraphExp
 deriving (Show,Eq)

-- Comandos
data Comm = Skip
          | LetIntExp Variable IntExp
          | LetBoolExp Variable BoolExp
          | LetGraphExp Variable GraphExp
          | SetBoolExp Variable BoolExp
          | SetIntExp Variable IntExp
          | SetGraphExp Variable GraphExp
          | Seq Comm Comm
          | Cond BoolExp Comm Comm
          | Repeat BoolExp Comm
 deriving (Show,Eq)