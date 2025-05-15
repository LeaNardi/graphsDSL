module ASTGraphs where

-- Identificadores de Variable
type Variable = String

-- Expresiones Aritmeticas
data IntExp = Const Integer
            | Var Variable
            | UMinus IntExp
            | Plus IntExp IntExp
            | Minus IntExp IntExp
            | Times IntExp IntExp
            | Div IntExp IntExp
            | Question BoolExp IntExp IntExp -- a > 0 ? 1 : 2;
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
             | esCiclico GraphExp
             | esConexo GraphExp
 deriving (Show,Eq)

  -- Grafos viejo: este "no es AST" porque no representa operaciones sobre los grafos sino un resultado final y no nos deja motrar transformaciones o contrucciones un poco mas complejas en nuestro DSL
-- newtype Graph = Graph [(Node, [(Node, Weight)])] deriving (Show,Eq)

type Node = Integer
type Weight = Double
data GraphExp = EmptyGraph
              | SingleNode Node
              | AddNode Node GraphExp
              | DeleteNode xxx GraphExp
              | AddEdge Node Node Weight GraphExp
              | DeleteEdge Node Node Weight GraphExp
              | Union GraphExp GraphExp
              | Interseccion GraphExp GraphExp
              | Complemento GraphExp
              | ContarAristas Node GraphExp 
              | Kruskal GraphExp
              | VarGraph Variable  -- Para grafos que existen

-- Para tratar valores de Int o Graphs
-- data Value = VInt IntExp
--            | VGraph Graph
--            deriving (Show, Eq)

-- Comandos (sentencias)
-- Observar que solo se permiten variables de un tipo (entero)
data Comm = Skip
          | Let Variable IntExp
          | LetGraph Variable GraphExp -- Ver de hacer conjuntamente con IntExp
          | Seq Comm Comm
          | Cond BoolExp Comm Comm
          | Repeat Comm BoolExp
 deriving (Show,Eq)
