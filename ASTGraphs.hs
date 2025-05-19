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
             | esCiclico GraphExp
             | esConexo GraphExp
 deriving (Show,Eq)

<<<<<<< HEAD
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
=======

-- Expresiones Grafos No Dirigidos
data GraphExp = ValuedGraph Graph
            | VarGraph Variable
            | AddNode GraphExp Node
            | DeleteNode GraphExp Node
            | AddEdge GraphExp Edge Weight
            | DeleteEdge GraphExp Edge Weight
            | GraphComplement GraphExp
            | GraphUnion GraphExp GraphExp
            | GraphIntersection GraphExp GraphExp
            -- | Kruskal GraphExp
>>>>>>> main
 deriving (Show,Eq)


-- Expresiones Grafos Dirigidos
-- data DirGraphExp = ValuedDirGraph DirGraph


-- Comandos
data Comm = Skip
          | Seq Comm Comm
          | LetValue Variable ValueExp
          | Cond BoolExp Comm Comm
          | Repeat BoolExp Comm
 deriving (Show,Eq)