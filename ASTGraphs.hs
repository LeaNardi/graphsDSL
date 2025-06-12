module ASTGraphs where


-- Alias
type Variable = String
type Env = [(Variable, Value)]
type Ticks = Integer
type Value = Either Integer Graph


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
             | EqNode NodeExp NodeExp
             | esCiclico GraphExp
             | esConexo GraphExp
 deriving (Show,Eq)


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
 deriving (Show,Eq)


-- Comandos
data Comm = Skip
          | Seq Comm Comm
          | LetValue Variable ValueExp
          | Cond BoolExp Comm Comm
          | Repeat BoolExp Comm
 deriving (Show,Eq)


data ValueExp = IntVal IntExp
              | GraphVal GraphExp
              | EdgeVal EdgeExp
              | NodeVal NodeExp
              | ListEdgeVal ListEdgeExp
              | UnionFindVal UnionFindExp
 deriving (Show,Eq)


data ListEdgeExp = EmptyList
                 | VarList Variable
                 | GetEdges GraphExp
                 | SortByWeight ListEdgeExp
                 | Tail ListEdgeExp
 deriving (Show,Eq)


data EdgeExp = ValuedEdge (NodeExp, NodeExp, IntExp)
             | VarEdge Variable
             | Head ListEdgeExp
 deriving (Show,Eq)


data NodeExp = Node String
             | VarNode Variable
             | GetNode1 EdgeExp
             | GetNode2 EdgeExp
             | Find NodeExp UnionFindExp
 deriving (Show,Eq)


data UnionFindExp = ValuedUnionFind [(NodeExp, NodeExp)]
                  | VarUnionFind Variable
                  | Union NodeExp NodeExp UnionFindExp
 deriving (Show,Eq)