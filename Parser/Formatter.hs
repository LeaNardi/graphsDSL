module Parser.Formatter where

import ASTGraphs (Comm(..), Expr(..))

-- Solo agrega saltos de linea e indentacion
formatAST :: Comm -> String
formatAST = formatComm 0

formatComm :: Int -> Comm -> String
formatComm indent comm = case comm of
    Skip -> 
        indentStr indent ++ "Skip"
    
    Seq c1 c2 -> 
        indentStr indent ++ "Seq\n" ++
        indentStr indent ++ "  (" ++ formatComm 0 c1 ++ ")\n" ++
        indentStr indent ++ "  (" ++ formatComm 0 c2 ++ ")"
    
    AssignValue var expr -> 
        indentStr indent ++ "AssignValue " ++ show var ++ " (" ++ show expr ++ ")"
    
    Cond condition thenBranch elseBranch ->
        indentStr indent ++ "Cond\n" ++
        indentStr indent ++ "  (" ++ show condition ++ ")\n" ++
        indentStr indent ++ "  (" ++ formatComm 0 thenBranch ++ ")\n" ++
        indentStr indent ++ "  (" ++ formatComm 0 elseBranch ++ ")"
    
    While condition body ->
        indentStr indent ++ "While\n" ++
        indentStr indent ++ "  (" ++ show condition ++ ")\n" ++
        indentStr indent ++ "  (" ++ formatComm 0 body ++ ")"
    
    For var listExpr body ->
        indentStr indent ++ "For " ++ show var ++ "\n" ++
        indentStr indent ++ "  (" ++ show listExpr ++ ")\n" ++
        indentStr indent ++ "  (" ++ formatComm 0 body ++ ")"
    
    Visualize graphExpr fileExpr ->
        indentStr indent ++ "Visualize (" ++ show graphExpr ++ ") (" ++ show fileExpr ++ ")"
    
    Print expr ->
        indentStr indent ++ "Print (" ++ show expr ++ ")"

    ForNeighbors v g from upto body ->
        indentStr indent ++ "ForNeighbors " ++ show v ++ "\n" ++
        indentStr indent ++ "  (" ++ show g ++ ")\n" ++
        indentStr indent ++ "  (from: " ++ show from ++ ")\n" ++
        indentStr indent ++ "  (upto: " ++ show upto ++ ")\n" ++
        indentStr indent ++ "  (" ++ formatComm 0 body ++ ")"

    ForNodes v g body ->
        indentStr indent ++ "ForNodes " ++ show v ++ "\n" ++
        indentStr indent ++ "  (" ++ show g ++ ")\n" ++
        indentStr indent ++ "  (" ++ formatComm 0 body ++ ")"

    ForEdges e g body ->
        indentStr indent ++ "ForEdges " ++ show e ++ "\n" ++
        indentStr indent ++ "  (" ++ show g ++ ")\n" ++
        indentStr indent ++ "  (" ++ formatComm 0 body ++ ")"

    ForIncident e g from body ->
        indentStr indent ++ "ForIncident " ++ show e ++ "\n" ++
        indentStr indent ++ "  (" ++ show g ++ ")\n" ++
        indentStr indent ++ "  (" ++ show from ++ ")\n" ++
        indentStr indent ++ "  (" ++ formatComm 0 body ++ ")"

    ForComponent gVar g body ->
        indentStr indent ++ "ForComponent " ++ show gVar ++ "\n" ++
        indentStr indent ++ "  (" ++ show g ++ ")\n" ++
        indentStr indent ++ "  (" ++ formatComm 0 body ++ ")"

indentStr :: Int -> String
indentStr n = replicate n ' '