# graphsDSL

ASTGraphs.hs: define el AST del DSL (expresiones, comandos, tipos base)

MonadClasses.hs: define typeclasses MonadState, MonadError, MonadTick que abstraen efectos usados por el evaluador

StateErrorTick.hs: define la instancia monádica StateErrorTick y su ejecución (runStateErrorTick)

EvalCore.hs: lógica de evaluación del AST (evalComm, evalIntExp, etcétera)

Eval.hs: la función eval

Utils.hs: funciones auxiliares, mayormente de manipulación de grafos (addEdge, Kruskal, etcétera)