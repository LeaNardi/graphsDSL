# graphsDSL

ASTGraphs.hs: define el AST del DSL (expresiones, comandos, tipos base)

Utils.hs: funciones auxiliares, mayormente de manipulación de grafos (addEdge, Kruskal, etcétera)

Eval/MonadClasses.hs: define typeclasses MonadState, MonadError, MonadTick que abstraen efectos usados por el evaluador

Eval/StateErrorTick.hs: define la instancia monádica StateErrorTick y su ejecución (runStateErrorTick)

Eval/Core.hs: lógica de evaluación del AST (evalComm, evalIntExp, etcétera)

Eval/Eval.hs: la función eval

Parser/Core.hs: ...

Parser/Parser.hs: ...




ejemplo_kruskal_1.gph -> no es no dirigido, tiene que dar problemas