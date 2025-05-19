### 📄 Archivos del Proyecto

* **`ASTGraphs.hs`**: define el AST del DSL (expresiones, comandos, tipos base como `Graph`, `Env`, etc.).

* **`Utils.hs`**: funciones auxiliares para manipulación de grafos (`addNode`, `addEdge`, `kruskal`, etc.). También verifica si un grafo es no dirigido con `isUndirected`.

* **`Eval/MonadClasses.hs`**: define las typeclasses `MonadState`, `MonadError`, `MonadTick` que encapsulan los efectos requeridos para la evaluación.

* **`Eval/StateErrorTick.hs`**: implementación concreta del evaluador monádico `StateErrorTick`, que combina estado (`Env`), errores y conteo de operaciones (`Ticks`). Provee `runStateErrorTick`.

* **`Eval/Core.hs`**: lógica de evaluación del AST (comandos, expresiones aritméticas, booleanas y de grafos). Utiliza las clases monádicas abstractas.

* **`Eval/Eval.hs`**: función `eval`, que ejecuta un `Comm` en el entorno inicial y devuelve el entorno resultante junto con el número de ticks usados.

* **`Parser/Lexer.hs`**: define el análisis léxico del lenguaje con `makeTokenParser` (palabras clave, operadores, comentarios).

* **`Parser/Core.hs`**: define los parsers para las distintas expresiones del DSL: enteras, booleanas, de grafos y comandos.

* **`Parser/Parser.hs`**: función `parseGraphs`, que toma un string fuente y retorna un AST (`Comm`) o un error de parseo.

* **`Tests/ParserTests.hs`**: tests unitarios del parser. Verifica que ciertas cadenas fuente del DSL se transformen correctamente en ASTs esperados.

* **`Tests/EvalTests.hs`**: tests unitarios del evaluador. Verifica resultados correctos del evaluador para distintos programas del DSL.

* **`Main.hs`**: programa principal que permite ejecutar archivos `.gph`. Parsea y evalúa el contenido mostrando el AST y el entorno final por consola.


### ✅ Pendientes

* [ ] Agregar tests más complejos al parser
* [ ] Corregir el código para que pasen los tests actuales del eval
* [ ] Agregar tests simples y complejos al eval (que sean equivalentes a los del `ParserTests`)
* [ ] Llevar los programas actuales a formatos de tests
  > 🟡 *Importante:* `ejemplo_kruskal_1.gph` y otros contienen grafos no dirigidos, por lo tanto **no debería poder ejecutarse `kruskal`** sobre ellos.
* [ ] Pensar programas complejos de aplicaciones prácticas para tener como ejemplos de uso real del DSL