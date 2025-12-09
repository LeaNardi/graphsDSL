@echo off

REM Procesamiento de varios archivos ejemplo
echo Procesando archivos...
echo.

echo "01_arithmetic_simple.gph"
runghc Main.hs Programas/01_arithmetic_simple.gph
echo.

echo "02_conditional_simple.gph"
runghc Main.hs Programas/02_conditional_simple.gph
echo.

echo "03_graph_simple.gph"
runghc Main.hs Programas/03_graph_simple.gph
echo.

echo "04_arithmetic_double.gph"
runghc Main.hs Programas/04_arithmetic_double.gph
echo.

echo "ejemplo_algoritmo_bfs.gph"
runghc Main.hs Programas/ejemplo_algoritmo_bfs.gph
echo.

echo "ejemplo_algoritmo_kruskal.gph"
runghc Main.hs Programas/ejemplo_algoritmo_kruskal.gph
echo.


echo Finalizado el procesamiento de archivos
