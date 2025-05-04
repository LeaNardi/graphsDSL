#!/bin/bash

# Compilar el archivo de tests
echo "⚙️ Compilando EvalTests.hs..."
ghc -package parsec -package tasty -package tasty-hunit Tests/EvalTests.hs -o EvalTests

# Verificar si compiló bien
if [ $? -eq 0 ]; then
  echo "✅ Compilación exitosa. Ejecutando tests..."
  ./EvalTests

  echo "🧹 Limpiando archivos *.hi y *.o..."
  find . -name "*.hi" -delete
  find . -name "*.o" -delete

  echo "🗑️  Borrando ejecutable EvalTests..."
  rm -f EvalTests
else
  echo "❌ Error en la compilación. No se ejecutan los tests ni se limpia."
fi
