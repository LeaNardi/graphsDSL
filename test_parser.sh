#!/bin/bash

# Compilar el archivo de tests
echo "⚙️ Compilando ParserTests.hs..."
ghc -package parsec -package tasty -package tasty-hunit Tests/ParserTests.hs -o ParserTests

# Verificar si compiló bien
if [ $? -eq 0 ]; then
  echo "✅ Compilación exitosa. Ejecutando tests..."
  ./ParserTests

  echo "🧹 Limpiando archivos *.hi y *.o..."
  find . -name "*.hi" -delete
  find . -name "*.o" -delete

  echo "🗑️  Borrando ejecutable ParserTests..."
  rm -f ParserTests
else
  echo "❌ Error en la compilación. No se ejecutan los tests ni se limpia."
fi
