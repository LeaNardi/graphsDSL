#!/bin/bash

# Compilar el archivo de tests
echo "⚙️ Compilando ParserTests.hs..."
ghc -package parsec -package tasty -package tasty-hunit ParserTests.hs -o parser-tests

# Verificar si compiló bien
if [ $? -eq 0 ]; then
  echo "✅ Compilación exitosa. Ejecutando tests..."
  ./parser-tests

  echo "🧹 Limpiando archivos *.hi y *.o..."
  find . -name "*.hi" -delete
  find . -name "*.o" -delete
else
  echo "❌ Error en la compilación. No se ejecutan los tests ni se limpia."
fi
