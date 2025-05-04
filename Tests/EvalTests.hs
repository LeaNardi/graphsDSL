module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Parser.Parser (parseGraphs)
import Eval.Eval (eval)
import ASTGraphs

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Eval DSL Tests"
  [ testCase "skip: no cambia entorno ni ticks" $
      evalFrom "skip" @?= ([], 0),

    testCase "let x := 1" $
      evalFrom "x := 1" @?= ([("x", Left 1)], 0),

    testCase "x := 1; x := x + 1" $
      evalFrom "x := 1; x := x + 1" @?= ([("x", Left 2)], 1),

    testCase "cond true then x := 1 else x := 2 end" $
      evalFrom "cond true then x := 1 else x := 2 end" @?= ([("x", Left 1)], 0),

    testCase "repeat x := x + 1 until x > 2 end" $
      evalFrom "x := 0; repeat x := x + 1 until x > 2 end" @?= ([("x", Left 3)], 2),

    testCase "grafo vacío" $
      evalFrom "g := newgraph []" @?= ([("g", Right [])], 0),

    testCase "grafo con nodo" $
      evalFrom "g := addnode newgraph [] \"a\"" @?= ([("g", Right [("a", [])])], 0)
  ]

-- Función auxiliar
evalFrom :: String -> (Env, Ticks)
evalFrom src = case parseGraphs "test" src of
  Left err -> error (show err)
  Right ast -> eval ast
