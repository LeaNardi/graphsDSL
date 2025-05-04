module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Parser.Parser (parseGraphs)
import ASTGraphs

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Parser DSL Tests"
  [ 
    testCase "skip" (parseGraphs "test" "skip" @?= Right Skip),
    testCase "secuencia de skips" (parseGraphs "test" "skip; skip" @?= Right (Seq Skip Skip)), 
    testCase "let intval" (parseGraphs "test" "x := 1" @?= Right (LetValue "x" (IntVal (Const 1)))), 
    testCase "condicional simple" (parseGraphs "test" "cond true then skip else skip end" @?= Right (Cond BTrue Skip Skip)), 
    testCase "repeat simple" (parseGraphs "test" "repeat skip until false end" @?= Right (Repeat BFalse Skip)),
    testCase "let graphval vacío" (parseGraphs "test" "x := newgraph []" @?= Right (LetValue "x" (GraphVal (ValuedGraph [])))),
    testCase "let graphval no vacío" (parseGraphs "test" "x := newgraph [(\"a\", [(\"b\", 1)]), (\"b\", [])]" @?= Right (LetValue "x" (GraphVal (ValuedGraph [("a", [("b", 1)]), ("b", [])])))),  

    testCase "cond true then x := 1 else x := 2 end" $
      parseGraphs "test" "cond true then x := 1 else x := 2 end" @?=
      Right (Cond BTrue
                (LetValue "x" (IntVal (Const 1)))
                (LetValue "x" (IntVal (Const 2))))
  ]