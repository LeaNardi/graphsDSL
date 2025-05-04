module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Parser.Parser (parseGraphs)
import ASTGraphs (Comm(..), ValueExp(..), IntExp(..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Parser DSL Tests"
  [ 
    testCase "skip" (parseGraphs "test" "skip" @?= Right Skip),
    testCase "secuencia de skips" (parseGraphs "test" "skip; skip" @?= Right (Seq Skip Skip)), 
    testCase "let simple" (parseGraphs "test" "x := 1" @?= Right (LetValue "x" (IntVal (Const 1))))
  ]

--  , testCase "condicional simple" $
--      parseGraphs "test" "cond true then skip else skip end"
--        @?= Right (Cond BTrue Skip Skip)
--
--  , testCase "repeat simple" $
--      parseGraphs "test" "repeat skip until false end"
--        @?= Right (Repeat BFalse Skip)
--  ]
