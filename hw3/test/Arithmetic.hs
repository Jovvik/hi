{-# LANGUAGE ScopedTypeVariables #-}

module Arithmetic
  ( arithmetic
  ) where

import HW3.Base (HiError (..))
import Runner (evalFailsWith, evalSame, makeOp, (@!))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import Test.Tasty.QuickCheck (testProperty)

arithmetic :: TestTree
arithmetic = testGroup "Arithmetic"
  [ statement
  , testCase "Mixed fractions" mixedFractions
  , qc
  , fromDoc ]

statement :: TestTree
statement = testGroup "Statement"
  [ testCase "Eval" statementEval
  , testCase "Errors" statementErrors
  , testCase "Pretty" statementPretty]

statementEval :: Assertion
statementEval = do
  "add(500, 12)"                      @! "512"
  "sub(10, 100)"                      @! "-90"
  "mul(23, 768)"                      @! "17664"
  "div(57, 190)"                      @! "0.3"
  "div(add(mul(2, 5), 1), sub(11,6))" @! "2.2"

statementErrors :: Assertion
statementErrors = do
  "sub(1)"            `evalFailsWith` HiErrorArityMismatch
  "sub(1, 2, 3)"      `evalFailsWith` HiErrorArityMismatch
  "div(1, 0)"         `evalFailsWith` HiErrorDivideByZero
  "div(1, sub(5, 5))" `evalFailsWith` HiErrorDivideByZero
  "15(2)"             `evalFailsWith` HiErrorInvalidFunction
  "sub(10, add)"      `evalFailsWith` HiErrorInvalidArgument

statementPretty :: Assertion
statementPretty = do
  "100"                     @! "100"
  "-15"                     @! "-15"
  "add(100, -15)"           @! "85"
  "add(3, div(14, 100))"    @! "3.14"
  "add(3, div(14, 100))"    @! "3.14"
  "div(10, 3)"              @! "3 + 1/3"
  "sub(mul(201, 11), 0.33)" @! "2210.67"

mixedFractions :: Assertion
mixedFractions = do
  "267 / 11" @! "24 + 3/11"
  "-71 / 7"  @! "-10 - 1/7"

makeQc :: Show a => TestName -> (Int -> Int -> a) -> TestTree
makeQc name op = testProperty name (\x y -> makeOp name [x, y] `evalSame` show (x `op` y))

qc :: TestTree
qc = testGroup "qc properties"
  [ makeQc "add" (+)
  , makeQc "sub" (-)
  , makeQc "mul" (*) ]

fromDoc :: TestTree
fromDoc = testCase "From googledoc" $ do
  "(div(1))(10)" `evalFailsWith` HiErrorArityMismatch
  "(div)(1,2)" @! "0.5"
