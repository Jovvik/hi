{-# LANGUAGE ScopedTypeVariables #-}

module Boolean
  ( boolean
  ) where

import Generators ()
import HW3.Base (ConvertibleToHiExpr (toExpr), HiValue (HiValueBool))
import Runner (makeOpExpr, maybeDiffErrors, parseFails, showExpr, (@!))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import Test.Tasty.QuickCheck (testProperty)

boolean :: TestTree
boolean = testGroup "Boolean"
  [ statement
  , complements
  , branching
  , fromDoc ]

statement :: TestTree
statement = testGroup "Statement"
  [ testCase "Boolean algebra" booleanAlgebra
  , testCase "Equality checking" equalityChecking
  , testCase "Comparisons" comparisons
  , testCase "Session" session ]

session :: Assertion
session = do
  "false"                                         @! "false"
  "equals(add(2, 2), 4)"                          @! "true"
  "less-than(mul(999, 99), 10000)"                @! "false"
  "if(greater-than(div(2, 5), div(3, 7)), 1, -1)" @! "-1"
  "and(less-than(0, 1), less-than(1, 0))"         @! "false"
  "if(true, add, mul)"                            @! "add"
  "if(true, add, mul)(10, 10)"                    @! "20"
  "if(false, add, mul)(10, 10)"                   @! "100"
  "equals(add, add)"                              @! "true"
  "equals(add, mul)"                              @! "false"

booleanAlgebra :: Assertion
booleanAlgebra = do
  "not(true)"        @! "false"
  "and(true, false)" @! "false"
  "or(true, false)"  @! "true"

equalityChecking :: Assertion
equalityChecking = do
  "equals(10, 10)"       @! "true"
  "equals(false, false)" @! "true"
  "equals(3, 10)"        @! "false"
  "equals(1, true)"      @! "false"

comparisons :: Assertion
comparisons = do
  "less-than(3, 10)"       @! "true"
  "less-than(false, true)" @! "true"
  "less-than(false, 0)"    @! "true"

complements :: TestTree
complements = testGroup "complements"
  [ testProperty "greater-less" (\x y
    -> makeOpExpr "greater-than" [x, y] `maybeDiffErrors` makeOpExpr "less-than" [y, x])
  , testProperty "not-equals" (\x y
    -> makeOpExpr "not-equals" [x, y] `maybeDiffErrors` ("not(" ++ makeOpExpr "equals" [x, y] ++ ")"))
  , testProperty "not-less-than" (\x y
    -> makeOpExpr "not-less-than" [x, y] `maybeDiffErrors` ("not(" ++ makeOpExpr "less-than" [x, y] ++ ")"))
  , testProperty "not-greater-than" (\x y
    -> makeOpExpr "not-greater-than" [x, y] `maybeDiffErrors` ("not(" ++ makeOpExpr "greater-than" [x, y] ++ ")"))
    ]

branching :: TestTree
branching = testGroup "branching"
  [ testProperty "if false" (\x y
    -> makeOpExpr "if" [toExpr $ HiValueBool False, x, y] `maybeDiffErrors` showExpr y)
  , testProperty "if true" (\x y
    -> makeOpExpr "if" [toExpr $ HiValueBool True, x, y] `maybeDiffErrors` showExpr x)]

fromDoc :: TestTree
fromDoc = testGroup "From googledoc"
  [ testCase "Two-way comparison" $ parseFails "1 < 2 < 3" ]
