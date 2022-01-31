module Infix
  ( infx
  ) where

import Generators ()
import Runner (evalSame, makeOp, showExpr, (@!))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import Test.Tasty.QuickCheck (testProperty)

infx :: TestTree
infx = testGroup "Infix"
  [ testCase "Session" session
  , props ]

session :: Assertion
session = do
  "2 + 2"                     @! "4"
  "2 + 2 * 3"                 @! "8"
  "(2 + 2) * 3"               @! "12"
  "2 + 2 * 3 == (2 + 2) * 3"  @! "false"
  "10 == 2*5 && 143 == 11*13" @! "true"

makeInfixProperty :: TestName -> String -> TestTree
makeInfixProperty inf pref = testProperty inf (\x y ->
  (showExpr x ++ " " ++ " " ++ showExpr y) `evalSame` makeOp pref [x, y])

props :: TestTree
props = testGroup "Properties"
  [ makeInfixProperty "/" "div"
  , makeInfixProperty "*" "mul"
  , makeInfixProperty "+" "add"
  , makeInfixProperty "-" "sub"
  , makeInfixProperty "<" "less-than"
  , makeInfixProperty ">" "greater-than"
  , makeInfixProperty ">=" "not-less-than"
  , makeInfixProperty "<=" "not-greater-than"
  , makeInfixProperty "==" "equals"
  , makeInfixProperty "/=" "not-equals"
  , makeInfixProperty "&&" "and"
  , makeInfixProperty "||" "or" ]
