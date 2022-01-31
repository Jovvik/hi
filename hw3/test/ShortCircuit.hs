{-# LANGUAGE QuasiQuotes #-}

module ShortCircuit
  ( shortCircuit
  ) where

import Data.Proxy (Proxy (Proxy))
import HW3.Action (HiPermission (AllowWrite))
import Runner (runHiIOEq, (@!))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import Text.RawString.QQ (r)

shortCircuit :: TestTree
shortCircuit = testGroup "Short circuit"
  [ session
  , lazy ]

session :: TestTree
session = testCase "Session" $ do
  "echo"            @! "echo"
  "echo(\"Hello\")" @! "echo(\"Hello\")"
  runHiIOEq "echo(\"Hello\")!" [AllowWrite] "null"
  [r|"Hello"(0) || "Z"|]  @! "\"H\""
  [r|"Hello"(99) || "Z"|] @! "\"Z\""

lazy :: TestTree
lazy = testCase "Laziness" $ do
  "if(true, null, 1 / 0)"  @! "null"
  "if(false, 1 / 0, null)" @! "null"
  "true || 1 / 0"          @! "true"
  "false && 1 / 0"         @! "false"
  "null && 1 / 0"          @! "null"
  "1 && 2"                 @! "2"
