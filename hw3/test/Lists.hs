{-# LANGUAGE QuasiQuotes #-}

module Lists
  ( lists
  ) where

import HW3.Base (HiError (HiErrorArityMismatch))
import Runner (evalFailsWith, (@!))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Text.RawString.QQ (r)

lists :: TestTree
lists = testGroup "Lists"
  [ statement
  , fromDoc ]

statement :: TestTree
statement = testGroup "Statement"
  [ operations
  , overload
  , indexSlice
  , session ]

operations :: TestTree
operations = testCase "Operations" $ do
  "list(1, 2, 3)"           @! "[1, 2, 3]"
  "range(5, 10.3)"          @! "[5, 6, 7, 8, 9, 10]"
  "fold(add, [11, 22, 33])" @! "66"
  "fold(mul, [11, 22, 33])" @! "7986"
  "fold(div, [11, 22, 33])" @! "1/66"

overload :: TestTree
overload = testCase "Overload" $ do
  "length([1, true, \"Hello\"])"  @! "3"
  "reverse([1, true, \"Hello\"])" @! "[\"Hello\", true, 1]"
  "[1, 2] + [3, 4, 5]"            @! "[1, 2, 3, 4, 5]"
  "[0, \"x\"] * 3"                @! "[0, \"x\", 0, \"x\", 0, \"x\"]"

indexSlice :: TestTree
indexSlice = testCase "Indexing and slicing" $ do
  "[\"hello\", true, \"world\"](1)" @! "true"
  "[\"hello\", true, \"world\"](1, 3)" @! "[true, \"world\"]"

session :: TestTree
session = testCase "Session" $ do
  [r|list(1, 2, 3, 4, 5)|]                      @! [r|[1, 2, 3, 4, 5]|]
  [r|fold(add, [2, 5] * 3)|]                    @! [r|21|]
  [r|fold(mul, range(1, 10))|]                  @! [r|3628800|]
  [r|[0, true, false, "hello", "world"](2, 4)|] @! [r|[false, "hello"]|]
  [r|reverse(range(0.5, 70/8))|]                @! [r|[8.5, 7.5, 6.5, 5.5, 4.5, 3.5, 2.5, 1.5, 0.5]|]

fromDoc :: TestTree
fromDoc = testGroup "From googledoc"
  [ testCase "Non binary functions in fold" $ do
    "fold(if, [])"  @! "null"
    "fold(if, [1])" @! "1"
    "fold(if, [1, 2])" `evalFailsWith` HiErrorArityMismatch
    "fold(not, [])" @! "null"
    "fold(not, [1])" @! "1"
    "fold(not, [1, 2])" `evalFailsWith` HiErrorArityMismatch ]
