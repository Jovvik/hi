{-# LANGUAGE QuasiQuotes #-}

module Strings
  ( strings
  ) where

import HW3.Base (HiError (HiErrorInvalidArgument))
import Runner (evalFailsWith, (@!))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Text.RawString.QQ (r)

strings :: TestTree
strings = testGroup "Strings"
  [ statement
  , fromDoc ]

statement :: TestTree
statement = testGroup "Statement"
  [ operations
  , overloads
  , indexing
  , slice
  , sliceAdvanced
  , session ]

operations :: TestTree
operations = testCase "Operations" $ do
  "length(\"Hello World\")"   @! "11"
  "to-upper(\"Hello World\")" @! "\"HELLO WORLD\""
  "to-lower(\"Hello World\")" @! "\"hello world\""
  "reverse(\"stressed\")"     @! "\"desserts\""
  "trim(\" Hello World \")"   @! "\"Hello World\""

overloads :: TestTree
overloads = testCase "Overloads" $ do
  "\"Hello\" + \"World\""   @! "\"HelloWorld\""
  "\"Cat\" * 5"             @! "\"CatCatCatCatCat\""
  "\"/home/user\" / \"hi\"" @! "\"/home/user/hi\""

indexing :: TestTree
indexing = testCase "Indexing" $ do
  "\"Hello World\"(0)"  @! "\"H\""
  "\"Hello World\"(7)"  @! "\"o\""
  "\"Hello World\"(-1)" @! "null"
  "\"Hello World\"(99)" @! "null"

slice :: TestTree
slice = testCase "Slice" $ do
  "\"Hello World\"(0, 5)" @! "\"Hello\""
  "\"Hello World\"(2, 4)" @! "\"ll\""

sliceAdvanced :: TestTree
sliceAdvanced = testCase "Advanced slice" $ do
  "\"Hello World\"(0, -4)"    @! "\"Hello W\""
  "\"Hello World\"(-4, -1)"   @! "\"orl\""
  "\"Hello, World\"(2, null)" @! "\"llo, World\""
  "\"Hello, World\"(null, 5)" @! "\"Hello\""

session :: TestTree
session = testCase "Session" $ do
  [r|to-upper("what a nice language")(7, 11)|] @! [r|"NICE"|]
  [r|"Hello" == "World"|] @! [r|false|]
  [r|length("Hello" + "World")|] @! [r|10|]
  [r|length("hehe" * 5) / 3|] @! [r|6 + 2/3|]

fromDoc :: TestTree
fromDoc = testGroup "From googledoc"
  [ testCase "Float in slice" $ [r|"abc"(1, 2.2)|] `evalFailsWith` HiErrorInvalidArgument
  , testCase "Invalid multiplication" $ do
    [r|"cat" * 0|] `evalFailsWith` HiErrorInvalidArgument
    [r|"cat" * -1|] `evalFailsWith` HiErrorInvalidArgument
    [r|"cat" * 1.5|] `evalFailsWith` HiErrorInvalidArgument
  , testCase "Slice beyond end" $ [r|"suicide"(4,100)|] @! "\"ide\""
  , testCase "Slices" $ do
    [r|"abc"(0, 5)|] @! [r|"abc"|]
    [r|"abc"(3, 1)|] @! [r|""|]
    [r|"abc"(3, -3)|] @! [r|""|] ]
