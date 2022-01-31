{-# LANGUAGE QuasiQuotes #-}

module Dictionaries
  ( dictionaries
  ) where

import Runner (parseFails, (@!))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Text.RawString.QQ (r)

dictionaries :: TestTree
dictionaries = testGroup "Dictionaries"
  [ statement
  , session
  , fromDoc ]

statement :: TestTree
statement = testCase "Statement" $ do
  [r|{"width": 120, "height": 80}.width|]    @! "120"
  [r|{"width": 120, "height": 80}("width")|] @! "120"
  [r|keys({"width": 120, "height": 80})|]    @! [r|["height", "width"]|]
  [r|values({"width": 120, "height": 80})|]  @! [r|[80, 120]|]
  [r|count("XXXOX")|]                        @! [r|{"O": 1, "X": 4}|]
  [r|count([# 58 58 58 4f 58 #])|]           @! "{79: 1, 88: 4}"
  [r|count([true, true, false, true]) |]     @! "{false: 1, true: 3}"
  [r|invert({"x": 1, "y" : 2, "z": 1})|]     @! [r|{1: ["z", "x"], 2: ["y"]}|]

session :: TestTree
session = testCase "Session" $ do
  [r|count("Hello World").o|] @! "2"
  [r|invert(count("big blue bag"))|] @! [r|{1: ["u", "l", "i", "e", "a"], 2: ["g", " "], 3: ["b"]}|]
  [r|fold(add, values(count("Hello, World!")))|] @! "13"

misc :: TestTree
misc = testCase "Misc" $ do
  [r|{ "add" : div }.add(1, 10)|] @! "0.1"

fromDoc :: TestTree
fromDoc = testGroup "From googledoc"
  [ testCase "Function dot call" $ "reverse.hello" @! "\"olleh\""
  , testCase "Dot chain" $ [r|if(true, { "width" : 1 }, 1+1).width|] @! "1"
  , testCase "Nested" $ "{1: 2, 3: {4: 5, 6: 7}}" @! "{1: 2, 3: {4: 5, 6: 7}}"
  , testCase "Dot assoc" $ do
    "{\"a\": 2, \"b\": {\"c\": 5, 6: 7}}.b.c" @! "5"
    "{\"a\": {\"hello\": {\"b\": 42}}}.a(\"hello\").b" @! "42"
  , testCase "No key" $ "{1 : 2}.a" @! "null" ]
