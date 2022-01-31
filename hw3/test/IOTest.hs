module IOTest
  ( io
  ) where

import Control.Exception (SomeException (SomeException), try)
import HW3.Action (HiPermission (..))
import Runner (runHiIOEq, (@!))
import System.Directory (doesDirectoryExist, doesFileExist, removeDirectoryRecursive)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assert, assertBool, testCase, (@?=))

io :: TestTree
io = testGroup "IO"
  [ session
  , evalAction]

assertIOBool :: String -> IO Bool -> IO ()
assertIOBool message check = do
  wasCreated <- check
  assertBool message wasCreated

session :: TestTree
session = testCase "Session" $ do
  try (removeDirectoryRecursive "tmp") :: IO (Either SomeException ())
  runHiIOEq "mkdir(\"tmp\")!" [AllowWrite] "null"
  assertIOBool "Tmp was not created" $ doesDirectoryExist "tmp"
  runHiIOEq "read(\"tmp\")!" [AllowRead] "[]"
  runHiIOEq "mkdir(\"tmp/a\")!" [AllowWrite] "null"
  assertIOBool "\"a\" was not created" $ doesDirectoryExist "tmp/a"
  runHiIOEq "mkdir(\"tmp/b\")!" [AllowWrite] "null"
  assertIOBool "\"b\" was not created" $ doesDirectoryExist "tmp/b"
  runHiIOEq "read(\"tmp\")!" [AllowRead] "[\"a\", \"b\"]"
  runHiIOEq "write(\"tmp/hi.txt\", \"Hello\")!" [AllowWrite] "null"
  assertIOBool "\"hi.txt\" was not created" $ doesFileExist "tmp/hi.txt"
  runHiIOEq "cd(\"tmp\")!" [AllowRead] "null"
  runHiIOEq "read(\"hi.txt\")!" [AllowRead] "\"Hello\""

evalAction :: TestTree
evalAction = testCase "Evaluation of actions" $ do
  "read(\"hi.txt\")"           @! "read(\"hi.txt\")"
  "write(\"hi.txt\", \"Hi!\")" @! "write(\"hi.txt\", [#48 69 21#])"
  "mkdir(\"dir\")"             @! "mkdir(\"dir\")"
  "cd(\"dir\")"                @! "cd(\"dir\")"
