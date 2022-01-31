module Random
  ( random
  ) where

import Control.Monad (replicateM)
import Runner (RunResult (Success), runHiIO, (@!))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

random :: TestTree
random = testGroup "Random"
  [ session, uniform ]

session :: TestTree
session = testCase "Session" $ do
  "rand"        @! "rand"
  "rand(0, 10)" @! "rand(0, 10)"

uniform :: TestTree
uniform = testCase "Uniform" $ do
  nums <- replicateM 100 (runHiIO "rand(0, 5)!" [])
  mapM_ (\x -> assertBool "" $ Success (show x) `elem` nums) [0 .. 5]
