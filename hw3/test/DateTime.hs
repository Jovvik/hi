{-# LANGUAGE QuasiQuotes #-}

module DateTime
  ( dateTime
  ) where

import HW3.Action (HiPermission (AllowTime))
import Runner (RunResult (Success), runHiIO, (@!))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)
import Text.RawString.QQ (r)

dateTime :: TestTree
dateTime = testGroup "Date and time" $ do
  [ session, timeDiff, operators ]

session :: TestTree
session = testCase "Session" $ do
  "parse-time(\"2021-01-01 00:00:00 UTC\") + 365 * 24 * 60 * 60" @! "parse-time(\"2022-01-01 00:00:00 UTC\")"

timeDiff :: TestTree
timeDiff = testCase "now! - now!" $ do
  res <- runHiIO "now! - now!" [AllowTime]
  case res of
    Success s -> assertBool ("now! - now! is not negative: " ++ s) ((read s :: Double) < 0)
    _         -> assertFailure "now! - now! failed"

operators :: TestTree
operators = testCase "Operators" $ do
  [r|parse-time("2021-12-15 00:00:00 UTC") + 1000|] @! [r|parse-time("2021-12-15 00:16:40 UTC")|]
  [r|parse-time("2021-12-15 00:37:51.000890793 UTC") - parse-time("2021-12-15 00:37:47.649047038 UTC")|] @! [r|3.351843755|]
