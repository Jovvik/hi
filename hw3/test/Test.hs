{-# LANGUAGE OverloadedStrings #-}

module Test where
import Arithmetic (arithmetic)
import Boolean (boolean)
import Bytes (bytes)
import DateTime (dateTime)
import Dictionaries (dictionaries)
import HW3.Base
import IOTest (io)
import Idempotence (idempotence)
import Infix (infx)
import Lists (lists)
import Random (random)
import Runner
import ShortCircuit (shortCircuit)
import Strings (strings)
import System.Environment (setEnv)
import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests (QuickCheckTests),
                              QuickCheckVerbose (QuickCheckVerbose))

main :: IO ()
main = do
  putStrLn $ showExpr $ HiExprApply (HiExprValue (HiValueFunction HiFunParseTime)) [HiExprValue HiValueNull,HiExprValue (HiValueFunction HiFunToUpper)]
  defaultMain
    $ localOption (QuickCheckTests 1000)
    tests

tests :: TestTree
tests = testGroup "Hi tests"
  [dictionaries, shortCircuit, random, dateTime, io, bytes, lists, strings, infx, idempotence, boolean, arithmetic]
