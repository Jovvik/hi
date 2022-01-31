{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bytes
  ( bytes
  ) where

import Data.ByteString (ByteString)
import Generators ()
import HW3.Base (ConvertibleToHiExpr (toExpr), HiError (HiErrorInvalidArgument),
                 HiValue (HiValueBytes))
import Runner (evalFailsWith, evalSame, parseFails, showExpr, (@!))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import Text.RawString.QQ (r)

bytes :: TestTree
bytes = testGroup "Bytes"
  [ statement ]

statement :: TestTree
statement = testGroup "Statement"
  [ operations
  , overload
  , indexSlice
  , session
  , zipUnzip
  , desSer
  , fromDoc ]

operations :: TestTree
operations = testCase "Operations" $ do
  "pack-bytes([ 3, 255, 158, 32 ])"   @! "[#03 ff 9e 20#]"
  "unpack-bytes([#10 20 30#])"      @! "[16, 32, 48]"
  "encode-utf8(\"Hello!\")"           @! "[#48 65 6c 6c 6f 21#]"
  "decode-utf8([#48 65 6c 6c 6f#])" @! "\"Hello\""
  "decode-utf8([#c3 28#])"          @! "null"

overload :: TestTree
overload = testCase "Overload" $ do
  "[#00 ff#] + [#01 e3#]" @! "[#00 ff 01 e3#]"
  "[#00 ff#] * 3"           @! "[#00 ff 00 ff 00 ff#]"

indexSlice :: TestTree
indexSlice = testCase "Indexing and slicing" $ do
  "[#00 ff 01 e3#](1)" @! "255"
  "[#00 ff 01 e3#](1,3)" @! "[#ff 01#]"

session :: TestTree
session = testCase "Session" $ do
  [r|pack-bytes(range(30, 40))|]                     @! [r|[#1e 1f 20 21 22 23 24 25 26 27 28#]|]
  [r|zip(encode-utf8("Hello, World!" * 1000))|]      @! [r|[#78 da ed c7 31 0d 00 20 0c 00 30 2b f0 23 64 0e 30 00 df 92 25 f3 7f a0 82 af fd 1a 37 b3 d6 d8 d5 79 66 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 fc c9 03 ca 0f 3b 28#]|]
  [r|decode-utf8([#68 69#] * 5)|]                  @! [r|"hihihihihi"|]
  [r|unzip([#78 da 63 64 62 06 00 00 0d 00 07#])|] @! [r|[#01 02 03#]|]

zipUnzip :: TestTree
zipUnzip = testProperty "unzip(zip(A)) === A"
  (\(a :: ByteString) -> ("unzip(zip(" ++ s a ++ "))") `evalSame` s a)
    where
      s a = (showExpr . toExpr . HiValueBytes) a

desSer :: TestTree
desSer = testProperty "deserialise(serialise(A)) === A"
  (\a -> ("deserialise(serialise(" ++ showExpr a ++ "))") `evalSame` showExpr a)

fromDoc :: TestTree
fromDoc = testGroup "From googledoc"
  [ testCase "No expr in byte string literal" $ do
    parseFails "[# add(1,15) #]"
    parseFails "[# (1 + 2) #]"
  , testCase "Invalid byte" $ do
    "pack-bytes([256])" `evalFailsWith` HiErrorInvalidArgument
    "pack-bytes([-1])" `evalFailsWith` HiErrorInvalidArgument
  , testCase "Strictly two chars" $ do
    parseFails "[# 1 23 #]"
    parseFails "[# 0 1 2 3 #]"
  , testCase "Empty" $ "[##]" @! "[##]"
  , testCase "Spaces" $ do
    "[#     00      #]" @! "[#00#]"
    "[# 00    ff #]" @! "[#00 ff#]"
    "[ # 00 ff # ]" @! "[#00 ff#]" ]
