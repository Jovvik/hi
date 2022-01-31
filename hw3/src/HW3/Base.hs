{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module HW3.Base
  ( ConvertibleToHiExpr (toExpr)
  , ConvertibleToHiValue (toValue)
  , HiAction (..)
  , HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiMonad (..)
  , HiValue (..)
  , returnValue
  , showFun
  ) where

import Codec.Serialise.Class (Serialise)
import Data.ByteString (ByteString)
import Data.Data (Typeable)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ratio ((%))
import Data.Sequence (Seq, fromList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

-- | Actions
data HiAction =
    HiActionRead FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass Serialise

-- | Function names
data HiFun
  = HiFunAdd
  | HiFunSub
  | HiFunAnd
  | HiFunDiv
  | HiFunEquals
  | HiFunGreaterThan
  | HiFunIf
  | HiFunLessThan
  | HiFunMul
  | HiFunNotEquals
  | HiFunNotGreaterThan
  | HiFunNotLessThan
  | HiFunNot
  | HiFunOr
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass Serialise

showFun :: HiFun -> String
showFun HiFunAdd            = "add"
showFun HiFunAnd            = "and"
showFun HiFunDiv            = "div"
showFun HiFunEquals         = "equals"
showFun HiFunGreaterThan    = "greater-than"
showFun HiFunIf             = "if"
showFun HiFunLessThan       = "less-than"
showFun HiFunMul            = "mul"
showFun HiFunNot            = "not"
showFun HiFunNotEquals      = "not-equals"
showFun HiFunNotGreaterThan = "not-greater-than"
showFun HiFunNotLessThan    = "not-less-than"
showFun HiFunOr             = "or"
showFun HiFunSub            = "sub"
showFun HiFunLength         = "length"
showFun HiFunToUpper        = "to-upper"
showFun HiFunToLower        = "to-lower"
showFun HiFunReverse        = "reverse"
showFun HiFunTrim           = "trim"
showFun HiFunList           = "list"
showFun HiFunRange          = "range"
showFun HiFunFold           = "fold"
showFun HiFunPackBytes      = "pack-bytes"
showFun HiFunUnpackBytes    = "unpack-bytes"
showFun HiFunEncodeUtf8     = "encode-utf8"
showFun HiFunDecodeUtf8     = "decode-utf8"
showFun HiFunZip            = "zip"
showFun HiFunUnzip          = "unzip"
showFun HiFunSerialise      = "serialise"
showFun HiFunDeserialise    = "deserialise"
showFun HiFunRead           = "read"
showFun HiFunWrite          = "write"
showFun HiFunMkDir          = "mkdir"
showFun HiFunChDir          = "cd"
showFun HiFunParseTime      = "parse-time"
showFun HiFunRand           = "rand"
showFun HiFunEcho           = "echo"
showFun HiFunCount          = "count"
showFun HiFunKeys           = "keys"
showFun HiFunValues         = "values"
showFun HiFunInvert         = "invert"

-- | Values
data HiValue
  = HiValueNull
  | HiValueBool Bool
  | HiValueFunction HiFun
  | HiValueNumber Rational
  | HiValueString Text
  | HiValueBytes ByteString
  | HiValueList (Seq HiValue)
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass Serialise

class ConvertibleToHiValue from where
  toValue :: from -> HiValue

class ConvertibleToHiExpr from where
  toExpr :: from -> HiExpr

instance {-# OVERLAPPABLE #-} ConvertibleToHiValue from => ConvertibleToHiExpr from where
  toExpr = HiExprValue . toValue

instance ConvertibleToHiValue HiFun where
  toValue = HiValueFunction

instance ConvertibleToHiValue Rational where
  toValue = HiValueNumber

instance ConvertibleToHiValue Text where
  toValue = HiValueString

instance ConvertibleToHiValue Char where
  toValue = toValue . T.singleton

instance ConvertibleToHiValue Bool where
  toValue = HiValueBool

instance ConvertibleToHiValue ByteString where
  toValue = HiValueBytes

instance ConvertibleToHiValue (Seq HiValue) where
  toValue = HiValueList

instance ConvertibleToHiValue HiAction where
  toValue = HiValueAction

instance ConvertibleToHiValue UTCTime where
  toValue = HiValueTime

instance {-# OVERLAPPABLE #-} Integral a => ConvertibleToHiValue a where
  toValue x = toValue $ toInteger x % 1

instance ConvertibleToHiValue [HiValue] where
  toValue = toValue . fromList

instance (ConvertibleToHiValue k, ConvertibleToHiValue v) => ConvertibleToHiValue (Map k v) where
  toValue dict = HiValueDict $ M.mapKeys toValue $ M.map toValue dict

instance ConvertibleToHiValue HiValue where
  toValue = id

instance ConvertibleToHiExpr HiValue where
  toExpr = HiExprValue

instance ConvertibleToHiExpr [(HiExpr, HiExpr)] where
  toExpr = HiExprDict

returnValue :: (ConvertibleToHiValue a, Monad m) => a -> m HiValue
returnValue = return . toValue

-- | Expressions
data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show, Generic)

-- | Evaluation errors
data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq)
