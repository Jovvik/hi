{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ViewPatterns              #-}

module HW3.Evaluator
  ( eval
  ) where

import Codec.Compression.Zlib (CompressParams (compressLevel), bestCompression, compressWith,
                               decompress, defaultCompressParams)
import Codec.Serialise (DeserialiseFailure (DeserialiseFailure), deserialise, deserialiseOrFail,
                        serialise)
import Control.Arrow ((***))
import Control.Monad (foldM, join)
import Control.Monad.Except (Except, ExceptT (ExceptT), MonadError (throwError), runExceptT)
import Control.Monad.Trans.Except (except)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (fold, foldlM, toList)
import Data.ListLike (ListLike)
import qualified Data.ListLike as LL
import Data.Map (Map, insertWith)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator, (%))
import Data.Semigroup (stimes)
import Data.Sequence (Seq (Empty, (:<|)), cycleTaking, fromList, (><))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (UTCTime (UTCTime), addUTCTime, diffUTCTime)
import HW3.Base (ConvertibleToHiExpr (toExpr), ConvertibleToHiValue (toValue), HiAction (..),
                 HiError (..), HiExpr (..), HiFun (..), HiMonad (runAction), HiValue (..),
                 returnValue)
import Text.Read (readMaybe)

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT (eval' expr)

eval' :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
eval' (HiExprValue value) = return value
eval' (HiExprDict pairs) = do
  pairs' <- mapM evalBoth pairs
  returnValue $ M.fromList pairs'
    where
      evalBoth (x, y) = do
        x' <- eval' x
        y' <- eval' y
        return (x', y')
eval' (HiExprRun (HiExprValue val)) = case val of
  HiValueAction action -> ExceptT $ Right <$> runAction action
  _                    -> throwError HiErrorInvalidArgument
eval' (HiExprRun notAnAction) = do
  notAnAction' <- eval' notAnAction
  eval' $ HiExprRun $ toExpr notAnAction'
eval' (HiExprApply (HiExprValue op) args) = case op of
  HiValueFunction f -> evalFun f args
  HiValueDict dict -> case args of
    [key] -> do
      key' <- eval' key
      case M.lookup key' dict of
        Nothing  -> return HiValueNull
        Just val -> return val
    _ -> throwError HiErrorArityMismatch
  _ -> do
    evaluatedArgs <- mapM eval' args
    indexOrSlice op evaluatedArgs
eval' (HiExprApply nonValueOp args) = do
  op <- eval' nonValueOp
  eval' $ HiExprApply (toExpr op) args

isLazy :: HiFun -> Bool
isLazy x = x `elem` [HiFunAnd, HiFunOr, HiFunIf]

evalFun :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalFun HiFunAnd [lhs, rhs] = do
  x <- eval' lhs
  case x of
    HiValueNull -> return x
    HiValueBool b -> if b
      then eval' rhs
      else return x
    _           -> eval' rhs
evalFun HiFunOr [lhs, rhs] = do
  x <- eval' lhs
  case x of
    HiValueNull -> eval' rhs
    HiValueBool b -> if b
      then return x
      else eval' rhs
    _           -> return x
evalFun HiFunIf [cond, ifTrue, ifFalse] = do
  cond' <- eval' cond
  case cond' of
    HiValueBool b -> if b
      then eval' ifTrue
      else eval' ifFalse
    _ -> throwError HiErrorInvalidArgument
evalFun HiFunFold [foldOp, coll] = do
  case coll of
    (HiExprApply (HiExprValue (HiValueFunction HiFunList)) args) -> case length args of
      0 -> return HiValueNull
      1 -> do eval' (head args)
      _ -> do
        head <- eval' (head args)
        foldlM folder head (tail args)
      where
        folder a b = eval' $ HiExprApply foldOp [toExpr a, b]
    _ -> do
      coll' <- eval' coll
      case coll' of
        HiValueList lst -> evalFun HiFunFold
          [foldOp, HiExprApply (toExpr HiFunList) $ map toExpr (toList lst)]
        _ -> throwError HiErrorInvalidArgument
evalFun (isLazy -> True) _ = throwError HiErrorArityMismatch
evalFun f args = do
  evaluatedArgs <- mapM eval' args
  evalStrict f evaluatedArgs

evalStrict :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
evalStrict HiFunEquals [lhs, rhs]      = returnValue $ lhs == rhs
evalStrict HiFunLessThan [lhs, rhs]    = returnValue $ lhs < rhs
evalStrict HiFunGreaterThan [lhs, rhs] = evalStrict HiFunLessThan [rhs, lhs]
evalStrict HiFunNotEquals args         = evalNot HiFunEquals args
evalStrict HiFunNotGreaterThan args    = evalNot HiFunGreaterThan args
evalStrict HiFunNotLessThan args       = evalNot HiFunLessThan args
evalStrict HiFunToUpper [arg]          = stringOp arg T.toUpper
evalStrict HiFunToLower [arg]          = stringOp arg T.toLower
evalStrict HiFunTrim [arg]             = stringOp arg T.strip
evalStrict HiFunEncodeUtf8 [arg]       = stringOp arg encodeUtf8
evalStrict HiFunAdd [lhs, rhs] = case (lhs, rhs) of
  (HiValueNumber x, HiValueNumber y)   -> returnValue $ x + y
  (HiValueTime t, HiValueNumber n)     -> returnValue $ addUTCTime (fromRational n) t
  (HiValueNumber n, HiValueTime t)     -> evalStrict HiFunAdd [rhs, lhs]
  (HiValueBytes b1, HiValueBytes b2)   -> returnValue $ b1 <> b2
  (HiValueList l1, HiValueList l2)     -> returnValue $ l1 <> l2
  (HiValueString s1, HiValueString s2) -> returnValue $ s1 <> s2
  _                                    -> throwError HiErrorInvalidArgument
evalStrict HiFunSub [lhs, rhs] = case (lhs, rhs) of
  (HiValueNumber x, HiValueNumber y) -> returnValue $ x - y
  (HiValueTime t1, HiValueTime t2)   -> returnValue $ toRational $ diffUTCTime t1 t2
  _                                  -> throwError HiErrorInvalidArgument
evalStrict HiFunMul [lhs, rhs] = case (lhs, rhs) of
  (HiValueNumber x, HiValueNumber y)   -> returnValue $ x * y
  (HiValueBytes b, HiValueNumber num)  -> containerTimes b num stimes
  (HiValueString s, HiValueNumber num) -> containerTimes s num stimes
  (HiValueList l, HiValueNumber num)   -> containerTimes l num fastTimes
    where
      fastTimes n l = cycleTaking (fromInteger n * LL.length l) l
  _                                    -> throwError HiErrorInvalidArgument
  where
    containerTimes c count multiplier = do
      num <- assertIsInteger count
      if num <= 0
        then throwError HiErrorInvalidArgument
        else returnValue $ multiplier num c
evalStrict HiFunDiv [lhs, rhs] = case (lhs, rhs) of
    (HiValueNumber x, HiValueNumber y)   -> case y of
      0 -> throwError HiErrorDivideByZero
      _ -> returnValue $ x / y
    (HiValueString s1, HiValueString s2) -> returnValue $ s1 <> "/" <> s2
    _                                    -> throwError HiErrorInvalidArgument
evalStrict HiFunNot [arg] = case arg of
    HiValueBool b -> returnValue $ not b
    _             -> throwError HiErrorInvalidArgument
evalStrict HiFunLength [arg] = case arg of
    HiValueBytes bytes -> returnValue $ LL.length bytes
    HiValueList lst    -> returnValue $ LL.length lst
    HiValueString txt  -> returnValue $ LL.length txt
    _                  -> throwError HiErrorInvalidArgument
evalStrict HiFunList args = do returnValue args
evalStrict HiFunReverse [arg] = case arg of
  HiValueBytes bytes -> returnValue $ LL.reverse bytes
  HiValueList lst    -> returnValue $ LL.reverse lst
  HiValueString txt  -> returnValue $ LL.reverse txt
  _                  -> throwError HiErrorInvalidArgument
evalStrict HiFunRange [start, end] = do
  start <- assertIsRational start
  end <- assertIsRational end
  returnValue $ map toValue [start .. end]
evalStrict HiFunPackBytes [arg] = case arg of
  HiValueList lst -> do
    ints <- mapM assertIsNumberAndInteger $ toList lst
    ints' <- mapM isByte ints
    returnValue $ B.pack $ map fromIntegral ints'
      where
        isByte :: HiMonad m => Integer -> ExceptT HiError m Integer
        isByte n
          | n > 255 = throwError HiErrorInvalidArgument
          | n < 0 = throwError HiErrorInvalidArgument
          | otherwise = return n
  _ -> throwError HiErrorInvalidArgument
evalStrict HiFunUnpackBytes [arg] = case arg of
  HiValueBytes bytes -> returnValue nums
    where
      nums = map toValue $ B.unpack bytes
  _ -> throwError HiErrorInvalidArgument
evalStrict HiFunDecodeUtf8 [arg] = case arg of
  HiValueBytes bytes -> case decodeUtf8' bytes of
    Left _    -> returnValue HiValueNull
    Right txt -> returnValue txt
  _ -> throwError HiErrorInvalidArgument
evalStrict HiFunZip [arg] = case arg of
  HiValueBytes bytes -> returnValue $ toStrict compressed
    where
      compressParams = defaultCompressParams { compressLevel = bestCompression }
      compressed = compressWith compressParams $ fromStrict bytes
  _ -> throwError HiErrorInvalidArgument
evalStrict HiFunUnzip [arg] = case arg of
  HiValueBytes bytes -> returnValue $ toStrict $ decompress $ fromStrict bytes
  _                  -> throwError HiErrorInvalidArgument
evalStrict HiFunSerialise [arg] = returnValue $ toStrict $ serialise arg
evalStrict HiFunDeserialise [arg] = case arg of
  HiValueBytes bytes -> case deserialiseOrFail $ fromStrict bytes of
    Left _                 -> throwError HiErrorInvalidArgument
    Right (val :: HiValue) -> returnValue val
  _                  -> throwError HiErrorInvalidArgument
evalStrict HiFunWrite [arg1, arg2] = case (arg1, arg2) of
  (HiValueString path, HiValueString txt) -> returnValue
    $ HiActionWrite (T.unpack path) (encodeUtf8 txt)
  _                                       -> throwError HiErrorInvalidArgument
evalStrict HiFunMkDir [arg] = stringOp arg (HiActionMkDir . T.unpack)
evalStrict HiFunChDir [arg] = stringOp arg (HiActionChDir . T.unpack)
evalStrict HiFunRead [arg]  = stringOp arg (HiActionRead . T.unpack)
evalStrict HiFunEcho [arg]  = stringOp arg HiActionEcho
evalStrict HiFunParseTime [arg] = case arg of
  HiValueString txt -> case readMaybe $ T.unpack txt :: Maybe UTCTime of
    Nothing   -> return HiValueNull
    Just time -> returnValue time
  _                 -> throwError HiErrorInvalidArgument
evalStrict HiFunRand [start, end] = do
  start' <- assertIsNumberAndInteger start
  end' <- assertIsNumberAndInteger end
  if not (isInt start') || not (isInt end')
    then throwError HiErrorInvalidArgument
    else returnValue $ HiActionRand (fromInteger start') (fromInteger end')
    where
      isInt x = x <= toInteger (maxBound :: Int) && x >= toInteger (minBound :: Int)
evalStrict HiFunKeys [arg] = case arg of
  HiValueDict dict -> returnValue $ M.keys dict
  _                -> throwError HiErrorInvalidArgument
evalStrict HiFunValues [arg] = case arg of
  HiValueDict dict -> returnValue $ M.elems dict
  _                -> throwError HiErrorInvalidArgument
evalStrict HiFunInvert [arg] = case arg of
  HiValueDict dict -> returnValue $ foldl worker M.empty $ M.toList dict
    where
      worker dict (k, v) = insertWith (++) v [k] dict
  _                -> throwError HiErrorInvalidArgument

evalStrict HiFunCount [arg] = case arg of
  HiValueBytes bytes -> returnCounts bytes
  HiValueList lst    -> returnCounts lst
  HiValueString txt  -> returnCounts txt
  _                  -> throwError HiErrorInvalidArgument
  where
    returnCounts
      :: (ListLike l a, ConvertibleToHiValue a, Ord a, HiMonad m)
      => l
      -> ExceptT HiError m HiValue
    returnCounts = returnValue . counts . LL.toList

    counts :: Ord a => [a] -> Map a Int
    counts lst = foldl (\dict a -> insertWith (+) a 1 dict) M.empty lst

evalStrict _ _  = throwError HiErrorArityMismatch

stringOp
  :: (ConvertibleToHiValue a, HiMonad m)
  => HiValue
  -> (Text -> a)
  -> ExceptT HiError m HiValue
stringOp arg f = do
  case arg of
    HiValueString txt -> returnValue $ f txt
    _                 -> throwError HiErrorInvalidArgument

evalNot :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
evalNot negated args = evalFun HiFunNot [HiExprApply (toExpr negated) (map toExpr args)]

indexOrSlice
  :: HiMonad m
  => HiValue
  -> [HiValue]
  -> ExceptT HiError m HiValue
indexOrSlice val args = case val of
  (HiValueBytes bytes) -> helper bytes args id
  (HiValueList lst)    -> helper lst args id
  (HiValueString text) -> helper text args T.singleton
  _                    -> throwError HiErrorInvalidFunction
  where
    helper :: (ListLike l a, HiMonad m, ConvertibleToHiValue v, ConvertibleToHiValue l)
      => l
      -> [HiValue]
      -> (a -> v)
      -> ExceptT HiError m HiValue
    helper lst [idx] f = do
      idx <- assertIsNumberAndInteger idx
      if idx < 0 || idx >= toInteger (maxBound :: Int) || fromInteger idx >= LL.length lst
        then returnValue HiValueNull
        else returnValue $ f $ LL.index lst (fromInteger idx)
    helper lst [start, end] _ = do
      let len = toInteger $ LL.length lst
      start <- fromMaybe 0 <$> assertIsIntegerOrNull start
      end <- fromMaybe len <$> assertIsIntegerOrNull end
      let start' = if start >= 0 then start else start + len
      let end' = if end >= 0 then end else end + len
      returnValue $ if start' >= end'
        then LL.empty
        else LL.take (fromInteger $ end' - start')
          $ LL.drop (fromInteger start') lst
    helper lst _ _ = throwError HiErrorArityMismatch

assertIsInteger :: Monad m => Rational -> ExceptT HiError m Integer
assertIsInteger ratio = case (numerator ratio, denominator ratio) of
  (num, 1) -> return num
  _        -> throwError HiErrorInvalidArgument

assertIsRational :: Monad m => HiValue -> ExceptT HiError m Rational
assertIsRational (HiValueNumber num) = return num
assertIsRational _                   = throwError HiErrorInvalidArgument

assertIsNumberAndInteger :: Monad m => HiValue -> ExceptT HiError m Integer
assertIsNumberAndInteger (HiValueNumber frac) = assertIsInteger frac
assertIsNumberAndInteger _                    = throwError HiErrorInvalidArgument

assertIsIntegerOrNull :: HiMonad m => HiValue -> ExceptT HiError m (Maybe Integer)
assertIsIntegerOrNull (HiValueNumber frac) = Just <$> assertIsInteger frac
assertIsIntegerOrNull HiValueNull          = return Nothing
assertIsIntegerOrNull _                    = throwError HiErrorInvalidArgument
