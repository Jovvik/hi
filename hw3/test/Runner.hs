module Runner
  ( RunResult (..)
  , evalFailsWith
  , evalSame
  , makeOp
  , makeOpExpr
  , maybeDiffErrors
  , parseFails
  , runHi
  , runHiExpr
  , runHiIO
  , runHiIOEq
  , showExpr
  , (@!)
  ) where

import Control.Exception (SomeException, catch, try)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (Identity, runIdentity)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import HW3.Action (HIO (runHIO), HiPermission)
import HW3.Base (HiError, HiExpr (..), HiMonad (..), HiValue (HiValueNull))
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue)
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import Text.Megaparsec (errorBundlePretty)

data RunResult
  = Success String
  | ParseError String
  | EvalError HiError
  | PermissionError HiPermission
  deriving (Show)

instance Eq RunResult where
  Success lhs == Success rhs                 = lhs == rhs
  ParseError _ == ParseError _               = True
  EvalError lhs == EvalError rhs             = lhs == rhs
  PermissionError lhs == PermissionError rhs = lhs == rhs
  _ == _                                     = False

instance HiMonad Identity where
  runAction = const $ return HiValueNull

runHi :: String -> RunResult
runHi s = case parse s of
  Left peb   -> ParseError $ errorBundlePretty peb
  Right expr -> case runIdentity $ eval expr of
    Left err  -> EvalError err
    Right val -> Success $ show $ prettyValue val

runHiIOEq :: String -> [HiPermission] -> [Char] -> IO ()
runHiIOEq s perms expected = do
  res <- runHiIO s perms
  assertBool ("Expected " ++ expected ++ ", got" ++ show res) $ res == Success expected

runHiIO :: String -> [HiPermission] -> IO RunResult
runHiIO s perms = do
  case parse s of
    Left peb   -> return $ ParseError $ errorBundlePretty peb
    Right expr -> do
      res <- liftIO (runHIO (eval expr) (S.fromList perms))
      return $ case res of
        Left err  -> EvalError err
        Right val -> Success $ show $ prettyValue val

showExpr :: HiExpr -> String
showExpr (HiExprValue v)      = show $ prettyValue v
showExpr (HiExprApply f args) = showExpr f ++ "(" ++ intercalate ", " (map showExpr args)  ++ ")"
showExpr (HiExprDict lp)      = "{" ++
  intercalate ", " (map (\(k, v) -> showExpr k ++ ": " ++ showExpr v) lp) ++ "}"
showExpr (HiExprRun r)        = showExpr r ++ "!"

runHiExpr :: HiExpr -> RunResult
runHiExpr = runHi . showExpr

evalSame :: String -> String -> Bool
evalSame expr1 expr2 = runHi expr1 == runHi expr2

maybeDiffErrors :: String -> String -> Bool
maybeDiffErrors expr1 expr2 = case (runHi expr1, runHi expr2) of
  (Success e1, Success e2)     -> e1 == e2
  (EvalError _, EvalError _)   -> True
  (ParseError _, ParseError _) -> True
  _                            -> False

bothSucceed :: String -> String -> Bool
bothSucceed expr1 expr2 = case (runHi expr1, runHi expr2) of
  (Success e1, Success e2) -> e1 == e2
  _                        -> False

succeedsTo :: String -> String -> Bool
succeedsTo expr expected = runHi expr == Success expected

infix 1 @!
(@!) :: String -> String -> Assertion
(@!) = hSucceedsTo

hSucceedsTo :: String -> String -> Assertion
hSucceedsTo expr expected = runHi expr @?= Success expected

failsWith :: (t -> RunResult) -> String -> t -> Assertion
failsWith errKind expr expected = runHi expr @?= errKind expected

parseFails :: String -> Assertion
parseFails expr = runHi expr @?= ParseError ""

evalFailsWith :: String -> HiError -> Assertion
evalFailsWith = failsWith EvalError

makeOp :: (Show a) => String -> [a] -> String
makeOp name xs = name ++ "(" ++ intercalate ", " (map show xs) ++ ")"

makeOpExpr :: String -> [HiExpr] -> String
makeOpExpr name exprs = name ++ "(" ++ intercalate ", " (map showExpr exprs) ++ ")"
