module Idempotence
  ( idempotence
  ) where
import Generators ()
import Runner (RunResult (..), runHi, runHiExpr)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

idempotence :: TestTree
idempotence = testProperty "Parse-eval idempotence"
  (\expr -> case runHiExpr expr of
    Success s1 -> Just $ case runHi s1 of
      Success s2 -> s1 == s2
      _          -> False
    _         -> Nothing)
