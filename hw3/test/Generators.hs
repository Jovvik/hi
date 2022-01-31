module Generators
  (
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Generic.Random (genericArbitraryRec, genericArbitraryRecG, listOf', uniform, withBaseCase,
                       (%))
import HW3.Base (HiAction, HiExpr (HiExprValue), HiFun, HiValue)
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), Gen, Positive (Positive), choose, frequency,
                              suchThat)

instance Arbitrary ByteString where
  arbitrary = B.pack <$> listOf' arbitrary

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary HiAction where
  arbitrary = genericArbitraryRec uniform

instance Arbitrary HiFun where
  arbitrary = genericArbitraryRec -- unzip may fail, don't test it
    (1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 0 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % 1 % ())

genPos :: Gen Int
genPos = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 0)

instance Arbitrary UTCTime where
  arbitrary = posixSecondsToUTCTime . fromInteger . toInteger <$> genPos

instance Arbitrary HiValue where
  arbitrary = genericArbitraryRec (100 % 100 % 100 % 0 % 100 % 100 % 10 % 10 % 100 % 1 % ())

genArgs :: Gen [HiExpr]
genArgs = frequency
  [ (5, (: []) <$> arbitrary)
  , (2, do
      fst <- arbitrary
      snd <- arbitrary
      return [fst, snd])
  , (1, do
      fst <- arbitrary
      snd <- arbitrary
      thrd <- arbitrary
      return [fst, snd, thrd])]

instance Arbitrary HiExpr where
  arbitrary = genericArbitraryRecG genArgs (100 % 30 % 0 % 1 % ()) -- run may fail
