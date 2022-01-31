{-# LANGUAGE OverloadedStrings #-}

module HW3.Pretty
  ( prettyValue
  ) where

import qualified Data.ByteString as B
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Ratio (denominator, numerator)
import Data.Scientific (FPFormat (Fixed), formatScientific, fromRationalRepetendUnlimited)
import HW3.Base (HiAction (..), HiFun (..), HiValue (..), showFun, toValue)
import Prettyprinter (Doc, Pretty, braces, brackets, comma, concatWith, dquotes, enclose, hsep,
                      parens, pretty, punctuate, space, surround, viaShow, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)
import Text.Printf (printf)

commaSeparated :: [Doc ann] -> Doc ann
commaSeparated = hsep . punctuate comma

instance Pretty HiValue where
  pretty (HiValueNumber n) = let (num, denom) = (numerator n, denominator n) in
    case denom of
      1 -> pretty num -- because formatScientific will add ".0" to integers
      _ -> case fromRationalRepetendUnlimited n of
        (sc, Nothing) -> pretty $ formatScientific Fixed Nothing sc
        (_, Just _)   -> case quot of
            0 -> (if rem > 0 then "" else "-") <> fractionStr
            _ -> pretty quot
              <+> (if quot > 0 then "+" else "-")
              <+> fractionStr
          where
            (quot, rem) = quotRem num denom
            fractionStr = viaShow (abs rem) <> "/" <> viaShow denom
  pretty (HiValueFunction fn) = pretty $ showFun fn
  pretty (HiValueBool b) = if b then "true" else "false"
  pretty HiValueNull = "null"
  pretty (HiValueString t) = viaShow t
  pretty (HiValueList lst) = brackets $ commaSeparated $ map pretty $ toList lst
  pretty (HiValueBytes b) = enclose "[#" "#]" $ pretty
    $ unwords (map (printf "%02x") $ B.unpack b)
  pretty (HiValueDict dict) = braces $ commaSeparated $ map showKV $ M.toList dict
    where
      showKV (k, v) = pretty k <> ":" <+> pretty v
  pretty (HiValueTime time) = funCall HiFunParseTime [dquotes $ viaShow time]
  pretty (HiValueAction action) = case action of
    HiActionRead path        -> funCall HiFunRead [viaShow path]
    HiActionWrite path bytes -> funCall HiFunWrite [viaShow path, pretty $ toValue bytes]
    HiActionMkDir path       -> funCall HiFunMkDir [viaShow path]
    HiActionChDir path       -> funCall HiFunChDir [viaShow path]
    HiActionRand start end   -> funCall HiFunRand [viaShow start, viaShow end]
    HiActionEcho text        -> funCall HiFunEcho [viaShow text]
    HiActionCwd              -> "cwd"
    HiActionNow              -> "now"

funCall :: HiFun -> [Doc ann] -> Doc ann
funCall name args = pretty (showFun name) <> parens (commaSeparated args)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = pretty
