module HW3.Parser
  ( parse
  ) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixN, InfixR, Postfix), makeExprParser)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (digitToInt, isAlpha, isAlphaNum)
import Data.List (intercalate, intersperse)
import Data.Ratio ((%))
import Data.Scientific (Scientific (base10Exponent, coefficient))
import Data.Sequence (Seq, fromList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import HW3.Base (ConvertibleToHiExpr (..), ConvertibleToHiValue (..), HiAction (..), HiExpr (..),
                 HiFun (..), HiValue (..), showFun)
import Text.Megaparsec (MonadParsec (eof, notFollowedBy, try), ParseErrorBundle, Parsec, between,
                        choice, empty, many, manyTill, runParser, satisfy, sepBy, sepBy1, sepEndBy,
                        some, (<?>))
import Text.Megaparsec.Char (char, hexDigitChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

type Op = Operator Parser HiExpr

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse input = runParser (sc *> pExpr <* eof) "" input

pExpr :: Parser HiExpr
pExpr = makeExprParser (choice
  [ toExpr <$> pValue
  , parens pExpr
  , pName
  , toExpr <$> pDict
  , HiExprApply (toExpr HiFunList) <$> try pList]) opTable <?> "expression"

pName :: Parser HiExpr
pName = toExpr <$> choice
  [ HiActionCwd <$ symbol "cwd"
  , HiActionNow <$ symbol "now" ]

pList :: Parser [HiExpr]
pList = between (symbol "[") (symbol "]") (pExpr `sepBy` symbol ",") <?> "list literal"

opTable :: [[Operator Parser HiExpr]]
opTable = [ [ applicationChain ]
          , [ makeInfixL HiFunMul "*" ""
            , makeInfixL HiFunDiv "/" "=" ]
          , [ makeInfixL HiFunAdd "+" ""
            , makeInfixL HiFunSub "-" "" ]
          , [ makeInfixN HiFunEquals "==" ""
            , makeInfixN HiFunNotEquals "/=" ""
            , makeInfixN HiFunNotGreaterThan "<=" ""
            , makeInfixN HiFunNotLessThan ">=" ""
            , makeInfixN HiFunLessThan "<" "="
            , makeInfixN HiFunGreaterThan ">" "=" ]
          , [ makeInfixR HiFunAnd "&&" "" ]
          , [ makeInfixR HiFunOr "||" "" ] ]
  where
    makeInfixL, makeInfixR, makeInfixN :: HiFun -> String -> String -> Op
    makeInfixL = makeOp InfixL
    makeInfixR = makeOp InfixR
    makeInfixN = makeOp InfixN

    makeOp :: (Parser (HiExpr -> HiExpr -> HiExpr) -> Op) -> HiFun -> String -> String -> Op
    makeOp fixity f name notFollowed = fixity
      $ (\a b -> HiExprApply (toExpr f) [a, b])
      <$ try (symbol name <* notFollowedBy (choice (map char notFollowed)))

applicationChain :: Op
applicationChain = Postfix
  $ foldr1 (flip (.)) . map addArgsOrExclam <$> pApplicationArgumentsOrExclam
    where
      addArgsOrExclam (Just args) head = HiExprApply head args
      addArgsOrExclam Nothing head     = HiExprRun head

pApplicationArgumentsOrExclam :: Parser [Maybe [HiExpr]]
pApplicationArgumentsOrExclam = some (choice
  [ Just <$> parens (pExpr `sepBy` symbol ",")
  , char '.' *> (Just . (: []) . toExpr . T.pack . intercalate "-" <$> names)
  , Nothing <$ symbol "!"]) <?> "arguments of function application"
    where
      names :: Parser [[Char]]
      names = ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy` char '-'

pValue :: Parser HiValue
pValue = choice
  [ toValue <$> pNumber
  , toValue <$> pFunction
  , toValue <$> pBoolean
  , toValue <$> pString
  , toValue <$> pNull
  , toValue <$> pBytes ] <?> "value"

pDict :: Parser [(HiExpr, HiExpr)]
pDict = between (symbol "{") (symbol "}") (entry `sepBy` symbol ",") <?> "dictionary literal"
  where
    entry = do
      k <- pExpr
      void $ symbol ":"
      v <- pExpr
      return (k, v)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

pBytes :: Parser ByteString
pBytes = B.pack <$> try (between (symbol "[" *> symbol "#") (symbol "#" *> symbol "]")
  ((byte <?> "byte literal") `sepEndBy` lexeme space1)) <?> "byte string literal"
  where
    byte = do
      c1 <- digitToInt <$> hexDigitChar
      c2 <- digitToInt <$> hexDigitChar
      return $ fromIntegral $ c1 * 16 + c2

pNull :: Parser HiValue
pNull = HiValueNull <$ symbol "null"

pString :: Parser Text
pString = T.pack <$> (string "\"" *> manyTill L.charLiteral (symbol "\"")) <?> "string literal"

pBoolean :: Parser Bool
pBoolean = choice
  [ True <$ symbol "true"
  , False <$ symbol "false" ] <?> "boolean literal"

pNumber :: Parser Rational
pNumber = do
  sc <- lexeme $ L.signed sc L.scientific
  let exp = base10Exponent sc
  let coef = coefficient sc
  return (if exp >= 0
    then (10 ^ exp * coef) % 1
    else coef % (10 ^ (-exp))) <?> "number literal"

pFunction :: Parser HiFun
pFunction = choice $ map pFun (enumFrom minBound :: [HiFun])
  where
    pFun :: HiFun -> Parser HiFun
    pFun f = f <$ symbol (showFun f)
