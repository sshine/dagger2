
module Sword.Contract.Parser
  ( parseContract
  , parseSwordDiffTime
  ) where

import           Control.Monad (void)
import           Data.Bifunctor (first)
import           Data.Char (isLetter)
import           Data.Foldable (asum)
import           Data.Functor (($>))
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as Mega
import qualified Text.Megaparsec.Error as Mega
import           Control.Monad.Combinators.Expr (makeExprParser, Operator(..))
import           Numeric.Natural (Natural)

import qualified Sword.Contract as C
import           Sword.Contract (Contract, Expr, Asset, Party, Oracle)
import           Sword.Time (seconds, minutes, hours, days, weeks, SwordDiffTime)

type Parser = Parsec Void Text

parseContract :: Integral word => Text -> Either String (Contract word)
parseContract = first Mega.errorBundlePretty . parse contractP ""

parseSwordDiffTime :: Text -> Either String SwordDiffTime
parseSwordDiffTime = first Mega.errorBundlePretty . parse swordDiffTimeP ""

contractP :: Integral word => Parser (Contract word)
contractP = asum
  [ zeroP <?> "zero"
  , transferP <?> "transfer"
  , scaleP <?> "scale"
  , bothP <?> "both"
  , delayP <?> "delay"
  , ifWithinP <?> "if-within"
  ]

zeroP :: Integral word => Parser (Contract word)
zeroP = C.zero <$ symbol "zero"

transferP :: Integral word => Parser (Contract word)
transferP =
  fun "transfer" $ C.transfer <$> assetP
                <* symbol "," <*> partyP

scaleP :: Integral word => Parser (Contract word)
scaleP =
  fun "scale" $ C.scale <$> exprP
          <* symbol "," <*> contractP

bothP :: Integral word => Parser (Contract word)
bothP =
  fun "both" $ C.both <$> contractP
        <* symbol "," <*> contractP

delayP :: Integral word => Parser (Contract word)
delayP =
  fun "delay" $ C.delay <$> swordDiffTimeP
          <* symbol "," <*> contractP

ifWithinP :: Integral word => Parser (Contract word)
ifWithinP = do
  cond <- symbol "if" *> exprP
  time <- symbol "within" *> swordDiffTimeP
  thenC <- symbol "then" *> contractP
  elseC <- symbol "else" *> contractP
  pure (C.ifWithin cond time thenC elseC)

assetP :: Parser Asset
assetP = C.Asset <$> takeWhile1P Nothing isLetter <?> "asset"

partyP :: Parser Party
partyP = C.Party <$> takeWhile1P Nothing isLetter <?> "party"

oracleP :: Parser Oracle
oracleP = C.Oracle <$> takeWhile1P Nothing isLetter

exprP :: Integral word => Parser (Expr word)
exprP =
  label "expression" $
    makeExprParser termP
      [ [ InfixL (C.mul <$ symbol "*")
        , InfixL (C.div <$ symbol "/")
        ]
      , [ InfixL (C.add <$ symbol "+")
        , InfixL (C.sub <$ symbol "-")
        ]
      , [ InfixN (C.eq <$ symbol "=")
        , InfixN (C.geq <$ symbol ">=")
        , InfixN (C.leq <$ symbol "<=")
        , InfixN (C.lt <$ symbol "<")
        , InfixN (C.gt <$ symbol ">")
        ]
      , [ InfixL (C.and <$ keyword "and") ]
      , [ InfixL (C.or <$ keyword "or") ]
      ]

termP :: Integral word => Parser (Expr word)
termP = asum
  [ fun "min" $ C.min <$> exprP <* symbol "," <*> exprP
  , fun "max" $ C.max <$> exprP <* symbol "," <*> exprP
  , fun "not" $ C.not <$> exprP
  , fun "get" $ C.get <$> oracleP
  , C.const <$> constP
  , C.bool True <$ keyword "true"
  , C.bool False <$ keyword "false"
  , parens exprP
  ]

constP :: Integral word => Parser word
constP =
  asum [ Mega.decimal, chunk "0x" *> Mega.hexadecimal ] <?> "integer"

swordDiffTimeP :: Parser SwordDiffTime
swordDiffTimeP =
  mconcat <$> (singleSwordDiffTimeP `sepBy1` symbol ",") <?> "timespan"

singleSwordDiffTimeP :: Parser SwordDiffTime
singleSwordDiffTimeP = do
  n <- timeQtyP
  f <- timeUnitP
  pure (f n)
  where
    timeQtyP :: Num n => Parser n
    timeQtyP = lexeme Mega.decimal

    timeUnitP :: Parser (Natural -> SwordDiffTime)
    timeUnitP = asum
      [ plural "second" $> seconds
      , plural "minute" $> minutes
      , plural "hour"   $> hours
      , plural "day"    $> days
      , plural "week"   $> weeks
      ]

    plural :: Text -> Parser ()
    plural word =
      label' (word <> "(s)") . lexeme . void $
        chunk word <* optional (chunk "s")

fun :: Text -> Parser a -> Parser a
fun s p = label' s $ symbol s *> parens p

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

keyword :: Text -> Parser ()
keyword s =
  label' s . lexeme $
    chunk s >> notFollowedBy (satisfy isLetter)

label' :: Text -> Parser a -> Parser a
label' = label . Text.unpack

symbol :: Text -> Parser ()
symbol = lexeme . void . chunk

lexeme :: Parser a -> Parser a
lexeme = (<* space)