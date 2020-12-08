module Sword.Contract.Parser
  ( parseContract,
    parseSwordDiffTime,
  )
where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Bifunctor (first)
import Data.Char (isLetter)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Numeric.Natural (Natural)
import Sword.Contract (Asset (..), Contract (..), Expr (..), Oracle (..), Party (..))
import Sword.Time (SwordDiffTime, days, hours, minutes, seconds, weeks)
import Text.Megaparsec
import Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as Mega
import qualified Text.Megaparsec.Error as Mega

type Parser = Parsec Void Text

parseContract :: Integral word => Text -> Either String (Contract word)
parseContract = first Mega.errorBundlePretty . parse contractP ""

parseSwordDiffTime :: Text -> Either String SwordDiffTime
parseSwordDiffTime = first Mega.errorBundlePretty . parse swordDiffTimeP ""

contractP :: Integral word => Parser (Contract word)
contractP =
  asum
    [ zeroP <?> "zero",
      transferP <?> "transfer",
      scaleP <?> "scale",
      bothP <?> "both",
      delayP <?> "delay",
      ifWithinP <?> "if-within"
    ]

zeroP :: Integral word => Parser (Contract word)
zeroP = Zero <$ symbol "zero"

transferP :: Integral word => Parser (Contract word)
transferP =
  fun "transfer" $
    Transfer <$> assetP <* symbol "," <*> partyP

scaleP :: Integral word => Parser (Contract word)
scaleP =
  fun "scale" $
    Scale <$> exprP <* symbol "," <*> contractP

bothP :: Integral word => Parser (Contract word)
bothP =
  fun "both" $
    Both <$> contractP <* symbol "," <*> contractP

delayP :: Integral word => Parser (Contract word)
delayP =
  fun "delay" $
    Delay <$> swordDiffTimeP <* symbol "," <*> contractP

ifWithinP :: Integral word => Parser (Contract word)
ifWithinP = do
  cond <- symbol "if" *> exprP
  time <- symbol "within" *> swordDiffTimeP
  thenC <- symbol "then" *> contractP
  elseC <- symbol "else" *> contractP
  pure (IfWithin cond time thenC elseC)

assetP :: Parser Asset
assetP = Asset <$> takeWhile1P Nothing isLetter <?> "asset"

partyP :: Parser Party
partyP = Party <$> takeWhile1P Nothing isLetter <?> "party"

oracleP :: Parser Oracle
oracleP = Oracle <$> takeWhile1P Nothing isLetter

exprP :: Integral word => Parser (Expr word)
exprP =
  label "expression" $
    ifExprP
      <|> makeExprParser
        termP
        [ [ InfixL (Mul <$ symbol "*"),
            InfixL (Div <$ symbol "/")
          ],
          [ InfixL (Add <$ symbol "+"),
            InfixL (Sub <$ symbol "-")
          ],
          [ InfixN (Eq <$ symbol "="),
            InfixN (Geq <$ symbol ">="),
            InfixN (Leq <$ symbol "<="),
            InfixN (Lt <$ symbol "<"),
            InfixN (Gt <$ symbol ">")
          ],
          [InfixL (And <$ keyword "&&")],
          [InfixL (Or <$ keyword "||")]
        ]

ifExprP :: Integral word => Parser (Expr word)
ifExprP =
  If <$> (keyword "if" *> exprP)
    <*> (keyword "then" *> exprP)
    <*> (keyword "else" *> exprP)

termP :: Integral word => Parser (Expr word)
termP =
  asum
    [ fun "min" $ Min <$> exprP <* symbol "," <*> exprP,
      fun "max" $ Max <$> exprP <* symbol "," <*> exprP,
      fun "not" $ Not <$> exprP,
      fun "get" $ Get <$> oracleP,
      Const <$> constP,
      Bool True <$ keyword "true",
      Bool False <$ keyword "false",
      parens exprP
    ]

constP :: Integral word => Parser word
constP =
  asum [Mega.decimal, chunk "0x" *> Mega.hexadecimal] <?> "integer"

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
    timeUnitP =
      asum
        [ plural "second" $> seconds,
          plural "minute" $> minutes,
          plural "hour" $> hours,
          plural "day" $> days,
          plural "week" $> weeks
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