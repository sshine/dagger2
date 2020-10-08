
module Dagger.Contract.Parser
  ( parseContract
  ) where

import           Control.Monad (void)
import           Data.Bifunctor (first)
import           Data.Foldable (asum)
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char -- (space, digitChar, hexDigitChar, char, char')
import qualified Text.Megaparsec.Char.Lexer as Mega
import qualified Text.Megaparsec.Error as Mega
import           Control.Monad.Combinators.Expr

import           Dagger.Contract
import           Dagger.Time

-- Parse: type Contract time asset party expr

type Parser = Parsec Void Text

parseContract s = first Mega.errorBundlePretty $ parse timeP "" s

timeP :: Num n => Parser [(n, TimeUnit)]
timeP = pair `sepBy1` comma
  where
    pair = (,) <$> numberP <*> timeUnitP
    comma = symbol ","

    numberP :: Num n => Parser n
    numberP = lexeme Mega.decimal

    timeUnitP :: Parser TimeUnit
    timeUnitP = asum
      [ Seconds <$ plural "second"
      , Minutes <$ plural "minute"
      , Days    <$ plural "day"
      , Hours   <$ plural "hour"
      , Weeks   <$ plural "week"
      ]

plural :: Text -> Parser ()
plural word = lexeme . void $
  chunk (word <> "s") <|> chunk word

symbol :: Text -> Parser ()
symbol = lexeme . void . chunk

lexeme :: Parser a -> Parser a
lexeme = (<* space)