
module Sword.Time where

import Data.Bifunctor (second)
import Numeric.Natural (Natural)

newtype BlockTimestamp = BlockTimestamp Natural
  deriving (Eq, Show)

data TimeUnit
  = Seconds
  | Minutes
  | Hours
  | Days
  | Weeks
  deriving (Eq, Show)

unit :: Integral i => TimeUnit -> i
unit = \case
  Seconds -> 1
  Minutes -> 60
  Hours   -> 60 * 60
  Days    -> 60 * 60 * 24
  Weeks   -> 60 * 60 * 24 * 7

collect :: Integral i => [(i, TimeUnit)] -> i
collect = sum . map (uncurry (*) . second unit)
