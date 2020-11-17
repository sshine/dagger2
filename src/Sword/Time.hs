
module Sword.Time where

import qualified Data.Map as Map
import           Data.Map (Map)
import           Numeric.Natural (Natural)
import           Data.Monoid (Sum(..))

-- FIXME: Add 'Now' as alternative option.
newtype SwordDiffTime = SwordDiffTime { unSwordDiffTime :: Map TimeUnit Natural }
  deriving (Eq, Show)

data TimeUnit
  = Seconds
  | Minutes
  | Hours
  | Days
  | Weeks
  deriving (Eq, Ord, Show)

instance Semigroup SwordDiffTime where
  SwordDiffTime t1 <> SwordDiffTime t2 =
    SwordDiffTime (Map.unionWith (+) t1 t2)

instance Monoid SwordDiffTime where
  mempty = SwordDiffTime Map.empty

seconds :: Natural -> SwordDiffTime
seconds = SwordDiffTime . Map.singleton Seconds

minutes :: Natural -> SwordDiffTime
minutes = SwordDiffTime . Map.singleton Minutes

hours :: Natural -> SwordDiffTime
hours = SwordDiffTime . Map.singleton Hours

days :: Natural -> SwordDiffTime
days = SwordDiffTime . Map.singleton Days

weeks :: Natural -> SwordDiffTime
weeks = SwordDiffTime . Map.singleton Weeks

timeUnitFactor :: TimeUnit -> Natural
timeUnitFactor = \case
  Seconds -> 1
  Minutes -> 60
  Hours -> 60 * 60
  Days -> 60 * 60 * 24
  Weeks -> 60 * 60 * 24 * 7
