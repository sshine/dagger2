{-# LANGUAGE DeriveFunctor #-}

module Sword.Contract where

import Data.Fix (Fix(..))
import Data.Text (Text)

import Sword.Time

-- | A 'Contract' is...
type Contract word = Fix (Contract' (Expr word))

-- | An 'Expr' is...
type Expr word = Fix (Expr' word)

-- | A 'Contract'' is...
data Contract' expr contract
  = Zero                                          -- ^ The 'Zero' contract does nothing.
  | Transfer Asset Party                          -- ^ The 'Transfer' contract moves @asset@ to @party@.
  | Scale expr contract                           -- ^ The 'Scale' contract multiplies asset quantities of @contract@.
  | Both contract contract                        -- ^ The 'Both' contract executes both of its sub-contracts.
  | Delay SwordDiffTime contract                  -- ^ The 'Delay' contract executes @contract@ after @time@.
  | IfWithin expr SwordDiffTime contract contract -- ^ The 'IfWithin' contract conditionally executes one of its two branches.
  deriving (Show, Functor)

-- | An 'Expr'' is...
data Expr' word expr
  = Add expr expr
  | Sub expr expr
  | Mul expr expr
  | Div expr expr
  | Min expr expr
  | Max expr expr

  | And expr expr
  | Or expr expr
  | Not expr
  | If expr expr expr

  | Eq expr expr
  | Lt expr expr
  | Gt expr expr
  | Leq expr expr
  | Geq expr expr

  | Const word
  | Bool Bool
  | Get Oracle
  deriving (Show, Functor)

newtype Party = Party Text
  deriving (Eq, Ord, Show)

newtype Asset = Asset Text
  deriving (Eq, Ord, Show)

newtype Oracle = Oracle Text
  deriving (Eq, Ord, Show)

zero :: Contract word
zero = Fix Zero

transfer :: Asset -> Party -> Contract word
transfer asset party = Fix (Transfer asset party)

scale :: Expr word -> Contract word -> Contract word
scale factor contract = Fix (Scale factor contract)

both :: Contract word -> Contract word -> Contract word
both contract1 contract2 = Fix (Both contract1 contract2)

delay :: SwordDiffTime -> Contract word -> Contract word
delay time contract = Fix (Delay time contract)

ifWithin :: Expr word
         -> SwordDiffTime
         -> Contract word
         -> Contract word
         -> Contract word
ifWithin cond time thenContract elseContract =
  Fix (IfWithin cond time thenContract elseContract)

binopFix :: (x -> y -> f (Fix f)) -> x -> y -> Fix f
binopFix op x y = Fix (op x y)

add :: Expr word -> Expr word -> Expr word
add = binopFix Add

sub :: Expr word -> Expr word -> Expr word
sub = binopFix Sub

mul :: Expr word -> Expr word -> Expr word
mul = binopFix Mul

div :: Expr word -> Expr word -> Expr word
div = binopFix Div

min :: Expr word -> Expr word -> Expr word
min = binopFix Min

max :: Expr word -> Expr word -> Expr word
max = binopFix Max

and :: Expr word -> Expr word -> Expr word
and = binopFix And

or :: Expr word -> Expr word -> Expr word
or = binopFix Or

not :: Expr word -> Expr word
not p = Fix (Not p)

eq :: Expr word -> Expr word -> Expr word
eq = binopFix Eq

lt :: Expr word -> Expr word -> Expr word
lt = binopFix Lt

gt :: Expr word -> Expr word -> Expr word
gt = binopFix Gt

leq :: Expr word -> Expr word -> Expr word
leq = binopFix Leq

geq :: Expr word -> Expr word -> Expr word
geq = binopFix Geq

if' :: Expr word -> Expr word -> Expr word -> Expr word
if' c x y = Fix (If c x y)

const :: word -> Expr word
const word = Fix (Const word)

bool :: Bool -> Expr word
bool b = Fix (Bool b)

get :: Oracle -> Expr word
get oracle = Fix (Get oracle)
