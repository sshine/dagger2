
module Dagger.Contract where

import Numeric.Natural (Natural)

-- | A 'Contract'' is parameterised over blockchain-specific :wq
data Contract' asset party expr contract time binding
  = Zero                                  -- ^ The 'Zero' contract does nothing.
  | Transfer asset party                  -- ^ The 'Transfer' contract moves @asset@ to @party@.
  | Scale expr contract                   -- ^ The 'Scale' contract multiplies asset quantities of @contract@.
  | Both contract contract                -- ^ The 'Both' contract executes both of its sub-contracts.
  | Delay time contract                   -- ^ The 'Delay' contract executes @contract@ after @time@.
  | IfWithin expr time contract contract  -- ^ The 'IfWithin' contract...
  | Let binding contract                  -- ^ Creates @binding@ for use in @contract@.

data ArithExpr expr
  = Add expr expr
  | Sub expr expr
  | Mul expr expr
  | Div expr expr

data LogicExpr expr
  = And expr expr
  | Or expr expr
  | Not expr
  | If expr expr expr

data CompareExpr expr
  = Eq expr expr
  | Lt expr expr
  | Gt expr expr
  | Leq expr expr
  | Geq expr expr
