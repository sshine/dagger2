{-# LANGUAGE DeriveFunctor #-}

module Sword.Contract where

import Data.Text (Text)
import Sword.Time

-- | A 'Contract' is...
data Contract word
  = -- | The 'Zero' contract does nothing.
    Zero
  | -- | The 'Transfer' contract moves @asset@ to @party@.
    Transfer Asset Party
  | -- | The 'Scale' contract multiplies asset quantities of @contract@.
    Scale (Expr word) (Contract word)
  | -- | The 'Both' contract executes both of its sub-contracts.
    Both (Contract word) (Contract word)
  | -- | The 'Delay' contract executes @contract@ after @time@.
    Delay SwordDiffTime (Contract word)
  | -- | The 'IfWithin' contract conditionally executes one of its two branches
    -- if either the condition is true within the given timespan, or the timespan
    -- passes without it having been true.
    IfWithin (Expr word) SwordDiffTime (Contract word) (Contract word)
  deriving (Show, Functor)

-- | An 'Expr' is...
data Expr word
  = Add (Expr word) (Expr word)
  | Sub (Expr word) (Expr word)
  | Mul (Expr word) (Expr word)
  | Div (Expr word) (Expr word)
  | Min (Expr word) (Expr word)
  | Max (Expr word) (Expr word)
  | And (Expr word) (Expr word)
  | Or (Expr word) (Expr word)
  | Not (Expr word)
  | If (Expr word) (Expr word) (Expr word)
  | Eq (Expr word) (Expr word)
  | Lt (Expr word) (Expr word)
  | Gt (Expr word) (Expr word)
  | Leq (Expr word) (Expr word)
  | Geq (Expr word) (Expr word)
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
