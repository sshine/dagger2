{-# LANGUAGE DeriveFunctor #-}

module Sword.Contract where

import Data.Text (Text)

import Sword.Time

-- | A 'Contract' is...
data Contract word
  = Zero
    -- ^ The 'Zero' contract does nothing.
  | Transfer Asset Party
    -- ^ The 'Transfer' contract moves @asset@ to @party@.
  | Scale (Expr word) (Contract word)
    -- ^ The 'Scale' contract multiplies asset quantities of @contract@.
  | Both (Contract word) (Contract word)
    -- ^ The 'Both' contract executes both of its sub-contracts.
  | Delay SwordDiffTime (Contract word)
    -- ^ The 'Delay' contract executes @contract@ after @time@.
  | IfWithin (Expr word) SwordDiffTime (Contract word) (Contract word)
    -- ^ The 'IfWithin' contract conditionally executes one of its two branches.
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
