{-# LANGUAGE DeriveFunctor #-}

module Sword.Contract where

import Data.Fix (Fix)
import Data.Text (Text)

-- | A 'Contract'' is parameterised over @time@, @asset@, @party@ and @expr@.
--
-- Different blockchains handle these differently:
--
--   * In Solidity on Ethereum, one may either use @block.timestamp@ and accept
--     the imprecision of an average block mining length, or use @block.number@
--     which is less ambiguous but also less ergonomic.
--
--   * Depending on the blockchain, the purpose of a contract, and the backend,
--     assets can follow one of many token standards; for example if the asset
--     is fungible or non-fungible, if asset contracts represent multiple token
--     types, or if assets represent subscriptions. On Ethereum, popular ones
--     include ERC20, ERC223, ERC721, ERC777, ERC1155 and ERC1337:
--     https://crushcrypto.com/ethereum-erc-token-standards/
--
--   * Sword contracts may be deployed in many ways. Depending on the way that
--     contract parties are handled, they can either be compile-time hardcoded
--     addresses, or some type that denotes a placeholder entered at runtime
--     before activating the contract.
--
--   * See 'Expr'' for the blockchain-specific variations here.
--
data Contract' time asset party expr contract
  = Zero                                  -- ^ The 'Zero' contract does nothing.
  | Transfer asset party                  -- ^ The 'Transfer' contract moves @asset@ to @party@.
  | Scale expr contract                   -- ^ The 'Scale' contract multiplies asset quantities of @contract@.
  | Both contract contract                -- ^ The 'Both' contract executes both of its sub-contracts.
  | Delay time contract                   -- ^ The 'Delay' contract executes @contract@ after @time@.
  | IfWithin expr time contract contract
  | LetParty Ident party contract
  | LetExpr Ident expr contract
  | LetContract Ident contract contract
  deriving (Show, Functor)

-- | An 'Expr'' is parameterised over @word@ and @oracle@.
--
-- Different blockchains have different constraints for expressions:
--
--   * Word sizes and overflow semantics
--
--   * Oracles have different interfaces: On Ethereum the dominant oracle
--     provider is ChainLink, which provides different interfaces for their
--     oracles. On Tezos, oracles are expected to follow an asynchronous
--     callback model.
--
data Expr' word oracle expr
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

  | Var Ident
  | Const word
  | Oracle oracle
  deriving (Show, Functor)

-- | Variable identifier
type Ident = Text

-- | A 'Contract' represents the full syntax tree of contracts.
type Contract time asset party expr =
  Fix (Contract' time asset party expr)

-- | An 'Expr' represents the full syntax tree of expressions.
type Expr word oracle =
  Fix (Expr' word oracle)
