
module Sword.EVM.Contract where

import Data.DoubleWord (Word256)

import Sword.Contract
import Sword.Time

type EthereumContract = Contract BlockTimestamp ERC1155 EthereumAddress EthereumExpr
type EthereumExpr = Expr Word256 ChainlinkAggregatorInterface

newtype EthereumAddress = Address Word256
  deriving (Eq, Show)

newtype ERC1155 = ERC1155 EthereumAddress
  deriving (Eq, Show)

newtype ChainlinkAggregatorInterface = ChainlinkAggregatorInterface EthereumAddress
  deriving (Eq, Show)
