{-# LANGUAGE DeriveFunctor #-}

module Dagger.Contract.Traversal where

import qualified Data.Fix as Fix

import Dagger.Contract

countTransfers :: Contract t a p e -> Int
countTransfers = Fix.foldFix $ \case
  Zero -> 0
  Transfer _asset _party -> 1
  Scale _factor c -> c
  Both c1 c2 -> c1 + c2
  Delay _time c -> c
  IfWithin _cond _time c1 c2 -> c1 + c2
  LetParty _ident _party c -> c
  LetExpr _dent _expr c -> c
  LetContract _ident c1 c2 -> c1 + c2
