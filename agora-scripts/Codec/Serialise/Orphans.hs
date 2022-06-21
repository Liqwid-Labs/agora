{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module     : Codec.Serialise.Orphans
Maintainer : emi@haskell.fyi
Description: Orphan instances for Serialising and Hashing Cardano types.

Orphan instances for Serialising and Hashing Cardano types.
-}
module Codec.Serialise.Orphans () where

import Codec.Serialise (Serialise, serialise)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Tagged (Tagged (Tagged))
import PlutusLedgerApi.V1 (TxId, TxOutRef)
import PlutusLedgerApi.V1.Value (AssetClass, CurrencySymbol, TokenName)
import Types (ScriptParams)

deriving anyclass instance
  Serialise TxOutRef

deriving anyclass instance
  Serialise TxId

deriving anyclass instance
  Serialise AssetClass

deriving anyclass instance
  Serialise CurrencySymbol

deriving anyclass instance
  Serialise TokenName

deriving newtype instance
  Serialise a =>
  Serialise (Tagged s a)

deriving anyclass instance Serialise ScriptParams

instance Hashable ScriptParams where
  hashWithSalt s scriptParams = hashWithSalt s (serialise scriptParams)
