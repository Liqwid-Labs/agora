{- |
Module     : Agora.SafeMoney
Maintainer : emi@haskell.fyi
Description: Tags and bonuses for Plutarch.SafeMoney.

Tags and bonuses for "Plutarch.SafeMoney".
-}
module Agora.SafeMoney (
  ADATag,
  GTTag,
  adaRef,
) where

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Value (AssetClass (AssetClass))

import Plutarch.SafeMoney

--------------------------------------------------------------------------------
-- Example tags

-- | Governance token.
data GTTag

-- | ADA.
data ADATag

--------------------------------------------------------------------------------

-- | Resolves ada tags.
adaRef :: Tagged ADATag AssetClass
adaRef = Tagged (AssetClass ("", ""))
