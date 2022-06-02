{- |
Module     : Agora.SafeMoney
Maintainer : emi@haskell.fyi
Description: Tags and bonuses for Plutarch.SafeMoney.

Tags and extras for "Plutarch.SafeMoney".
-}
module Agora.SafeMoney (
  ADATag,
  GTTag,
  GovernorSTTag,
  StakeSTTag,
  ProposalSTTag,
  adaRef,
) where

--------------------------------------------------------------------------------

import PlutusLedgerApi.V1.Value (AssetClass (AssetClass))

import Data.Tagged (Tagged (Tagged))

--------------------------------------------------------------------------------
-- Tags

-- | Governance token.
data GTTag

-- | ADA.
data ADATag

-- | Governor ST token.
data GovernorSTTag

-- | Stake ST token.
data StakeSTTag

-- | Proposal ST token.
data ProposalSTTag

--------------------------------------------------------------------------------

-- | Resolves ada tags.
adaRef :: Tagged ADATag AssetClass
adaRef = Tagged (AssetClass ("", ""))
