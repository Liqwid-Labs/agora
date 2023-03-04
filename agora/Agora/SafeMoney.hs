{- |
Module     : Agora.SafeMoney
Maintainer : emi@haskell.fyi
Description: Tags and extras for "Plutarch.SafeMoney".

Tags and extras for "Plutarch.SafeMoney".
-}
module Agora.SafeMoney (
  ADATag,
  GTTag,
  GovernorSTTag,
  StakeSTTag,
  ProposalSTTag,
  AuthorityTokenTag,
  adaRef,
) where

import Data.Tagged (Tagged (Tagged))
import PlutusLedgerApi.V1.Value (AssetClass (AssetClass))

{- | Governance token.

     @since 0.1.0
-}
type GTTag = "GTTag"

{- | ADA.

     @since 0.1.0
-}
type ADATag = "ADATag"

{- | Governor ST token.

     @since 0.1.0
-}
type GovernorSTTag = "GovernorSTTag"

{- | Stake ST token.

     @since 0.1.0
-}
type StakeSTTag = "StakeSTTag"

{- | Proposal ST token.

     @since 0.1.0
-}
type ProposalSTTag = "ProposalSTTag"

{- | Authority token.

     @since 1.0.0
-}
type AuthorityTokenTag = "AuthorityTokenTag"

{- | Resolves ada tags.

     @since 0.1.0
-}
adaRef :: Tagged ADATag AssetClass
adaRef = Tagged (AssetClass ("", ""))
