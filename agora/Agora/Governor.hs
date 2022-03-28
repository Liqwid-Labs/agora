{- |
Module     : Agora.Governor
Maintainer : emi@haskell.fyi
Description: Governor entity scripts acting as authority of entire system.

Governor entity scripts acting as authority of entire system.
-}
module Agora.Governor (GovernorDatum (..), GovernorRedeemer (..)) where

import Agora.Proposal (ProposalThresholds)

data GovernorDatum = GovernorDatum
  { proposalThresholds :: ProposalThresholds
  -- ^ Gets copied over upon creation of a 'Proposal'.
  }

{- | Redeemer for Governor script.

     The governor has two primary responsibilities:
     - The gating of Proposal creation
     - The gating of minting authority tokens
-}
data GovernorRedeemer
  = -- | Checks that a proposal was created lawfully, and allows it.
    CreateProposal
  | -- | Checks that a SINGLE proposal finished correctly,
    --   and allows minting GATs for each effect script.
    MintGATs
