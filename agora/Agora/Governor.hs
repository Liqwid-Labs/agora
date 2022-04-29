{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Agora.Governor
Maintainer : emi@haskell.fyi
Description: Governor entity scripts acting as authority of entire system.

Governor entity scripts acting as authority of entire system.
-}
module Agora.Governor (
  -- * Haskell-land
  GovernorDatum (..),
  GovernorRedeemer (..),
  Governor (..),

  -- * Plutarch-land

  -- * Scripts
  governorPolicy,
  governorValidator,
) where

import Agora.Proposal (ProposalId, ProposalThresholds)
import Plutarch.Api.V1 (PMintingPolicy, PValidator)
import PlutusTx qualified

-- | Datum for the Governor script.
data GovernorDatum = GovernorDatum
  { proposalThresholds :: ProposalThresholds
  -- ^ Gets copied over upon creation of a 'Agora.Proposal.ProposalDatum'.
  , nextProposalId :: ProposalId
  -- ^ What tag the next proposal will get upon creating.
  }

PlutusTx.makeIsDataIndexed ''GovernorDatum [('GovernorDatum, 0)]

{- | Redeemer for Governor script. The governor has two primary
     responsibilities:

     1. The gating of Proposal creation.
     2. The gating of minting authority tokens.
-}
data GovernorRedeemer
  = -- | Checks that a proposal was created lawfully, and allows it.
    CreateProposal
  | -- | Checks that a SINGLE proposal finished correctly,
    --   and allows minting GATs for each effect script.
    MintGATs

PlutusTx.makeIsDataIndexed ''GovernorRedeemer [('CreateProposal, 0), ('MintGATs, 1)]

-- | Parameters for creating Governor scripts.
data Governor
  = Governor

--------------------------------------------------------------------------------

-- | Policy for Governors.
governorPolicy :: Governor -> ClosedTerm PMintingPolicy
governorPolicy _ =
  plam $ \_redeemer _ctx' -> P.do
    popaque (pconstant ())

-- | Validator for Governors.
governorValidator :: Governor -> ClosedTerm PValidator
governorValidator _ =
  plam $ \_datum _redeemer _ctx' -> P.do
    popaque (pconstant ())
