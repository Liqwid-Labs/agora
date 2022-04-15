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
  PGovernorDatum (..),
  PGovernorRedeemer (..),

  -- * Scripts
  governorPolicy,
  governorValidator,
) where

--------------------------------------------------------------------------------

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

--------------------------------------------------------------------------------

import Agora.Proposal (PProposalId, PProposalThresholds, ProposalId, ProposalThresholds)
import Agora.Utils (findOutputsToAddress, passert, passetClassValueOf', pfindTxInByTxOutRef, findTxOutDatum)

--------------------------------------------------------------------------------

import Plutarch (popaque)
import Plutarch.Api.V1 (PMaybeData (PDJust), PMintingPolicy, PScriptPurpose (PSpending), PValidator, PValue)
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Lift (PUnsafeLiftDecl (..))
import Plutarch.Monadic qualified as P

--------------------------------------------------------------------------------

import Plutarch.Unsafe (punsafeCoerce)
import Plutus.V1.Ledger.Value (AssetClass)
import PlutusTx qualified

--------------------------------------------------------------------------------

-- | Datum for the Governor script.
data GovernorDatum = GovernorDatum
  { proposalThresholds :: ProposalThresholds
  -- ^ Gets copied over upon creation of a 'Agora.Proposal.ProposalDatum'.
  , nextProposalId :: ProposalId
  -- ^ What tag the next proposal will get upon creating.
  }
  deriving stock (Show, GHC.Generic)

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
  | -- | Allows effects to mutate the datum.
    MutateDatum
  deriving stock (Show, GHC.Generic)

PlutusTx.makeIsDataIndexed
  ''GovernorRedeemer
  [ ('CreateProposal, 0)
  , ('MintGATs, 1)
  , ('MutateDatum, 2)
  ]

-- | Parameters for creating Governor scripts.
data Governor = Governor
  { datumNFT :: AssetClass
  -- ^ NFT that identifies the governor datum.
  }

--------------------------------------------------------------------------------

-- | Plutarch-level datum for the Governor script.
newtype PGovernorDatum (s :: S) = PGovernorDatum
  { getGovernorDatum ::
    Term
      s
      ( PDataRecord
          '[ "proposalThresholds" ':= PProposalThresholds
           , "nextProposalId" ':= PProposalId
           ]
      )
  }
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PGovernorDatum

instance PUnsafeLiftDecl PGovernorDatum where type PLifted PGovernorDatum = GovernorDatum
deriving via (DerivePConstantViaData GovernorDatum PGovernorDatum) instance (PConstant GovernorDatum)

-- | Plutarch-level version of 'GovernorRedeemer'.
data PGovernorRedeemer (s :: S)
  = PCreateProposal (Term s (PDataRecord '[]))
  | PMintGATs (Term s (PDataRecord '[]))
  | PMutateDatum (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PGovernorRedeemer

instance PUnsafeLiftDecl PGovernorRedeemer where type PLifted PGovernorRedeemer = GovernorRedeemer
deriving via (DerivePConstantViaData GovernorRedeemer PGovernorRedeemer) instance (PConstant GovernorRedeemer)

--------------------------------------------------------------------------------

-- | Policy for Governors.
governorPolicy :: Governor -> ClosedTerm PMintingPolicy
governorPolicy _ =
  plam $ \_redeemer _ctx' -> P.do
    popaque (pconstant ())

-- | Validator for Governors.
governorValidator :: Governor -> ClosedTerm PValidator
governorValidator params =
  plam $ \datum' redeemer' ctx' -> P.do
    -- TODO: use `PTryFrom`
    redeemer <- pmatch $ pfromData @PGovernorRedeemer $ punsafeCoerce redeemer'
    ctx <- pletFields @'["txInfo", "purpose"] ctx'

    txInfo <- plet $ pfromData $ ctx.txInfo

    PSpending ((pfield @"_0" #) -> txOutRef') <- pmatch $ pfromData ctx.purpose
    let txOutRef = pfromData txOutRef'

    PJust ((pfield @"resolved" #) -> ownInput') <- pmatch $ pfindTxInByTxOutRef # txOutRef # txInfo
    ownInput <- pletFields @'["address", "value", "datumHash"] ownInput'
    let selfAddress = pfromData $ ownInput.address
        
    PJust (((pfromData @PGovernorDatum) . punsafeCoerce) -> oldDatum') <-  pmatch $ findTxOutDatum # txInfo # ownInput'
    oldDatum <- pletFields @'["proposalThresholds", "nextProposalId"] oldDatum'

    let ownInputDatumNFTAmount = datumNFTValueOf # ownInput.value
    passert "own input should have exactly one datum NFT" $ ownInputDatumNFTAmount #== 1
    
    ownOutputs <- plet $ findOutputsToAddress # txInfo # selfAddress
    passert "exactly one utxo should be sent to the governor" $ plength # ownOutputs #== 1

    ownOutput <- pletFields @'["value", "datumHash"] $ phead # ownOutputs
    let ownOuputDatumNFTAmount = datumNFTValueOf # ownOutput.value
    passert "datum NFT should stay at governor's address" $ ownOuputDatumNFTAmount #== 1
    passert "output utxo to governor should have datum" $ pisDJust # ownOutput.datumHash

    let newDatum' = pfromData @PGovernorDatum $ punsafeCoerce datum'
    newDatum <- pletFields @'["proposalThresholds", "nextProposalId"] newDatum'

    case redeemer of
      PCreateProposal _ -> P.do
        -- TODO: deriving a PNum instance for PProposalId
        let oldPid = pto $ pfromData $ oldDatum.nextProposalId
            newPid = pto $ pfromData $ newDatum.nextProposalId        
        passert "proposal id should be advanced by 1" $ oldPid + 1 #== newPid
        
        ptraceError "not implemented yet"
      PMintGATs _ -> perror
      PMutateDatum _ -> perror
  where
    datumNFTValueOf :: Term s (PValue :--> PInteger)
    datumNFTValueOf = passetClassValueOf' params.datumNFT

pisDJust :: Term s (PMaybeData a :--> PBool)
pisDJust = phoistAcyclic $
  plam $ \x ->
    pmatch
      x
      ( \case
          PDJust _ -> pconstant True
          _ -> pconstant False
      )