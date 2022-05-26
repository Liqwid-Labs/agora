{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Agora.Governor
Maintainer : connor@mlabs.city
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

  -- * Utilities
  pgetNextProposalId,
  getNextProposalId,
  governorDatumValid,
) where

--------------------------------------------------------------------------------

import Control.Applicative (Const)
import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

--------------------------------------------------------------------------------

import Agora.Proposal (
  PProposalId (..),
  PProposalThresholds (..),
  ProposalId (ProposalId),
  ProposalThresholds,
 )
import Agora.SafeMoney (GTTag)
import Agora.Utils (tclet)

--------------------------------------------------------------------------------

import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Data.Tagged (Tagged (..))
import Plutarch.Extra.Comonad (pextract)
import Plutarch.TryFrom (PTryFrom (..))
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.SafeMoney (PDiscrete (..))
import Plutarch.Extra.TermCont (pmatchC)

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Api (TxOutRef)
import Plutus.V1.Ledger.Value (AssetClass (..))
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

     Parameters of the governor can also be mutated by an effect.
-}
data GovernorRedeemer
  = -- | Checks that a proposal was created lawfully, and allows it.
    CreateProposal
  | -- | Checks that a SINGLE proposal finished correctly,
    --   and allows minting GATs for each effect script.
    MintGATs
  | -- | Allows effects to mutate the parameters.
    MutateGovernor
  deriving stock (Show, GHC.Generic)

PlutusTx.makeIsDataIndexed
  ''GovernorRedeemer
  [ ('CreateProposal, 0)
  , ('MintGATs, 1)
  , ('MutateGovernor, 2)
  ]

-- | Parameters for creating Governor scripts.
data Governor = Governor
  { gstOutRef :: TxOutRef
  -- ^ Referenced utxo will be spent to mint the GST.
  , gtClassRef :: Tagged GTTag AssetClass
  -- ^ Governance token of the system.
  , maximumCosigners :: Integer
  -- ^ Arbitrary limit for maximum amount of cosigners on a proposal.
  -- See `Agora.Proposal.proposalDatumValid`.
  }
  deriving stock (GHC.Generic)

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
    (PlutusType, PIsData, PDataFields, PEq)
    via PIsDataReprInstances PGovernorDatum

instance PUnsafeLiftDecl PGovernorDatum where type PLifted PGovernorDatum = GovernorDatum
deriving via (DerivePConstantViaData GovernorDatum PGovernorDatum) instance (PConstantDecl GovernorDatum)

-- FIXME: derive this via 'PIsDataReprInstances'
-- Blocked by: PProposalThresholds
instance PTryFrom PData (PAsData PGovernorDatum) where
  type PTryFromExcess PData (PAsData PGovernorDatum) = Const ()

  ptryFrom' d k = k (punsafeCoerce d, ())

-- | Plutarch-level version of 'GovernorRedeemer'.
data PGovernorRedeemer (s :: S)
  = PCreateProposal (Term s (PDataRecord '[]))
  | PMintGATs (Term s (PDataRecord '[]))
  | PMutateGovernor (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PGovernorRedeemer

instance PUnsafeLiftDecl PGovernorRedeemer where type PLifted PGovernorRedeemer = GovernorRedeemer
deriving via (DerivePConstantViaData GovernorRedeemer PGovernorRedeemer) instance (PConstantDecl GovernorRedeemer)

deriving via PAsData (PIsDataReprInstances PGovernorRedeemer) instance PTryFrom PData (PAsData PGovernorRedeemer)

--------------------------------------------------------------------------------

-- | Plutrach version of 'getNextProposalId'.
pgetNextProposalId :: Term s (PProposalId :--> PProposalId)
pgetNextProposalId = phoistAcyclic $ plam $ \(pto -> pid) -> pcon $ PProposalId $ pid + 1

-- | Get next proposal id.
getNextProposalId :: ProposalId -> ProposalId
getNextProposalId (ProposalId pid) = ProposalId $ pid + 1

--------------------------------------------------------------------------------

governorDatumValid :: Term s (PGovernorDatum :--> PBool)
governorDatumValid = phoistAcyclic $
  plam $ \datum -> unTermCont $ do
    thresholds <-
      tcont $
        pletFields @'["execute", "draft", "vote"] $
          pfield @"proposalThresholds" # datum

    PDiscrete execute' <- pmatchC thresholds.execute
    PDiscrete draft' <- pmatchC thresholds.draft
    PDiscrete vote' <- pmatchC thresholds.vote

    execute <- tclet $ pextract # execute'
    draft <- tclet $ pextract # draft'
    vote <- tclet $ pextract # vote'

    pure $
      foldr1
        (#&&)
        [ ptraceIfFalse "Execute threshold is less than or equal to" $ 0 #<= execute
        , ptraceIfFalse "Draft threshold is less than or equal to " $ 0 #<= draft
        , ptraceIfFalse "Vote threshold is less than or equal to " $ 0 #<= vote
        , ptraceIfFalse "Draft threshold is less than vote threshold" $ draft #<= vote
        , ptraceIfFalse "Execute threshold is less than vote threshold" $ vote #< execute
        ]
