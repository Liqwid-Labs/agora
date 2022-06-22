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

import Agora.Proposal (
  PProposalId (..),
  PProposalThresholds (..),
  ProposalId (ProposalId),
  ProposalThresholds,
 )
import Agora.Proposal.Time (
  MaxTimeRangeWidth,
  PMaxTimeRangeWidth,
  PProposalTimingConfig,
  ProposalTimingConfig,
 )
import Agora.SafeMoney (GTTag)
import Data.Tagged (Tagged (..))
import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Extra.Comonad (pextract)
import Plutarch.Extra.TermCont (pletC, pmatchC)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.SafeMoney (PDiscrete (..))
import PlutusLedgerApi.V1 (TxOutRef)
import PlutusLedgerApi.V1.Value (AssetClass (..))
import PlutusTx qualified

--------------------------------------------------------------------------------

{- | Datum for the Governor script.

     @since 0.1.0
-}
data GovernorDatum = GovernorDatum
  { proposalThresholds :: ProposalThresholds
  -- ^ Gets copied over upon creation of a 'Agora.Proposal.ProposalDatum'.
  , nextProposalId :: ProposalId
  -- ^ What tag the next proposal will get upon creating.
  , proposalTimings :: ProposalTimingConfig
  -- ^ The timing configuration for proposals.
  --   Will get copied over upon the creation of proposals.
  , createProposalTimeRangeMaxWidth :: MaxTimeRangeWidth
  -- ^ The maximum valid duration of a transaction that creats a proposal.
  }
  deriving stock (Show, GHC.Generic)

-- | @since 0.1.0
PlutusTx.makeIsDataIndexed ''GovernorDatum [('GovernorDatum, 0)]

{- | Redeemer for Governor script. The governor has two primary
     responsibilities:

     1. The gating of Proposal creation.
     2. The gating of minting authority tokens.

     Parameters of the governor can also be mutated by an effect.

     @since 0.1.0
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

-- | @since 0.1.0
PlutusTx.makeIsDataIndexed
  ''GovernorRedeemer
  [ ('CreateProposal, 0)
  , ('MintGATs, 1)
  , ('MutateGovernor, 2)
  ]

{- | Parameters for creating Governor scripts.

     @since 0.1.0
-}
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

{- | Plutarch-level datum for the Governor script.

     @since 0.1.0
-}
newtype PGovernorDatum (s :: S) = PGovernorDatum
  { getGovernorDatum ::
      Term
        s
        ( PDataRecord
            '[ "proposalThresholds" ':= PProposalThresholds
             , "nextProposalId" ':= PProposalId
             , "proposalTimings" ':= PProposalTimingConfig
             , "createProposalTimeRangeMaxWidth" ':= PMaxTimeRangeWidth
             ]
        )
  }
  deriving stock
    ( -- | @since 0.1.0
      GHC.Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      PIsDataRepr
    )
  deriving
    ( -- | @since 0.1.0
      PlutusType
    , -- | @since 0.1.0
      PIsData
    , -- | @since 0.1.0
      PDataFields
    , -- | @since 0.1.0
      PEq
    )
    via PIsDataReprInstances PGovernorDatum

-- | @since 0.1.0
instance PUnsafeLiftDecl PGovernorDatum where type PLifted PGovernorDatum = GovernorDatum

-- | @since 0.1.0
deriving via (DerivePConstantViaData GovernorDatum PGovernorDatum) instance (PConstantDecl GovernorDatum)

-- | @since 0.1.0
deriving via PAsData (PIsDataReprInstances PGovernorDatum) instance PTryFrom PData (PAsData PGovernorDatum)

{- | Plutarch-level version of 'GovernorRedeemer'.

     @since 0.1.0
-}
data PGovernorRedeemer (s :: S)
  = PCreateProposal (Term s (PDataRecord '[]))
  | PMintGATs (Term s (PDataRecord '[]))
  | PMutateGovernor (Term s (PDataRecord '[]))
  deriving stock
    ( -- | @since 0.1.0
      GHC.Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      PIsDataRepr
    )
  deriving
    ( -- | @since 0.1.0
      PlutusType
    , -- | @since 0.1.0
      PIsData
    )
    via PIsDataReprInstances PGovernorRedeemer

-- | @since 0.1.0
instance PUnsafeLiftDecl PGovernorRedeemer where type PLifted PGovernorRedeemer = GovernorRedeemer

-- | @since 0.1.0
deriving via (DerivePConstantViaData GovernorRedeemer PGovernorRedeemer) instance (PConstantDecl GovernorRedeemer)

-- | @since 0.1.0
deriving via PAsData (PIsDataReprInstances PGovernorRedeemer) instance PTryFrom PData (PAsData PGovernorRedeemer)

--------------------------------------------------------------------------------

{- | Plutrach version of 'getNextProposalId'.

     @since 0.1.0
-}
pgetNextProposalId :: Term s (PProposalId :--> PProposalId)
pgetNextProposalId = phoistAcyclic $ plam $ \(pto -> pid) -> pcon $ PProposalId $ pid + 1

{- | Get next proposal id.

     @since 0.1.0
-}
getNextProposalId :: ProposalId -> ProposalId
getNextProposalId (ProposalId pid) = ProposalId $ pid + 1

--------------------------------------------------------------------------------

{- | Check whether a particular 'PGovernorDatum' is well-formed.

     @since 0.1.0
-}
governorDatumValid :: Term s (PGovernorDatum :--> PBool)
governorDatumValid = phoistAcyclic $
  plam $ \datum -> unTermCont $ do
    thresholds <-
      tcont $
        pletFields @'["execute", "create", "vote"] $
          pfield @"proposalThresholds" # datum

    PDiscrete execute' <- pmatchC thresholds.execute
    PDiscrete draft' <- pmatchC thresholds.create
    PDiscrete vote' <- pmatchC thresholds.vote

    execute <- pletC $ pextract # execute'
    draft <- pletC $ pextract # draft'
    vote <- pletC $ pextract # vote'

    pure $
      foldr1
        (#&&)
        [ ptraceIfFalse "Execute threshold is less than or equal to" $ 0 #<= execute
        , ptraceIfFalse "Draft threshold is less than or equal to " $ 0 #<= draft
        , ptraceIfFalse "Vote threshold is less than or equal to " $ 0 #<= vote
        , ptraceIfFalse "Draft threshold is less than vote threshold" $ draft #<= vote
        , ptraceIfFalse "Execute threshold is less than vote threshold" $ vote #< execute
        ]
