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
  pisGovernorDatumValid,
) where

import Agora.Proposal (
  PProposalId (PProposalId),
  PProposalThresholds,
  ProposalId (ProposalId),
  ProposalThresholds,
  pisProposalThresholdsValid,
 )
import Agora.Proposal.Time (
  MaxTimeRangeWidth,
  PMaxTimeRangeWidth,
  PProposalTimingConfig,
  ProposalTimingConfig,
  pisMaxTimeRangeWidthValid,
  pisProposalTimingConfigValid,
 )
import Agora.SafeMoney (GTTag)
import Data.Tagged (Tagged)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Extra.IsData (
  DerivePConstantViaEnum (DerivePConstantEnum),
  EnumIsData (EnumIsData),
  PlutusTypeEnumData,
 )
import Plutarch.Extra.TermCont (pletFieldsC)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import PlutusLedgerApi.V1 (TxOutRef)
import PlutusLedgerApi.V1.Value (AssetClass)
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
  , maximumProposalsPerStake :: Integer
  -- ^ The maximum number of unfinished proposals that a stake is allowed to be
  --   associated to.
  }
  deriving stock
    ( -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )

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
  deriving stock
    ( -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    , -- | @since 0.2.0
      Enum
    , -- | @since 0.2.0
      Bounded
    )
  deriving
    ( -- | @since 0.1.0
      PlutusTx.ToData
    , -- | @since 0.1.0
      PlutusTx.FromData
    )
    via (EnumIsData GovernorRedeemer)

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
  deriving stock
    ( -- | @since 0.1.0
      Generic
    , -- | @since 0.2.0
      Show
    )

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
             , "maximumProposalsPerStake" ':= PInteger
             ]
        )
  }
  deriving stock
    ( -- | @since 0.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      PlutusType
    , -- | @since 0.1.0
      PIsData
    , -- | @since 0.1.0
      PDataFields
    , -- | @since 0.1.0
      PEq
    )

-- | @since 0.2.0
instance DerivePlutusType PGovernorDatum where
  type DPTStrat _ = PlutusTypeData

-- | @since 0.1.0
instance PUnsafeLiftDecl PGovernorDatum where type PLifted PGovernorDatum = GovernorDatum

-- | @since 0.1.0
deriving via (DerivePConstantViaData GovernorDatum PGovernorDatum) instance (PConstantDecl GovernorDatum)

-- | @since 0.1.0
deriving anyclass instance PTryFrom PData PGovernorDatum

{- | Plutarch-level version of 'GovernorRedeemer'.

     @since 0.1.0
-}
data PGovernorRedeemer (s :: S)
  = PCreateProposal
  | PMintGATs
  | PMutateGovernor
  deriving stock
    ( -- | @since 0.1.0
      Generic
    , -- | @since 0.2.0
      Enum
    , -- | @since 0.2.0
      Bounded
    )
  deriving anyclass
    ( -- | @since 0.1.0
      PlutusType
    , -- | @since 0.1.0
      PIsData
    , -- | @since 0.2.0
      PEq
    )

-- | @since 0.2.0
instance PTryFrom PData (PAsData PGovernorRedeemer)

-- | @since 0.2.0
instance DerivePlutusType PGovernorRedeemer where
  type DPTStrat _ = PlutusTypeEnumData

-- | @since 0.1.0
instance PUnsafeLiftDecl PGovernorRedeemer where type PLifted PGovernorRedeemer = GovernorRedeemer

-- | @since 0.1.0
deriving via (DerivePConstantViaEnum GovernorRedeemer PGovernorRedeemer) instance (PConstantDecl GovernorRedeemer)

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
pisGovernorDatumValid :: Term s (PGovernorDatum :--> PBool)
pisGovernorDatumValid = phoistAcyclic $
  plam $ \datum -> unTermCont $ do
    datumF <-
      pletFieldsC
        @'[ "proposalThresholds"
          , "proposalTimings"
          , "createProposalTimeRangeMaxWidth"
          ]
        datum

    pure $
      foldr1
        (#&&)
        [ ptraceIfFalse "thresholds valid" $
            pisProposalThresholdsValid # pfromData datumF.proposalThresholds
        , ptraceIfFalse "timings valid" $
            pisProposalTimingConfigValid # pfromData datumF.proposalTimings
        , ptraceIfFalse "time range valid" $
            pisMaxTimeRangeWidthValid # datumF.createProposalTimeRangeMaxWidth
        ]
