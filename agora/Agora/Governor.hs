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
  presolveGovernorRedeemer,
) where

import Agora.Aeson.Orphans ()
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
import Agora.SafeMoney (GTTag, GovernorSTTag)
import Data.Aeson qualified as Aeson
import Data.Tagged (Tagged)
import Generics.SOP qualified as SOP
import Optics.TH (makeFieldLabelsNoPrefix)
import Plutarch.Api.V1.Scripts (PRedeemer)
import Plutarch.Api.V2 (KeyGuarantees (Unsorted), PMap, PScriptPurpose (PSpending), PTxInInfo)
import Plutarch.DataRepr (PDataFields)
import Plutarch.Extra.AssetClass (AssetClass, PAssetClass)
import Plutarch.Extra.Bind (PBind ((#>>=)))
import Plutarch.Extra.Field (pletAll)
import Plutarch.Extra.Function (pflip)
import Plutarch.Extra.Functor (PFunctor (pfmap))
import Plutarch.Extra.IsData (
  DerivePConstantViaDataList (DerivePConstantViaDataList),
  DerivePConstantViaEnum (DerivePConstantEnum),
  EnumIsData (EnumIsData),
  PlutusTypeDataList,
  PlutusTypeEnumData,
  ProductIsData (ProductIsData),
 )
import Plutarch.Extra.Maybe (pjust, pnothing)
import Plutarch.Extra.Record (mkRecordConstr, (.=))
import Plutarch.Extra.ScriptContext (ptryFromRedeemer)
import Plutarch.Extra.Tagged (PTagged)
import Plutarch.Extra.Value (passetClassValueOfT)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import PlutusLedgerApi.V1 (TxOutRef)
import PlutusTx qualified
import "liqwid-plutarch-extra" Plutarch.Extra.List (pfindJust)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pletFieldsC)

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
  , maximumCreatedProposalsPerStake :: Integer
  -- ^ The maximum number of proposals created by any given stakes.
  }
  deriving stock
    ( -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 1.0.0
      SOP.Generic
    )
  deriving
    ( -- | @since 1.0.0
      PlutusTx.ToData
    , -- | @since 1.0.0
      PlutusTx.FromData
    )
    via (ProductIsData GovernorDatum)

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
  deriving anyclass
    ( -- | @since 1.0.0
      Aeson.ToJSON
    , -- | @since 1.0.0
      Aeson.FromJSON
    )

makeFieldLabelsNoPrefix ''Governor

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
             , "maximumCreatedProposalsPerStake" ':= PInteger
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
    , -- | @since 0.2.1
      PShow
    )

-- | @since 0.2.0
instance DerivePlutusType PGovernorDatum where
  type DPTStrat _ = PlutusTypeDataList

-- | @since 0.1.0
instance PUnsafeLiftDecl PGovernorDatum where type PLifted _ = GovernorDatum

-- | @since 0.1.0
deriving via
  (DerivePConstantViaDataList GovernorDatum PGovernorDatum)
  instance
    (PConstantDecl GovernorDatum)

-- | @since 0.1.0
deriving anyclass instance PTryFrom PData (PAsData PGovernorDatum)

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
pgetNextProposalId :: forall (s :: S). Term s (PProposalId :--> PProposalId)
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
pisGovernorDatumValid :: forall (s :: S). Term s (PGovernorDatum :--> PBool)
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

{- | Find the governor input and resolve the corresponding governor redeemer,
     given the assetclass of GST.

     @since 1.0.0
-}
presolveGovernorRedeemer ::
  forall (s :: S).
  Term
    s
    ( PTagged GovernorSTTag PAssetClass
        :--> PBuiltinList PTxInInfo
        :--> PMap 'Unsorted PScriptPurpose PRedeemer
        :--> PMaybe PGovernorRedeemer
    )
presolveGovernorRedeemer = phoistAcyclic $
  plam $ \gstClass inputs redeemers ->
    let governorInputRef =
          pfindJust
            # plam
              ( flip pletAll $ \inputF ->
                  let value = pfield @"value" # inputF.resolved
                      isGovernorInput =
                        passetClassValueOfT
                          # gstClass
                          # value
                          #== 1
                   in pif
                        isGovernorInput
                        (pjust # inputF.outRef)
                        pnothing
              )
            # inputs

        governorScriptPurpose =
          pfmap
            # plam
              ( \ref ->
                  mkRecordConstr
                    PSpending
                    (#_0 .= ref)
              )
            # governorInputRef

        governorRedeemer =
          governorScriptPurpose
            #>>= pflip
            # ptryFromRedeemer @(PAsData PGovernorRedeemer)
            # redeemers
     in pfmap # plam pfromData # governorRedeemer
