{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Agora.Proposal
Maintainer : emi@haskell.fyi
Description: Proposal scripts encoding effects that operate on the system.

Proposal scripts encoding effects that operate on the system.
-}
module Agora.Proposal (
  -- * Haskell-land
  ProposalEffectMetadata (..),
  ProposalEffectGroup,
  ProposalDatum (..),
  ProposalRedeemer (..),
  ProposalStatus (..),
  ProposalThresholds (..),
  ProposalVotes (..),
  ProposalId (..),
  ResultTag (..),
  emptyVotesFor,

  -- * Plutarch-land
  PProposalEffectMetadata (..),
  PProposalEffectGroup,
  PProposalDatum (..),
  PProposalRedeemer (..),
  PProposalStatus (..),
  PProposalThresholds (..),
  PProposalVotes (..),
  PProposalId (..),
  PResultTag (..),

  -- * Plutarch helpers
  phasNeutralEffect,
  pisEffectsVotesCompatible,
  pisVotesEmpty,
  pwinner,
  pwinner',
  pneutralOption,
  pretractVotes,
  pisProposalThresholdsValid,
) where

import Agora.Plutarch.Orphans ()
import Agora.Proposal.Time (
  PProposalStartingTime,
  PProposalTimingConfig,
  ProposalStartingTime,
  ProposalTimingConfig,
 )
import Agora.SafeMoney (GTTag)
import Data.Map.Strict qualified as StrictMap
import Data.Tagged (Tagged)
import Generics.SOP qualified as SOP
import Plutarch.Api.V1 (PCredential, PMap, PValidatorHash)
import Plutarch.Api.V1.AssocMap qualified as PAssocMap
import Plutarch.Api.V2 (
  KeyGuarantees (Sorted),
  PDatumHash,
  PMaybeData,
  PScriptHash,
 )
import Plutarch.DataRepr (
  DerivePConstantViaData (
    DerivePConstantViaData
  ),
  PDataFields,
 )
import Plutarch.Extra.Comonad (pextract)
import Plutarch.Extra.Field (pletAllC)
import Plutarch.Extra.Function (pbuiltinUncurry)
import Plutarch.Extra.IsData (
  DerivePConstantViaDataList (DerivePConstantViaDataList),
  DerivePConstantViaEnum (DerivePConstantEnum),
  EnumIsData (EnumIsData),
  PlutusTypeDataList,
  PlutusTypeEnumData,
  ProductIsData (ProductIsData),
 )
import Plutarch.Extra.List (pfirstJust)
import Plutarch.Extra.Map qualified as PM
import Plutarch.Extra.Maybe (pfromJust)
import Plutarch.Extra.TermCont (pguardC, pletC, pmatchC)
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl,
  PUnsafeLiftDecl (type PLifted),
 )
import Plutarch.Orphans ()
import Plutarch.SafeMoney (PDiscrete (PDiscrete))
import PlutusLedgerApi.V2 (Credential, DatumHash, ScriptHash, ValidatorHash)
import PlutusTx qualified

--------------------------------------------------------------------------------
-- Haskell-land

{- | Identifies a Proposal, issued upon creation of a proposal. In practice,
     this number starts at zero, and increments by one for each proposal.
     The 100th proposal will be @'ProposalId' 99@. This counter lives
     in the 'Agora.Governor.Governor'. See 'Agora.Governor.nextProposalId', and
     'Agora.Governor.pgetNextProposalId'.

     @since 0.1.0
-}
newtype ProposalId = ProposalId {proposalTag :: Integer}
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )
  deriving newtype
    ( -- | @since 0.1.0
      PlutusTx.ToData
    , -- | @since 0.1.0
      PlutusTx.FromData
    , -- | @since 0.1.0
      PlutusTx.UnsafeFromData
    )

{- | Encodes a result. Typically, for a Yes/No proposal, we encode it like this:

     @
     "No"  ~ 'ResultTag' 0
     "Yes" ~ 'ResultTag' 1
     @

     @since 0.1.0
-}
newtype ResultTag = ResultTag {getResultTag :: Integer}
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Ord
    , -- | @since 0.1.0
      Generic
    )
  deriving newtype
    ( -- | @since 0.1.0
      PlutusTx.ToData
    , -- | @since 0.1.0
      PlutusTx.FromData
    , -- | @since 0.1.0
      PlutusTx.UnsafeFromData
    )

{- | The "status" of the proposal. This is only useful for state transitions that
     need to happen as a result of a transaction as opposed to time-based "periods".

     See the note on wording & the state machine in the tech-design.

     If the proposal is 'VotingReady', for instance, that doesn't necessarily
     mean that voting is possible, as this also requires the timing to be right.

     @since 0.1.0
-}
data ProposalStatus
  = -- | A draft proposal represents a proposal that has yet to be realized.
    --
    --   In effect, this means one which didn't have enough LQ to be a full
    --   proposal, and needs cosigners to enable that to happen. This is
    --   similar to a "temperature check", but only useful if multiple people
    --   want to pool governance tokens together. If the proposal doesn't get to
    --   'VotingReady' on time, the proposal will __never__ be able to get
    --   voted on.
    Draft
  | -- | The proposal has/had enough GT cosigned in order to be a fully fledged
    --   proposal.
    --
    --   This means that once the timing requirements align,
    --   proposal will be able to be voted on.
    VotingReady
  | -- | The proposal has been voted on, and the votes have been locked
    --   permanently. The proposal now goes into a locking time after the
    --   normal voting time. After this, it's possible to execute the proposal.
    Locked
  | -- | The proposal has finished.
    --
    --   This can mean it's been voted on and completed, but it can also mean
    --   the proposal failed due to time constraints or didn't
    --   get to 'VotingReady' first.
    --
    --   At this stage, the 'votes' field of 'ProposalDatum' is frozen.
    --
    --   See 'AdvanceProposal' for documentation on state transitions.
    --
    --   TODO: The owner of the proposal may choose to reclaim their proposal.
    Finished
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    , -- | @since 0.2.0
      Enum
    , -- | @since 0.2.0
      Bounded
    )
  deriving anyclass
    ( -- | @since 0.2.0
      SOP.Generic
    )
  deriving
    ( -- | @since 0.1.0
      PlutusTx.FromData
    , -- | @since 0.1.0
      PlutusTx.ToData
    , -- | @since 0.1.0
      PlutusTx.UnsafeFromData
    )
    via (EnumIsData ProposalStatus)

{- | The threshold values for various state transitions to happen.
     This data is stored centrally (in the 'Agora.Governor.Governor') and copied over
     to 'Proposal's when they are created.

     @since 0.1.0
-}
data ProposalThresholds = ProposalThresholds
  { execute :: Tagged GTTag Integer
  -- ^ How much GT minimum must a particular 'ResultTag' accumulate for it to pass.
  , create :: Tagged GTTag Integer
  -- ^ How much GT required to "create" a proposal.
  --
  -- It is recommended this be a high enough amount, in order to prevent DOS from bad
  -- actors.
  , vote :: Tagged GTTag Integer
  -- ^ How much GT required to allow voting to happen.
  -- (i.e. to move into 'VotingReady')
  }
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )

PlutusTx.makeIsDataIndexed 'ProposalThresholds [('ProposalThresholds, 0)]

{- | Map which encodes the total tally for each result.
     It's important that the "shape" is consistent with the shape of 'effects'.

     e.g. if the 'effects' field looks like the following:

     @[('ResultTag' 0, []), ('ResultTag' 1, [(vh, dh)])]@

     Then 'ProposalVotes' needs be of the shape:

     @[('ResultTag' 0, n), ('ResultTag' 1, m)]@

     @since 0.1.0
-}
newtype ProposalVotes = ProposalVotes
  { getProposalVotes :: StrictMap.Map ResultTag Integer
  }
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )
  deriving newtype
    ( -- | @since 0.1.0
      PlutusTx.ToData
    , -- | @since 0.1.0
      PlutusTx.FromData
    )

{- | Create a 'ProposalVotes' that has the same shape as the 'effects' field.

     @since 0.1.0
-}
emptyVotesFor :: forall a. StrictMap.Map ResultTag a -> ProposalVotes
emptyVotesFor = ProposalVotes . StrictMap.mapWithKey (const . const 0)

-- | @since 1.0.0
data ProposalEffectMetadata = ProposalEffectMetadata
  { datumHash :: DatumHash
  -- ^ Hash of datum sent to effect validator with GAT
  , scriptHash :: Maybe ScriptHash
  -- ^ A 'ScriptHash' that encodes the authority script.
  }
  deriving stock
    ( -- | @since 1.0.0
      Generic
    , -- | @since 1.0.0
      Show
    , -- | @since 1.0.0
      Eq
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
    via (ProductIsData ProposalEffectMetadata)

-- | @since 1.0.0
type ProposalEffectGroup = StrictMap.Map ValidatorHash ProposalEffectMetadata

{- | Haskell-level datum for Proposal scripts.

     @since 0.1.0
-}
data ProposalDatum = ProposalDatum
  { proposalId :: ProposalId
  -- ^ Identification of the proposal. Note that this map should be sorted in
  --    ascending order, and its keys should be unique.
  --
  -- TODO: could we encode this more efficiently?
  -- This is shaped this way for future proofing.
  -- See https://github.com/Liqwid-Labs/agora/issues/39
  , effects :: StrictMap.Map ResultTag ProposalEffectGroup
  -- ^ Effect lookup table. First by result, then by effect hash.
  , status :: ProposalStatus
  -- ^ The status the proposal is in.
  , cosigners :: [Credential]
  -- ^ Who created the proposal initially, and who cosigned it later.
  --
  -- This list should be sorted in **ascending** order.
  , thresholds :: ProposalThresholds
  -- ^ Thresholds copied over on initialization.
  , votes :: ProposalVotes
  -- ^ Vote tally on the proposal
  , timingConfig :: ProposalTimingConfig
  -- ^ Timing configuration copied over on initialization.
  , startingTime :: ProposalStartingTime
  -- ^ The time upon the creation of the proposal.
  }
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 0.2.0
      SOP.Generic
    )
  deriving
    ( -- | @since 0.1.0
      PlutusTx.ToData
    , -- | @since 0.1.0
      PlutusTx.FromData
    )
    via (ProductIsData ProposalDatum)

{- | Haskell-level redeemer for Proposal scripts.

     @since 0.1.0
-}
data ProposalRedeemer
  = -- | Cast one or more votes towards a particular 'ResultTag'.
    Vote ResultTag
  | -- | Add one or more public keys to the cosignature list.
    --   Must be signed by those cosigning.
    --
    --   This is particularly used in the 'Draft' 'ProposalStatus',
    --   where matching 'Agora.Stake.Stake's can be called to advance the proposal,
    --   provided enough GT is shared among them.
    --
    --   This list should be sorted in ascending order.
    Cosign [Credential]
  | -- | Allow unlocking one or more stakes with votes towards particular 'ResultTag'.
    Unlock
  | -- | Advance the proposal, performing the required checks for whether that is legal.
    --
    --   These are roughly the checks for each possible transition:
    --
    --   === @'Draft' -> 'VotingReady'@:
    --
    --     1. The sum of all of the cosigner's GT is larger than the 'vote' field of 'ProposalThresholds'.
    --     2. The proposal's current time ensures 'isDraftPeriod'.
    --
    --   === @'VotingReady' -> 'Locked'@:
    --
    --     1. The sum of all votes is larger than 'execute'.
    --     2. The winning 'ResultTag' has more votes than all other 'ResultTag's.
    --     3. The proposal's current time ensures 'isVotingPeriod'.
    --
    --   === @'Locked' -> 'Finished'@:
    --
    --     1. The proposal's current time ensures 'isExecutionPeriod'.
    --     2. The transaction mints the GATs to the receiving effects.
    --
    --   === @* -> 'Finished'@:
    --
    --     If the proposal has run out of time for the current 'ProposalStatus', it will always be possible
    --     to transition into 'Finished' status, because it has expired (and failed).
    AdvanceProposal
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )

-- | @since 0.1.0
PlutusTx.makeIsDataIndexed
  ''ProposalRedeemer
  [ ('Vote, 0)
  , ('Cosign, 1)
  , ('Unlock, 2)
  , ('AdvanceProposal, 3)
  ]

--------------------------------------------------------------------------------
-- Plutarch-land

{- | Plutarch-level version of 'ResultTag'.

     @since 0.1.0
-}
newtype PResultTag (s :: S) = PResultTag (Term s PInteger)
  deriving stock
    ( -- | @since 0.2.0
      Generic
    )
  deriving anyclass
    ( -- @since 0.1.0
      PlutusType
    , -- | @since 0.1.0
      PIsData
    , -- | @since 0.1.0
      PEq
    , -- | @since 0.2.0
      PPartialOrd
    , -- | @since 0.1.0
      POrd
    , -- | @since 0.2.0
      PShow
    )

-- | @since 0.2.0
instance DerivePlutusType PResultTag where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 0.1.0
instance PTryFrom PData (PAsData PResultTag)

-- | @since 0.1.0
instance PUnsafeLiftDecl PResultTag where type PLifted PResultTag = ResultTag

-- | @since 0.1.0
deriving via
  (DerivePConstantViaNewtype ResultTag PResultTag PInteger)
  instance
    (PConstantDecl ResultTag)

{- | Plutarch-level version of 'PProposalId'.

     @since 0.1.0
-}
newtype PProposalId (s :: S) = PProposalId (Term s PInteger)
  deriving stock
    ( -- | @since 0.2.0
      Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      PlutusType
    , -- | @since 0.1.0
      PIsData
    , -- | @since 0.1.0
      PEq
    , -- | @since 0.2.0
      PPartialOrd
    , -- | @since 0.1.0
      POrd
    , -- | @since 0.2.0
      PShow
    )

-- | @since 0.2.0
instance DerivePlutusType PProposalId where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 0.1.0
instance PTryFrom PData (PAsData PProposalId)

-- | @since 0.1.0
instance PUnsafeLiftDecl PProposalId where type PLifted PProposalId = ProposalId

-- | @since 0.1.0
deriving via
  (DerivePConstantViaNewtype ProposalId PProposalId PInteger)
  instance
    (PConstantDecl ProposalId)

{- | Plutarch-level version of 'ProposalStatus'.

     @since 0.1.0
-}
data PProposalStatus (s :: S)
  = -- | @since 0.2.0
    PDraft
  | -- | @since 1.0.0
    PVotingReady
  | -- | @since 0.2.0
    PLocked
  | -- | @since 0.2.0
    PFinished
  deriving stock
    ( -- | @since 0.1.0
      Generic
    , -- | @since 0.2.0
      Bounded
    , -- | @since 0.2.0
      Enum
    )
  deriving anyclass
    ( -- | @since 0.1.0
      PlutusType
    , -- | @since 0.1.0
      PIsData
    , -- | @since 0.1.0
      PEq
    )

-- | @since 0.2.0
instance DerivePlutusType PProposalStatus where
  type DPTStrat _ = PlutusTypeEnumData

-- | @since 0.1.0
instance PUnsafeLiftDecl PProposalStatus where type PLifted PProposalStatus = ProposalStatus

-- | @since 0.1.0
instance PTryFrom PData (PAsData PProposalStatus)

-- | @since 0.1.0
deriving via (DerivePConstantViaEnum ProposalStatus PProposalStatus) instance (PConstantDecl ProposalStatus)

{- | Plutarch-level version of 'ProposalThresholds'.

     @since 0.1.0
-}
newtype PProposalThresholds (s :: S) = PProposalThresholds
  { getProposalThresholds ::
      Term
        s
        ( PDataRecord
            '[ "execute" ':= PDiscrete GTTag
             , "create" ':= PDiscrete GTTag
             , "vote" ':= PDiscrete GTTag
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
    )

-- | @since 0.2.0
instance DerivePlutusType PProposalThresholds where
  type DPTStrat _ = PlutusTypeData

-- | @since 0.1.0
instance PTryFrom PData PProposalThresholds

-- | @since 0.1.0
instance PUnsafeLiftDecl PProposalThresholds where type PLifted PProposalThresholds = ProposalThresholds

-- | @since 0.1.0
deriving via
  (DerivePConstantViaData ProposalThresholds PProposalThresholds)
  instance
    (PConstantDecl ProposalThresholds)

{- | Plutarch-level version of 'ProposalVotes'.

     Note: we don't really need this map to be ordered on chain, the purpose of
     tagging it as sorted is to ensure the uniqueness of the keys. This
     introduces some performance overhead cause sortness is unnecessarily
     checked every time we try to recover a `PPropopsalVotes` from `PData`.

     FIXME(Connor): optimize away this.

     @since 0.1.0
-}
newtype PProposalVotes (s :: S)
  = PProposalVotes (Term s (PMap 'Sorted PResultTag PInteger))
  deriving stock
    ( -- | @since 0.2.0
      Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      PlutusType
    , -- | @since 0.1.0
      PIsData
    )

-- | @since 0.2.0
instance DerivePlutusType PProposalVotes where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 0.1.0
instance PTryFrom PData (PAsData PProposalVotes)

-- | @since 0.1.0
instance PUnsafeLiftDecl PProposalVotes where type PLifted PProposalVotes = ProposalVotes

-- | @since 0.1.0
deriving via
  (DerivePConstantViaNewtype ProposalVotes PProposalVotes (PMap 'Sorted PResultTag PInteger))
  instance
    (PConstantDecl ProposalVotes)

{- | Plutarch-level version of 'ProposalEffectMetadata'.

     @since 1.0.0
-}
newtype PProposalEffectMetadata (s :: S)
  = PProposalEffectMetadata
      ( Term
          s
          ( PDataRecord
              '[ "datumHash" ':= PDatumHash
               , "scriptHash" ':= PMaybeData (PAsData PScriptHash)
               ]
          )
      )
  deriving stock
    ( -- | @since 1.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 1.0.0
      PlutusType
    , -- | @since 1.0.0
      PIsData
    , -- | @since 1.0.0
      PEq
    , -- | @since 1.0.0
      PDataFields
    )

-- | @since 1.0.0
instance DerivePlutusType PProposalEffectMetadata where
  type DPTStrat _ = PlutusTypeDataList

-- | @since 1.0.0
instance PUnsafeLiftDecl PProposalEffectMetadata where
  type PLifted _ = ProposalEffectMetadata

-- | @since 1.0.0
deriving via
  (DerivePConstantViaDataList ProposalEffectMetadata PProposalEffectMetadata)
  instance
    (PConstantDecl ProposalEffectMetadata)

-- | @since 1.0.0
instance PTryFrom PData (PAsData PProposalEffectMetadata)

{- | The effect script hashes and their associated datum hash and authority check script hash
     belonging to a particular effect group or result.

     @since 1.0.0
-}
type PProposalEffectGroup =
  PMap
    'Sorted
    PValidatorHash
    PProposalEffectMetadata

{- | Plutarch-level version of 'ProposalDatum'.

     @since 0.1.0
-}
newtype PProposalDatum (s :: S) = PProposalDatum
  { getProposalDatum ::
      Term
        s
        ( PDataRecord
            '[ "proposalId" ':= PProposalId
             , "effects" ':= PMap 'Sorted PResultTag PProposalEffectGroup
             , "status" ':= PProposalStatus
             , "cosigners" ':= PBuiltinList (PAsData PCredential)
             , "thresholds" ':= PProposalThresholds
             , "votes" ':= PProposalVotes
             , "timingConfig" ':= PProposalTimingConfig
             , "startingTime" ':= PProposalStartingTime
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
      PEq
    , -- | @since 1.0.0
      PDataFields
    )

-- | @since 1.0.0
instance DerivePlutusType PProposalDatum where
  type DPTStrat _ = PlutusTypeDataList

instance PTryFrom PData (PAsData PProposalDatum)

-- | @since 0.1.0
instance PUnsafeLiftDecl PProposalDatum where type PLifted _ = ProposalDatum

-- | @since 0.1.0
deriving via (DerivePConstantViaDataList ProposalDatum PProposalDatum) instance (PConstantDecl ProposalDatum)

{- | Plutarch-level version of 'ProposalRedeemer'.

     @since 0.1.0
-}
data PProposalRedeemer (s :: S)
  = PVote (Term s (PDataRecord '["resultTag" ':= PResultTag]))
  | PCosign (Term s (PDataRecord '["newCosigners" ':= PBuiltinList (PAsData PCredential)]))
  | PUnlock (Term s (PDataRecord '[]))
  | PAdvanceProposal (Term s (PDataRecord '[]))
  deriving stock
    ( -- | @since 0.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      PlutusType
    , -- | @since 0.1.0
      PIsData
    )

-- | @since 0.2.0
instance DerivePlutusType PProposalRedeemer where
  type DPTStrat _ = PlutusTypeData

-- | @since 0.1.0
instance PTryFrom PData PProposalRedeemer

-- | @since 0.1.0
instance PUnsafeLiftDecl PProposalRedeemer where type PLifted PProposalRedeemer = ProposalRedeemer

-- | @since 0.1.0
deriving via (DerivePConstantViaData ProposalRedeemer PProposalRedeemer) instance (PConstantDecl ProposalRedeemer)

--------------------------------------------------------------------------------

{- | Check for various invariants a proposal must uphold.
     This can be used to check both upon creation and
     upon any following state transitions in the proposal.

     @since 0.1.0
-}

{- | Return true if the effect list contains at least one neutral outcome.

     @since 0.2.0
-}
phasNeutralEffect ::
  forall (s :: S).
  Term
    s
    ( PMap 'Sorted PResultTag PProposalEffectGroup
        :--> PBool
    )
phasNeutralEffect = phoistAcyclic $ PAssocMap.pany # PAssocMap.pnull

{- | Return true if votes and effects of the proposal have the same key set.

     @since 0.2.0
-}
pisEffectsVotesCompatible ::
  forall (s :: S).
  Term
    s
    ( PMap 'Sorted PResultTag PProposalEffectGroup
        :--> PProposalVotes
        :--> PBool
    )
pisEffectsVotesCompatible = phoistAcyclic $
  plam $ \((PM.pkeys @PList #) -> effectKeys) ((PM.pkeys #) . pto -> voteKeys) ->
    plistEquals # effectKeys # voteKeys

{- | Retutns true if vote counts of /all/ the options are zero.

   @since 0.2.0
-}
pisVotesEmpty ::
  forall (s :: S).
  Term
    s
    ( PProposalVotes
        :--> PBool
    )
pisVotesEmpty = phoistAcyclic $
  plam $ \(pto -> m :: Term _ (PMap _ _ _)) ->
    PAssocMap.pall # plam (#== 0) # m

{- | Wrapper for 'pwinner''. When the winner cannot be found,
      the 'neutral' option will be returned.

     @since 0.1.0
-}
pwinner ::
  forall (s :: S).
  Term
    s
    ( PProposalVotes
        :--> PInteger
        :--> PResultTag
        :--> PResultTag
    )
pwinner = phoistAcyclic $
  plam $ \votes quorum neutral -> pmatch (pwinner' # votes # quorum) $ \case
    PNothing -> neutral
    PJust winner -> winner

{- | Find the winner result tag, given the votes and the quorum.

     The winner should be unambiguous, meaning that if two options have the same highest votes,
       the function will return 'PNothing'.

     @since 0.1.0
-}
pwinner' ::
  forall (s :: S).
  Term
    s
    ( PProposalVotes
        :--> PInteger
        :--> PMaybe PResultTag
    )
pwinner' = phoistAcyclic $
  plam $ \votes quorum -> unTermCont $ do
    winner <- pletC $ phighestVotes # votes
    winnerResultTag <- pletC $ pfromData $ pfstBuiltin # winner
    highestVotes <- pletC $ pfromData $ psndBuiltin # winner

    let l :: Term _ (PBuiltinList _)
        l = pto $ pto votes

        f ::
          Term
            _
            ( PBuiltinPair (PAsData PResultTag) (PAsData PInteger)
                :--> PInteger
                :--> PInteger
            )
        f = plam $ \(pfromData . (psndBuiltin #) -> thisVotes) i ->
          pif
            (thisVotes #== highestVotes)
            (i + 1)
            i

        noDuplicateHighestVotes =
          ptraceIfFalse "Ambiguous winner" $
            pfoldr # f # 0 # l #== 1

        exceedQuorum =
          ptraceIfFalse "Highest vote count should exceed the minimum threshold" $
            quorum #< highestVotes

    pure $
      pif
        (noDuplicateHighestVotes #&& exceedQuorum)
        (pcon $ PJust winnerResultTag)
        (pcon PNothing)

{- | Find the outcome with the highest vote count given the votes.

     @since 0.1.0
-}
phighestVotes ::
  forall (s :: S).
  Term
    s
    ( PProposalVotes
        :--> PBuiltinPair (PAsData PResultTag) (PAsData PInteger)
    )
phighestVotes = phoistAcyclic $
  plam $ \votes ->
    let l :: Term _ (PBuiltinList _)
        l = pto $ pto votes

        f = phoistAcyclic $
          plam $ \this last ->
            let lastVotes = pfromData $ psndBuiltin # last
                thisVotes = pfromData $ psndBuiltin # this
             in pif (lastVotes #< thisVotes) this last
     in pfoldr # f # (phead # l) # l

{- | Find the "neutral" option (a dummy outcome with no effect) given the effects.

     @since 0.1.0
-}
pneutralOption ::
  forall (s :: S).
  Term
    s
    ( PMap 'Sorted PResultTag PProposalEffectGroup
        :--> PResultTag
    )
pneutralOption = phoistAcyclic $
  plam $ \effects ->
    let l :: Term _ (PBuiltinList (PBuiltinPair (PAsData PResultTag) _))
        l = pto effects

        f = phoistAcyclic $
          plam $
            pbuiltinUncurry $ \rt el ->
              pif
                (PAssocMap.pnull # el)
                (pcon $ PJust rt)
                (pcon PNothing)
     in pfromJust #$ pfirstJust # f # l

{- | Return true if the thresholds are valid.

     @since 0.2.0
-}
pisProposalThresholdsValid :: forall (s :: S). Term s (PProposalThresholds :--> PBool)
pisProposalThresholdsValid = phoistAcyclic $
  plam $ \thresholds -> unTermCont $ do
    thresholdsF <- pletAllC thresholds

    PDiscrete execute' <- pmatchC thresholdsF.execute
    PDiscrete draft' <- pmatchC thresholdsF.create
    PDiscrete vote' <- pmatchC thresholdsF.vote

    execute <- pletC $ pextract # execute'
    draft <- pletC $ pextract # draft'
    vote <- pletC $ pextract # vote'

    pure $
      foldr1
        (#&&)
        [ ptraceIfFalse "Execute threshold is less than or equal to 0" $ 0 #<= execute
        , ptraceIfFalse "Draft threshold is less than or equal to 0" $ 0 #<= draft
        , ptraceIfFalse "Vote threshold is less than or equal to 0" $ 0 #<= vote
        ]

{- | Retract votes given the option and the amount of votes.

     @since 0.1.0
-}
pretractVotes :: forall (s :: S). Term s (PResultTag :--> PInteger :--> PProposalVotes :--> PProposalVotes)
pretractVotes = phoistAcyclic $
  plam $ \rt count votes ->
    let voteMap :: Term _ (PMap 'Sorted PResultTag PInteger)
        voteMap = pto votes
     in pcon $
          PProposalVotes $
            PM.pupdate
              # plam
                ( \oldCount -> unTermCont $ do
                    newCount <- pletC $ oldCount - count
                    pguardC "Resulting vote count greater or equal to 0" $ 0 #<= newCount
                    pure $ pcon $ PJust newCount
                )
              # rt
              # voteMap
