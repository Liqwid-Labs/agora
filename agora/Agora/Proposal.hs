{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Agora.Proposal
Maintainer : emi@haskell.fyi
Description: Proposal scripts encoding effects that operate on the system.

Proposal scripts encoding effects that operate on the system.
-}
module Agora.Proposal (
  -- * Haskell-land
  Proposal (..),
  ProposalDatum (..),
  ProposalRedeemer (..),
  ProposalStatus (..),
  ProposalThresholds (..),
  ProposalVotes (..),
  ProposalId (..),
  ResultTag (..),
  emptyVotesFor,

  -- * Plutarch-land
  PProposalDatum (..),
  PProposalRedeemer (..),
  PProposalStatus (..),
  PProposalThresholds (..),
  PProposalVotes (..),
  PProposalId (..),
  PResultTag (..),

  -- * Plutarch helpers
  proposalDatumValid,
  pemptyVotesFor,
  pwinner,
  pwinner',
  pneutralOption,
  pretractVotes,
) where

import Agora.Proposal.Time (PProposalStartingTime, PProposalTimingConfig, ProposalStartingTime, ProposalTimingConfig)
import Agora.SafeMoney (GTTag)
import Agora.Utils (mustBePJust)
import Data.Tagged (Tagged)
import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))
import Plutarch.Api.V1 (
  KeyGuarantees (Unsorted),
  PDatumHash,
  PMap,
  PPubKeyHash,
  PValidatorHash,
 )
import Plutarch.DataRepr (DerivePConstantViaData (..), PDataFields, PIsDataReprInstances (..))
import Plutarch.Extra.IsData (
  DerivePConstantViaEnum (..),
  EnumIsData (..),
 )
import Plutarch.Extra.List (pnotNull)
import Plutarch.Extra.Map qualified as PM
import Plutarch.Extra.Map.Unsorted qualified as PUM
import Plutarch.Extra.Other (DerivePNewtype' (..))
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC)
import Plutarch.Lift (
  DerivePConstantViaNewtype (..),
  PConstantDecl,
  PUnsafeLiftDecl (..),
 )
import Plutarch.SafeMoney (PDiscrete)
import PlutusLedgerApi.V1 (DatumHash, PubKeyHash, ValidatorHash)
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap

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
  deriving newtype
    ( -- | @since 0.1.0
      PlutusTx.ToData
    , -- | @since 0.1.0
      PlutusTx.FromData
    , -- | @since 0.1.0
      PlutusTx.UnsafeFromData
    )
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      GHC.Generic
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
      GHC.Generic
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
      GHC.Generic
    , -- | @since 0.2.0
      Enum
    , -- | @since 0.2.0
      Bounded
    )
  deriving anyclass
    ( -- | @since 0.2.0
      Generic
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
      GHC.Generic
    )

-- | @since 0.1.0
PlutusTx.makeIsDataIndexed ''ProposalThresholds [('ProposalThresholds, 0)]

{- | Map which encodes the total tally for each result.
     It's important that the "shape" is consistent with the shape of 'effects'.

     e.g. if the 'effects' field looks like the following:

     @[('ResultTag' 0, []), ('ResultTag' 1, [(vh, dh)])]@

     Then 'ProposalVotes' needs be of the shape:

     @[('ResultTag' 0, n), ('ResultTag' 1, m)]@

     @since 0.1.0
-}
newtype ProposalVotes = ProposalVotes
  { getProposalVotes :: AssocMap.Map ResultTag Integer
  }
  deriving newtype
    ( -- | @since 0.1.0
      PlutusTx.ToData
    , -- | @since 0.1.0
      PlutusTx.FromData
    , -- | @since 0.1.0
      PlutusTx.UnsafeFromData
    )
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      GHC.Generic
    )

{- | Create a 'ProposalVotes' that has the same shape as the 'effects' field.

     @since 0.1.0
-}
emptyVotesFor :: forall a. AssocMap.Map ResultTag a -> ProposalVotes
emptyVotesFor = ProposalVotes . AssocMap.mapWithKey (const . const 0)

{- | Haskell-level datum for Proposal scripts.

     @since 0.1.0
-}
data ProposalDatum = ProposalDatum
  { proposalId :: ProposalId
  -- ^ Identification of the proposal.
  -- TODO: could we encode this more efficiently?
  -- This is shaped this way for future proofing.
  -- See https://github.com/Liqwid-Labs/agora/issues/39
  , effects :: AssocMap.Map ResultTag (AssocMap.Map ValidatorHash DatumHash)
  -- ^ Effect lookup table. First by result, then by effect hash.
  , status :: ProposalStatus
  -- ^ The status the proposal is in.
  , cosigners :: [PubKeyHash]
  -- ^ Who created the proposal initially, and who cosigned it later.
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
      GHC.Generic
    )

PlutusTx.makeIsDataIndexed ''ProposalDatum [('ProposalDatum, 0)]

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
    --   provided enough GT is shared  among them.
    Cosign [PubKeyHash]
  | -- | Allow unlocking one or more stakes with votes towards particular 'ResultTag'.
    Unlock ResultTag
  | -- | Advance the proposal, performing the required checks for whether that is legal.
    --
    --   These are roughly the checks for each possible transition:
    --
    --   === @'Draft' -> 'VotingReady'@:
    --
    --     1. The sum of all of the cosigner's GT is larger than the 'startVoting' field of 'ProposalThresholds'.
    --     2. The proposal's current time ensures 'isDraftPeriod'.
    --
    --   === @'VotingReady' -> 'Locked'@:
    --
    --     1. The sum of all votes is larger than 'countVoting'.
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
      GHC.Generic
    )

-- | @since 0.1.0
PlutusTx.makeIsDataIndexed
  ''ProposalRedeemer
  [ ('Vote, 0)
  , ('Cosign, 1)
  , ('Unlock, 2)
  , ('AdvanceProposal, 3)
  ]

{- | Parameters that identify the Proposal validator script.

     @since 0.1.0
-}
data Proposal = Proposal
  { governorSTAssetClass :: AssetClass
  , stakeSTAssetClass :: AssetClass
  , maximumCosigners :: Integer
  -- ^ Arbitrary limit for maximum amount of cosigners on a proposal.
  }
  deriving stock
    ( -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      GHC.Generic
    )

--------------------------------------------------------------------------------
-- Plutarch-land

{- | Plutarch-level version of 'ResultTag'.

     @since 0.1.0
-}
newtype PResultTag (s :: S) = PResultTag (Term s PInteger)
  deriving
    ( -- | @since 0.1.0
      PlutusType
    , -- | @since 0.1.0
      PIsData
    , -- | @since 0.1.0
      PEq
    , -- | @since 0.1.0
      POrd
    )
    via (DerivePNewtype PResultTag PInteger)

-- | @since 0.1.0
instance PUnsafeLiftDecl PResultTag where type PLifted PResultTag = ResultTag

-- | @since 0.1.0
deriving via
  (DerivePConstantViaNewtype ResultTag PResultTag PInteger)
  instance
    (PConstantDecl ResultTag)

-- | @since 0.1.0
deriving via
  PAsData (DerivePNewtype PResultTag PInteger)
  instance
    PTryFrom PData (PAsData PResultTag)

{- | Plutarch-level version of 'PProposalId'.

     @since 0.1.0
-}
newtype PProposalId (s :: S) = PProposalId (Term s PInteger)
  deriving
    ( -- | @since 0.1.0
      PlutusType
    , -- | @since 0.1.0
      PIsData
    , -- | @since 0.1.0
      PEq
    , -- | @since 0.1.0
      POrd
    )
    via (DerivePNewtype PProposalId PInteger)

-- | @since 0.1.0
deriving via
  PAsData (DerivePNewtype PProposalId PInteger)
  instance
    PTryFrom PData (PAsData PProposalId)

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
newtype PProposalStatus (s :: S) = PProposalStatus (Term s PInteger)
  deriving stock
    ( -- | @since 0.1.0
      GHC.Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      Generic
    )
  deriving
    ( -- | @since 0.1.0
      PlutusType
    , -- | @since 0.1.0
      PIsData
    , -- | @since 0.1.0
      PEq
    )
    via (DerivePNewtype' PProposalStatus)

-- | @since 0.1.0
instance PUnsafeLiftDecl PProposalStatus where type PLifted PProposalStatus = ProposalStatus

-- | @since 0.1.0
deriving via PAsData (DerivePNewtype' PProposalStatus) instance PTryFrom PData (PAsData PProposalStatus)

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
    )
    via (PIsDataReprInstances PProposalThresholds)

-- | @since 0.1.0
deriving via
  PAsData (PIsDataReprInstances PProposalThresholds)
  instance
    PTryFrom PData (PAsData PProposalThresholds)

-- | @since 0.1.0
instance PUnsafeLiftDecl PProposalThresholds where type PLifted PProposalThresholds = ProposalThresholds

-- | @since 0.1.0
deriving via
  (DerivePConstantViaData ProposalThresholds PProposalThresholds)
  instance
    (PConstantDecl ProposalThresholds)

{- | Plutarch-level version of 'ProposalVotes'.

     @since 0.1.0
-}
newtype PProposalVotes (s :: S)
  = PProposalVotes (Term s (PMap 'Unsorted PResultTag PInteger))
  deriving
    ( -- | @since 0.1.0
      PlutusType
    , -- | @since 0.1.0
      PIsData
    )
    via (DerivePNewtype PProposalVotes (PMap 'Unsorted PResultTag PInteger))

-- | @since 0.1.0
deriving via
  PAsData (DerivePNewtype PProposalVotes (PMap 'Unsorted PResultTag PInteger))
  instance
    PTryFrom PData (PAsData PProposalVotes)

{- | Retract votes given the option and the amount of votes.

     @since 0.1.0
-}
pretractVotes :: Term s (PResultTag :--> PInteger :--> PProposalVotes :--> PProposalVotes)
pretractVotes = phoistAcyclic $
  plam $ \rt count votes ->
    let voteMap :: Term _ (PMap 'Unsorted PResultTag PInteger)
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

-- | @since 0.1.0
instance PUnsafeLiftDecl PProposalVotes where type PLifted PProposalVotes = ProposalVotes

-- | @since 0.1.0
deriving via
  (DerivePConstantViaNewtype ProposalVotes PProposalVotes (PMap 'Unsorted PResultTag PInteger))
  instance
    (PConstantDecl ProposalVotes)

{- | Plutarch-level version of 'emptyVotesFor'.

     @since 0.1.0
-}
pemptyVotesFor :: forall s a. (PIsData a) => Term s (PMap 'Unsorted PResultTag a :--> PProposalVotes)
pemptyVotesFor =
  phoistAcyclic $
    plam
      ( \m ->
          pcon $
            PProposalVotes $ PM.pmap # plam (const $ pconstant 0) # m
      )

{- | Plutarch-level version of 'ProposalDatum'.

     @since 0.1.0
-}
newtype PProposalDatum (s :: S) = PProposalDatum
  { getProposalDatum ::
      Term
        s
        ( PDataRecord
            '[ "proposalId" ':= PProposalId
             , "effects" ':= PMap 'Unsorted PResultTag (PMap 'Unsorted PValidatorHash PDatumHash)
             , "status" ':= PProposalStatus
             , "cosigners" ':= PBuiltinList (PAsData PPubKeyHash)
             , "thresholds" ':= PProposalThresholds
             , "votes" ':= PProposalVotes
             , "timingConfig" ':= PProposalTimingConfig
             , "startingTime" ':= PProposalStartingTime
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
    via (PIsDataReprInstances PProposalDatum)

-- | @since 0.1.0
deriving via PAsData (PIsDataReprInstances PProposalDatum) instance PTryFrom PData (PAsData PProposalDatum)

-- | @since 0.1.0
instance PUnsafeLiftDecl PProposalDatum where type PLifted PProposalDatum = ProposalDatum

-- | @since 0.1.0
deriving via (DerivePConstantViaData ProposalDatum PProposalDatum) instance (PConstantDecl ProposalDatum)

{- | Plutarch-level version of 'ProposalRedeemer'.

     @since 0.1.0
-}
data PProposalRedeemer (s :: S)
  = PVote (Term s (PDataRecord '["resultTag" ':= PResultTag]))
  | PCosign (Term s (PDataRecord '["newCosigners" ':= PBuiltinList (PAsData PPubKeyHash)]))
  | PUnlock (Term s (PDataRecord '["resultTag" ':= PResultTag]))
  | PAdvanceProposal (Term s (PDataRecord '[]))
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
    via PIsDataReprInstances PProposalRedeemer

-- | @since 0.1.0
deriving via
  PAsData (PIsDataReprInstances PProposalRedeemer)
  instance
    PTryFrom PData (PAsData PProposalRedeemer)

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
proposalDatumValid :: Proposal -> Term s (Agora.Proposal.PProposalDatum :--> PBool)
proposalDatumValid proposal =
  phoistAcyclic $
    plam $ \datum' -> unTermCont $ do
      datum <- pletFieldsC @'["effects", "cosigners", "votes"] $ datum'

      let atLeastOneNegativeResult =
            pany
              # phoistAcyclic
                (plam $ \m -> pnull #$ pto $ pfromData $ psndBuiltin # m)
              #$ pto
              $ pfromData datum.effects

      pure $
        foldr1
          (#&&)
          [ ptraceIfFalse "Proposal has at least one ResultTag has no effects" atLeastOneNegativeResult
          , ptraceIfFalse "Proposal has at least one cosigner" $ pnotNull # pfromData datum.cosigners
          , ptraceIfFalse "Proposal has fewer cosigners than the limit" $ plength # pfromData datum.cosigners #<= pconstant proposal.maximumCosigners
          , ptraceIfFalse "Proposal votes and effects are compatible with each other" $ PUM.pkeysEqual # datum.effects # pto (pfromData datum.votes)
          ]

{- | Wrapper for 'pwinner''. When the winner cannot be found,
      the 'neutral' option will be returned.

     @since 0.1.0
-}
pwinner ::
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
  Term
    s
    ( PProposalVotes
        :--> PBuiltinPair (PAsData PResultTag) (PAsData PInteger)
    )
phighestVotes = phoistAcyclic $
  plam $ \votes ->
    let l :: Term _ (PBuiltinList _)
        l = pto $ pto votes

        f ::
          Term
            _
            ( PBuiltinPair (PAsData PResultTag) (PAsData PInteger)
                :--> PBuiltinPair (PAsData PResultTag) (PAsData PInteger)
                :--> PBuiltinPair (PAsData PResultTag) (PAsData PInteger)
            )
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
  Term
    s
    ( PMap 'Unsorted PResultTag (PMap 'Unsorted PValidatorHash PDatumHash)
        :--> PResultTag
    )
pneutralOption = phoistAcyclic $
  plam $ \effects ->
    let l :: Term _ (PBuiltinList (PBuiltinPair (PAsData PResultTag) _))
        l = pto effects

        f :: Term _ (PBuiltinPair (PAsData PResultTag) (PAsData (PMap 'Unsorted _ _)) :--> PBool)
        f = phoistAcyclic $
          plam $ \((pfromData . (psndBuiltin #) -> el)) ->
            let el' :: Term _ (PBuiltinList _)
                el' = pto el
             in pnull # el'
     in pfromData $ pfstBuiltin #$ mustBePJust # "No neutral option" #$ pfind # f # l
