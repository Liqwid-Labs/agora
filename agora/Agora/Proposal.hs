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
  ProposalStatus (..),
  ProposalThresholds (..),
  ProposalVotes (..),
  ProposalId (..),
  ResultTag (..),

  -- * Plutarch-land
  PProposalDatum (..),
  PProposalStatus (..),
  PProposalThresholds (..),
  PProposalVotes (..),
  PProposalId (..),
  PResultTag (..),

  -- * Scripts
  proposalValidator,
  proposalPolicy,
  proposalDatumValid,
) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))
import Plutarch.Api.V1 (
  PDatumHash,
  PMap,
  PMintingPolicy,
  PPubKeyHash,
  PScriptContext (PScriptContext),
  PScriptPurpose (PMinting, PSpending),
  PTxInfo (PTxInfo),
  PValidator,
  PValidatorHash,
 )
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutus.V1.Ledger.Api (DatumHash, PubKeyHash, ValidatorHash)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap

--------------------------------------------------------------------------------

import Agora.SafeMoney (GTTag)
import Agora.Utils (passert, pnotNull, ptokenSpent)
import Control.Arrow (first)
import Plutarch (popaque)
import Plutarch.Api.V1.Extra (passetClass, passetClassValueOf)
import Plutarch.Builtin (PBuiltinMap)
import Plutarch.Lift (DerivePConstantViaNewtype (..), PUnsafeLiftDecl (..))
import Plutarch.Monadic qualified as P
import Plutarch.SafeMoney (PDiscrete, Tagged)
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce)
import Plutus.V1.Ledger.Value (AssetClass (AssetClass))

--------------------------------------------------------------------------------
-- Haskell-land

{- | Encodes a result. Typically, for a Yes/No proposal, we encode it like this:

@
"No"  ~ 'ResultTag' 0
"Yes" ~ 'ResultTag' 1
@
-}
newtype ResultTag = ResultTag {getResultTag :: Integer}
  deriving stock (Eq, Show, Ord)
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

{- | The "status" of the proposal. This is only useful for state transitions,
     as opposed to time-based "phases".

     If the proposal is 'VotingReady', for instance, that doesn't necessarily
     mean that voting is possible, as this also requires the timing to be right.
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
    --   permanently. The proposal can now be executed.
    Voted
  | -- | The proposal has finished.
    --
    --   This can mean it's been voted on and completed, but it can also mean
    --   the proposal failed due to time constraints or didn't
    --   get to 'VotingReady' first.
    --
    --   At this stage, the 'votes' field of 'ProposalState' is frozen.
    --
    --   See 'AdvanceProposal' for documentation on state transitions.
    --
    --   TODO: The owner of the proposal may choose to reclaim their proposal.
    Finished
  deriving stock (Eq, Show, GHC.Generic)

PlutusTx.makeIsDataIndexed ''ProposalStatus [('Draft, 0), ('VotingReady, 1), ('Voted, 2), ('Finished, 3)]

{- | The threshold values for various state transitions to happen.
     This data is stored centrally (in the 'Agora.Governor.Governor') and copied over
     to 'Proposal's when they are created.
-}
data ProposalThresholds = ProposalThresholds
  { countVoting :: Tagged GTTag Integer
  -- ^ How much GT minimum must a particular 'ResultTag' accumulate for it to pass.
  , create :: Tagged GTTag Integer
  -- ^ How much GT required to "create" a proposal.
  , vote :: Tagged GTTag Integer
  -- ^ How much GT required to allow voting to happen.
  -- (i.e. to move into 'VotingReady')
  }
  deriving stock (Eq, Show, GHC.Generic)

PlutusTx.makeIsDataIndexed ''ProposalThresholds [('ProposalThresholds, 0)]

{- | Map which encodes the total tally for each result.
   It's important that the "shape" is consistent with the shape of 'effects'.

   e.g. if the 'effects' field looks like the following:

   @[('ResultTag' 0, []), ('ResultTag' 1, [(vh, dh)])]@

   Then 'ProposalVotes' needs be of the shape:

   @[('ResultTag' 0, n), ('ResultTag' 1, m)]@
-}
newtype ProposalVotes = ProposalVotes
  { getProposalVotes :: AssocMap.Map ResultTag Integer
  }
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
  deriving stock (Eq, Show, GHC.Generic)

-- | Haskell-level datum for Proposal scripts.
data ProposalDatum = ProposalDatum
  { -- TODO: could we encode this more efficiently?
  -- This is shaped this way for future proofing.
  -- See https://github.com/Liqwid-Labs/agora/issues/39
  effects :: AssocMap.Map ResultTag [(ValidatorHash, DatumHash)]
  -- ^ Effect lookup table. First by result, then by effect hash.
  , status :: ProposalStatus
  -- ^ The status the proposal is in.
  , cosigners :: [PubKeyHash]
  -- ^ Who created the proposal initially, and who cosigned it later.
  , thresholds :: ProposalThresholds
  -- ^ Thresholds copied over on initialization.
  , votes :: ProposalVotes
  -- ^ Vote tally on the proposal
  }
  deriving stock (Eq, Show, GHC.Generic)

PlutusTx.makeIsDataIndexed ''ProposalDatum [('ProposalDatum, 0)]

-- | Haskell-level redeemer for Proposal scripts.
data ProposalRedeemer
  = -- | Cast one or more votes towards a particular 'ResultTag'.
    Vote ResultTag
  | -- | Add one or more public keys to the cosignature list. Must be signed by
    --   those cosigning.
    --
    --   This is particularly used in the 'Draft' 'ProposalStatus'. Where matching
    --   'Stake's can be called to advance the proposal, provided enough GT is shared
    --   among them.
    Cosign [PubKeyHash]
  | -- | Allow unlocking one or more stakes with votes towards particular 'ResultTag'.
    Unlock ResultTag
  | -- | Advance the proposal, performing the required checks for whether that is legal.
    --
    --   These are roughly the checks for each possible transition:
    --
    --   @'Draft' -> 'VotingReady'@:
    --     1. The sum of all of the cosigner's GT is larger than the 'vote' field of 'ProposalThresholds'.
    --     2. The proposal hasn't been alive for longer than the review time.
    --
    --   @'VotingReady' -> 'Voted'@:
    --     1. The sum of all votes is larger than 'countVoting'.
    --     2. The winning 'ResultTag' has more votes than all other 'ResultTag's.
    --     3. The proposal hasn't been alive for longer than the voting time.
    --
    --   @'Voted' -> 'Finished'@:
    --     Always valid provided the conditions for the transition are met.
    --
    --   @* -> 'Finished'@:
    --     If the proposal has run out of time for the current 'ProposalStatus', it will always be possible
    --     to transition into 'Finished' state, because it has expired (and failed).
    AdvanceProposal
  deriving stock (Eq, Show, GHC.Generic)

PlutusTx.makeIsDataIndexed
  ''ProposalRedeemer
  [ ('Vote, 0)
  , ('Cosign, 1)
  , ('Unlock, 2)
  , ('AdvanceProposal, 3)
  ]

{- | Identifies a Proposal, issued upon creation of a proposal.
     In practice, this number starts at zero, and increments by one
     for each proposal. The 100th proposal will be @'ProposalId' 99@.
     This counter lives in the 'Governor', see 'nextProposalId'.
-}
newtype ProposalId = ProposalId {proposalTag :: Integer}
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
  deriving stock (Eq, Show, GHC.Generic)

-- | Parameters that identify the Proposal validator script.
data Proposal = Proposal
  { governorSTAssetClass :: AssetClass
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Plutarch-land

-- | Plutarch-level version of 'ResultTag'.
newtype PResultTag (s :: S) = PResultTag (Term s PInteger)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PResultTag PInteger)

instance PUnsafeLiftDecl PResultTag where type PLifted PResultTag = ResultTag
deriving via
  (DerivePConstantViaNewtype ResultTag PResultTag PInteger)
  instance
    (PConstant ResultTag)

-- FIXME: This instance and the one below, for 'PProposalId', should be derived.
-- Soon this will be possible through 'DerivePNewtype'.
instance PTryFrom PData (PAsData PResultTag) where
  type PTryFromExcess PData (PAsData PResultTag) = PTryFromExcess PData (PAsData PInteger)
  ptryFrom' d k =
    ptryFrom' @_ @(PAsData PInteger) d $
      -- JUSTIFICATION:
      -- We are coercing from @PAsData underlying@ to @PAsData (PTagged tag underlying)@.
      -- Since 'PTagged' is a simple newtype, their shape is the same.
      k . first punsafeCoerce

-- | Plutarch-level version of 'PProposalId'.
newtype PProposalId (s :: S) = PProposalId (Term s PInteger)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PProposalId PInteger)

instance PTryFrom PData (PAsData PProposalId) where
  type PTryFromExcess PData (PAsData PProposalId) = PTryFromExcess PData (PAsData PInteger)
  ptryFrom' d k =
    ptryFrom' @_ @(PAsData PInteger) d $
      -- JUSTIFICATION:
      -- We are coercing from @PAsData underlying@ to @PAsData (PTagged tag underlying)@.
      -- Since 'PTagged' is a simple newtype, their shape is the same.
      k . first punsafeCoerce

instance PUnsafeLiftDecl PProposalId where type PLifted PProposalId = ProposalId
deriving via
  (DerivePConstantViaNewtype ProposalId PProposalId PInteger)
  instance
    (PConstant ProposalId)

-- | Plutarch-level version of 'ProposalStatus'.
data PProposalStatus (s :: S)
  = -- TODO: 'PProposalStatus' ought te be encoded as 'PInteger'.
    -- e.g. like Tilde used 'pmatchEnum'.
    PDraft (Term s (PDataRecord '[]))
  | PVotingReady (Term s (PDataRecord '[]))
  | PFinished (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PProposalStatus

instance PUnsafeLiftDecl PProposalStatus where type PLifted PProposalStatus = ProposalStatus
deriving via (DerivePConstantViaData ProposalStatus PProposalStatus) instance (PConstant ProposalStatus)

-- | Plutarch-level version of 'ProposalThresholds'.
newtype PProposalThresholds (s :: S) = PProposalThresholds
  { getProposalThresholds ::
    Term
      s
      ( PDataRecord
          '[ "execute" ':= PDiscrete GTTag
           , "draft" ':= PDiscrete GTTag
           , "vote" ':= PDiscrete GTTag
           ]
      )
  }
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via (PIsDataReprInstances PProposalThresholds)

instance PUnsafeLiftDecl PProposalThresholds where type PLifted PProposalThresholds = ProposalThresholds
deriving via (DerivePConstantViaData ProposalThresholds PProposalThresholds) instance (PConstant ProposalThresholds)

-- | Plutarch-level version of 'ProposalVotes'.
newtype PProposalVotes (s :: S)
  = PProposalVotes (Term s (PMap PResultTag PInteger))
  deriving (PlutusType, PIsData) via (DerivePNewtype PProposalVotes (PMap PResultTag PInteger))

instance PUnsafeLiftDecl PProposalVotes where type PLifted PProposalVotes = ProposalVotes
deriving via
  (DerivePConstantViaNewtype ProposalVotes PProposalVotes (PMap PResultTag PInteger))
  instance
    (PConstant ProposalVotes)

-- | Plutarch-level version of 'ProposalDatum'.
newtype PProposalDatum (s :: S) = PProposalDatum
  { getProposalDatum ::
    Term
      s
      ( PDataRecord
          '[ "effects" ':= PMap PResultTag (PMap PValidatorHash PDatumHash)
           , "status" ':= PProposalStatus
           , "cosigners" ':= PBuiltinList (PAsData PPubKeyHash)
           , "thresholds" ':= PProposalThresholds
           , "votes" ':= PProposalVotes
           ]
      )
  }
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via (PIsDataReprInstances PProposalDatum)

instance PUnsafeLiftDecl PProposalDatum where type PLifted PProposalDatum = ProposalDatum
deriving via (DerivePConstantViaData ProposalDatum PProposalDatum) instance (PConstant ProposalDatum)

-- | Haskell-level redeemer for Proposal scripts.
data PProposalRedeemer (s :: S)
  = PVote (Term s (PDataRecord '["resultTag" ':= PResultTag]))
  | PCosign (Term s (PDataRecord '["newCosigners" ':= PBuiltinList (PAsData PPubKeyHash)]))
  | PUnlock (Term s (PDataRecord '["resultTag" ':= PResultTag]))
  | PAdvanceProposal (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PProposalRedeemer

-- TODO: Waiting on PTryFrom for 'PPubKeyHash'
-- deriving via
--   PAsData (PIsDataReprInstances PProposalRedeemer)
--   instance
--     PTryFrom PData (PAsData PProposalRedeemer)

instance PUnsafeLiftDecl PProposalRedeemer where type PLifted PProposalRedeemer = ProposalRedeemer
deriving via (DerivePConstantViaData ProposalRedeemer PProposalRedeemer) instance (PConstant ProposalRedeemer)

--------------------------------------------------------------------------------

{- | Policy for Proposals.
   This needs to perform two checks:
     - Governor is happy with mint.
     - Exactly 1 token is minted.

   NOTE: The governor needs to check that the datum is correct
         and sent to the right address.
-}
proposalPolicy :: Proposal -> ClosedTerm PMintingPolicy
proposalPolicy proposal =
  plam $ \_redeemer ctx' -> P.do
    PScriptContext ctx' <- pmatch ctx'
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    PTxInfo txInfo' <- pmatch $ pfromData ctx.txInfo
    txInfo <- pletFields @'["inputs", "mint"] txInfo'
    PMinting _ownSymbol <- pmatch $ pfromData ctx.purpose

    let inputs = txInfo.inputs
        mintedValue = pfromData txInfo.mint
        AssetClass (govCs, govTn) = proposal.governorSTAssetClass

    PMinting ownSymbol' <- pmatch $ pfromData ctx.purpose
    let mintedProposalST = passetClassValueOf # mintedValue # (passetClass # (pfield @"_0" # ownSymbol') # pconstant "")

    passert "Governance state-thread token must move" $
      ptokenSpent
        # (passetClass # pconstant govCs # pconstant govTn)
        # inputs

    passert "Minted exactly one proposal ST" $
      mintedProposalST #== 1

    popaque (pconstant ())

-- | Validator for Proposals.
proposalValidator :: Proposal -> ClosedTerm PValidator
proposalValidator _ =
  plam $ \_datum _redeemer ctx' -> P.do
    PScriptContext ctx' <- pmatch ctx'
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    PTxInfo txInfo' <- pmatch $ pfromData ctx.txInfo
    _txInfo <- pletFields @'["inputs", "mint"] txInfo'
    PSpending _txOutRef <- pmatch $ pfromData ctx.purpose
    popaque (pconstant ())

{- | Check for various invariants a proposal must uphold.
     This can be used to check both upopn creation and
     upon any following state transitions in the proposal.
-}
proposalDatumValid :: Term s (PProposalDatum :--> PBool)
proposalDatumValid =
  phoistAcyclic $
    plam $ \datum' -> P.do
      datum <- pletFields @'["effects", "cosigners"] $ datum'

      let effects :: Term _ (PBuiltinMap PResultTag (PBuiltinMap PValidatorHash PDatumHash))
          effects = punsafeCoerce datum.effects

          atLeastOneNegativeResult :: Term _ PBool
          atLeastOneNegativeResult =
            pany # plam (\pair -> pnull #$ pfromData $ psndBuiltin # pair) # effects

      foldr1
        (#&&)
        [ ptraceIfFalse "Proposal has at least one ResultTag has no effects" atLeastOneNegativeResult
        , ptraceIfFalse "Proposal has at least one cosigner" $ pnotNull # pfromData datum.cosigners
        ]
