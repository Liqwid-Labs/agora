{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Agora.Stake
Maintainer : emi@haskell.fyi
Description: Vote-lockable stake UTXOs holding GT.

Vote-lockable stake UTXOs holding GT.
-}
module Agora.Stake (
  -- * Haskell-land
  StakeDatum (..),
  StakeRedeemer (..),
  Stake (..),
  ProposalLock (..),

  -- * Plutarch-land
  PStakeDatum (..),
  PStakeRedeemer (..),
  PProposalLock (..),
  PStakeUsage (..),

  -- * Utility functions
  stakeLocked,
  findStakeOwnedBy,
  pgetStakeUsage,
) where

import Agora.Plutarch.Orphans ()
import Agora.Proposal (PProposalId, PResultTag, ProposalId (..), ResultTag (..))
import Agora.SafeMoney (GTTag)
import Data.Tagged (Tagged (..))
import GHC.Generics qualified as GHC
import Generics.SOP (Generic, HasDatatypeInfo, I (I))
import Plutarch.Api.V1 (
  PDatum,
  PDatumHash,
  PMaybeData (PDJust, PDNothing),
  PPubKeyHash,
  PTuple,
  PTxInInfo (PTxInInfo),
  PTxOut (PTxOut),
 )
import Plutarch.Api.V1.AssetClass (PAssetClass, passetClassValueOf)
import Plutarch.Api.V1.ScriptContext (ptryFindDatum)
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Extra.IsData (
  DerivePConstantViaDataList (..),
  ProductIsData (ProductIsData),
 )
import Plutarch.Extra.List (pmapMaybe, pnotNull)
import Plutarch.Extra.Other (DerivePNewtype' (..))
import Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC)
import Plutarch.Internal (punsafeCoerce)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.SafeMoney (PDiscrete)
import PlutusLedgerApi.V1 (PubKeyHash)
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusTx qualified
import Prelude hiding (Num (..))

--------------------------------------------------------------------------------

{- | Parameters for creating Stake scripts.

     @since 0.1.0
-}
data Stake = Stake
  { gtClassRef :: Tagged GTTag AssetClass
  -- ^ Used when inlining the AssetClass of a 'PDiscrete' in the script code.
  , proposalSTClass :: AssetClass
  }
  deriving stock
    ( -- | @since 0.1.0
      GHC.Generic
    )

{- | A lock placed on a Stake datum in order to prevent
     depositing and withdrawing when votes are in place.

     NOTE: Due to retracting votes always being possible,
     this lock will only lock with contention on the proposal.

     FIXME: Contention on Proposals could create contention
     on voting which in turn creates contention on stakers.

     Vaguely this is the dependency graph for this locking
     interaction. Both the stake validator and the proposal
     validator are only able to check for one another through
     the datum belonging to the ST:

     @
     ┌─────────────────┐   ┌────────────────────┐
     │ Stake Validator ├─┐ │ Proposal Validator │
     └────────┬────────┘ │ └──────┬─────┬───────┘
              │          │        │     │
              │        ┌─┼────────┘     │
              ▼        │ │              ▼
     ┌──────────────┐  │ │ ┌─────────────────┐
     │ Stake Policy │◄─┘ └►│ Proposal Policy │
     └──────────────┘      └─────────────────┘
     @

     @since 0.1.0
-}
data ProposalLock = ProposalLock
  { vote :: ResultTag
  -- ^ What was voted on. This allows retracting votes to
  --   undo their vote.
  , proposalId :: ProposalId
  -- ^ Identifies the proposal. See 'ProposalId' for further
  -- comments on its significance.
  }
  deriving stock
    ( -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      GHC.Generic
    )
  deriving anyclass (Generic)
  deriving
    ( -- | @since 0.1.0
      PlutusTx.ToData
    , -- | @since 0.1.0
      PlutusTx.FromData
    , -- | @since 0.1.0
      PlutusTx.UnsafeFromData
    )
    via (ProductIsData ProposalLock)

{- | Haskell-level redeemer for Stake scripts.

     @since 0.1.0
-}
data StakeRedeemer
  = -- | Deposit or withdraw a discrete amount of the staked governance token.
    --   Stake must be unlocked.
    DepositWithdraw (Tagged GTTag Integer)
  | -- | Destroy a stake, retrieving its LQ, the minimum ADA and any other assets.
    --   Stake must be unlocked.
    Destroy
  | -- | Permit a Vote to be added onto a 'Agora.Proposal.Proposal'.
    --   This also adds a lock to the 'lockedBy' field. See 'ProposalLock'.
    --   This needs to be done in sync with casting a vote, otherwise
    --   it's possible for a lock to be permanently placed on the stake,
    --   and then the funds are lost.
    PermitVote ProposalLock
  | -- | Retract a vote, removing it from the 'lockedBy' field. See 'ProposalLock'.
    --   This action checks for permission of the 'Agora.Proposal.Proposal'. Finished proposals are
    --   always allowed to have votes retracted and won't affect the Proposal datum,
    --   allowing 'Stake's to be unlocked.
    RetractVotes [ProposalLock]
  | -- | The owner can consume stake if nothing is changed about it.
    --   If the proposal token moves, this is equivalent to the owner consuming it.
    WitnessStake
  deriving stock (Show, GHC.Generic)

PlutusTx.makeIsDataIndexed
  ''StakeRedeemer
  [ ('DepositWithdraw, 0)
  , ('Destroy, 1)
  , ('PermitVote, 2)
  , ('RetractVotes, 3)
  , ('WitnessStake, 4)
  ]

{- | Haskell-level datum for Stake scripts.

     @since 0.1.0
-}
data StakeDatum = StakeDatum
  { stakedAmount :: Tagged GTTag Integer
  -- ^ Tracks the amount of governance token staked in the datum.
  -- This also acts as the voting weight for 'Agora.Proposal.Proposal's.
  , owner :: PubKeyHash
  -- ^ The hash of the public key this stake belongs to.
  --
  -- TODO Support for MultiSig/Scripts is tracked here:
  --      https://github.com/Liqwid-Labs/agora/issues/45
  , lockedBy :: [ProposalLock]
  -- ^ The current proposals locking this stake. This field must be empty
  -- for the stake to be usable for deposits and withdrawals.
  }
  deriving stock (Show, GHC.Generic)
  deriving anyclass (Generic)
  deriving
    ( -- | @since 0.1.0
      PlutusTx.ToData
    , -- | @since 0.1.0
      PlutusTx.FromData
    )
    via (ProductIsData StakeDatum)

--------------------------------------------------------------------------------

{- | Plutarch-level datum for Stake scripts.

     @since 0.1.0
-}
newtype PStakeDatum (s :: S) = PStakeDatum
  { getStakeDatum ::
      Term
        s
        ( PDataRecord
            '[ "stakedAmount" ':= PDiscrete GTTag
             , "owner" ':= PPubKeyHash
             , "lockedBy" ':= PBuiltinList (PAsData PProposalLock)
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
    via (DerivePNewtype' PStakeDatum)

-- | @since 0.1.0
instance Plutarch.Lift.PUnsafeLiftDecl PStakeDatum where type PLifted PStakeDatum = StakeDatum

-- | @since 0.1.0
deriving via (DerivePConstantViaDataList StakeDatum PStakeDatum) instance (Plutarch.Lift.PConstantDecl StakeDatum)

-- | @since 0.1.0
deriving via PAsData (DerivePNewtype' PStakeDatum) instance PTryFrom PData (PAsData PStakeDatum)

{- | Plutarch-level redeemer for Stake scripts.

     @since 0.1.0
-}
data PStakeRedeemer (s :: S)
  = -- | Deposit or withdraw a discrete amount of the staked governance token.
    PDepositWithdraw (Term s (PDataRecord '["delta" ':= PDiscrete GTTag]))
  | -- | Destroy a stake, retrieving its LQ, the minimum ADA and any other assets.
    PDestroy (Term s (PDataRecord '[]))
  | PPermitVote (Term s (PDataRecord '["lock" ':= PProposalLock]))
  | PRetractVotes (Term s (PDataRecord '["locks" ':= PBuiltinList (PAsData PProposalLock)]))
  | PWitnessStake (Term s (PDataRecord '[]))
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
    via PIsDataReprInstances PStakeRedeemer

deriving via
  PAsData (PIsDataReprInstances PStakeRedeemer)
  instance
    PTryFrom PData (PAsData PStakeRedeemer)

instance Plutarch.Lift.PUnsafeLiftDecl PStakeRedeemer where type PLifted PStakeRedeemer = StakeRedeemer
deriving via (DerivePConstantViaData StakeRedeemer PStakeRedeemer) instance (Plutarch.Lift.PConstantDecl StakeRedeemer)

{- | Plutarch-level version of 'ProposalLock'.

     @since 0.1.0
-}
newtype PProposalLock (s :: S) = PProposalLock
  { getProposalLock ::
      Term
        s
        ( PDataRecord
            '[ "vote" ':= PResultTag
             , "proposalTag" ':= PProposalId
             ]
        )
  }
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields, PEq)
    via (DerivePNewtype' PProposalLock)

deriving via
  PAsData (DerivePNewtype' PProposalLock)
  instance
    PTryFrom PData (PAsData PProposalLock)

instance Plutarch.Lift.PUnsafeLiftDecl PProposalLock where type PLifted PProposalLock = ProposalLock
deriving via (DerivePConstantViaDataList ProposalLock PProposalLock) instance (Plutarch.Lift.PConstantDecl ProposalLock)

--------------------------------------------------------------------------------

{- | Check whether a Stake is locked. If it is locked, various actions are unavailable.

     @since 0.1.0
-}
stakeLocked :: forall (s :: S). Term s (PStakeDatum :--> PBool)
stakeLocked = phoistAcyclic $
  plam $ \stakeDatum ->
    let locks :: Term _ (PBuiltinList (PAsData PProposalLock))
        locks = pfield @"lockedBy" # stakeDatum
     in pnotNull # locks

{- | Find a stake owned by a particular PK.

     @since 0.1.0
-}
findStakeOwnedBy ::
  Term
    s
    ( PAssetClass
        :--> PPubKeyHash
        :--> PBuiltinList (PAsData (PTuple PDatumHash PDatum))
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PMaybe (PAsData PStakeDatum)
    )
findStakeOwnedBy = phoistAcyclic $
  plam $ \ac pk datums inputs ->
    pmatch (pfind # (isInputStakeOwnedBy # ac # pk # datums) # inputs) $ \case
      PNothing -> pcon PNothing
      PJust (pfromData -> v) -> unTermCont $ do
        let txOut = pfield @"resolved" # pto v
        txOutF <- pletFieldsC @'["datumHash"] $ txOut
        pure $
          pmatch txOutF.datumHash $ \case
            PDNothing _ -> pcon PNothing
            PDJust ((pfield @"_0" #) -> dh) ->
              ptryFindDatum @(PAsData PStakeDatum) # dh # datums

{- | Check if a StakeDatum  is owned by a particular public key.

   @since 0.1.0
-}
stakeDatumOwnedBy :: Term _ (PPubKeyHash :--> PStakeDatum :--> PBool)
stakeDatumOwnedBy =
  phoistAcyclic $
    plam $ \pk stakeDatum ->
      pletFields @'["owner"] (pto stakeDatum) $ \stakeDatumF ->
        stakeDatumF.owner #== pdata pk

{- | Does the input have a `Stake` owned by a particular PK?

     @since 0.1.0
-}
isInputStakeOwnedBy ::
  Term
    _
    ( PAssetClass :--> PPubKeyHash
        :--> PBuiltinList (PAsData (PTuple PDatumHash PDatum))
        :--> PAsData PTxInInfo
        :--> PBool
    )
isInputStakeOwnedBy =
  plam $ \ac ss datums txInInfo' -> unTermCont $ do
    PTxInInfo ((pfield @"resolved" #) -> txOut) <- pmatchC $ pfromData txInInfo'
    PTxOut txOut' <- pmatchC txOut
    txOutF <- pletFieldsC @'["value", "datumHash"] txOut'
    outStakeST <- pletC $ passetClassValueOf # txOutF.value # ac
    pure $
      pmatch txOutF.datumHash $ \case
        PDNothing _ -> pcon PFalse
        PDJust ((pfield @"_0" #) -> datumHash) ->
          pif
            (outStakeST #== 1)
            ( pmatch (ptryFindDatum @(PAsData PStakeDatum) # datumHash # datums) $ \case
                PNothing -> pcon PFalse
                PJust v -> stakeDatumOwnedBy # ss # pfromData (punsafeCoerce v)
            )
            (pcon PFalse)

{- | Represent the usage of a stake on a particular proposal.
     A stake can be used to either create or vote on a proposal.

     @since 0.1.0
-}
data PStakeUsage (s :: S)
  = PVotedFor (Term s PResultTag)
  | PCreated
  | PDidNothing
  deriving stock
    ( -- | @since 0.1.0
      GHC.Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      Generic
    , -- | @since 0.1.0
      PlutusType
    , -- | @since 0.1.0
      HasDatatypeInfo
    , -- | @since 0.1.0
      PEq
    )

{- | / O(n) /.Return the usage of a stake on a particular proposal,
     given the 'lockedBy' field of a stake and the target proposal.

     @since 0.1.0
-}
pgetStakeUsage ::
  Term
    _
    ( PBuiltinList (PAsData PProposalLock)
        :--> PProposalId
        :--> PStakeUsage
    )
pgetStakeUsage = phoistAcyclic $
  plam $ \locks pid ->
    let -- All locks from the given proposal.
        filteredLocks =
          pmapMaybe
            # plam
              ( \lock'@(pfromData -> lock) -> unTermCont $ do
                  lockF <- pletFieldsC @'["proposalTag"] lock

                  pure $
                    pif
                      (lockF.proposalTag #== pid)
                      (pcon $ PJust lock')
                      (pcon PNothing)
              )
            # locks

        lockCount' = plength # filteredLocks
     in plet lockCount' $ \lockCount ->
          pif (lockCount #== 0) (pcon PDidNothing) $
            pif
              (lockCount #== 1)
              ( pcon $
                  PVotedFor $
                    pfromData $
                      pfield @"vote" #$ phead # filteredLocks
              )
              -- Note: see the implementation of the governor.
              (pcon PCreated)
