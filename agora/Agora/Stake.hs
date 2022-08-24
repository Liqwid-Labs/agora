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
  ProposalLock (..),

  -- * Plutarch-land
  PStakeDatum (..),
  PStakeRedeemer (..),
  PProposalLock (..),
  PStakeRole (..),

  -- * Utility functions
  pstakeLocked,
  pnumCreatedProposals,
  pextractVoteOption,
  pgetStakeRole,
  pisVoter,
  pisCreator,
  pisPureCreator,
  pisIrrelevant,
) where

import Agora.Proposal (PProposalId, PResultTag, ProposalId, ResultTag)
import Agora.SafeMoney (GTTag)
import Data.Tagged (Tagged)
import Generics.SOP qualified as SOP
import Plutarch.Api.V1 (PCredential)
import Plutarch.Api.V2 (
  PMaybeData,
 )
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
 )
import Plutarch.Extra.Field (pletAll)
import Plutarch.Extra.IsData (
  DerivePConstantViaDataList (DerivePConstantViaDataList),
  ProductIsData (ProductIsData),
 )
import Plutarch.Extra.List (pnotNull)
import Plutarch.Extra.Sum (PSum (PSum))
import Plutarch.Extra.Traversable (pfoldMap)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Orphans ()
import Plutarch.SafeMoney (PDiscrete)
import PlutusLedgerApi.V2 (Credential)
import PlutusTx qualified
import Prelude hiding (Num (..))

--------------------------------------------------------------------------------

{- | Locks that are stored in the stake datums for various purposes.

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
data ProposalLock
  = -- | The stake was used to create a proposal.
    --
    --   This kind of lock is placed upon the creation of a proposal, in order
    --    to limit creation of proposals per stake.
    --
    --   See also: https://github.com/Liqwid-Labs/agora/issues/68
    --
    --   @since 0.2.0
    Created
      ProposalId
      -- ^ The identifier of the proposal.
  | -- | The stake was used to vote on a proposal.
    --
    --   This kind of lock is placed while voting on a proposal, in order to
    --    prevent depositing and withdrawing when votes are in place.
    --
    --   @since 0.2.0
    Voted
      ProposalId
      -- ^ The identifier of the proposal.
      ResultTag
      -- ^ The option which was voted on. This allows votes to be retracted.
  deriving stock
    ( -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )

PlutusTx.makeIsDataIndexed
  ''ProposalLock
  [ ('Created, 0)
  , ('Voted, 1)
  ]

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
    PermitVote
  | -- | Retract a vote, removing it from the 'lockedBy' field. See 'ProposalLock'.
    --   This action checks for permission of the 'Agora.Proposal.Proposal'. Finished proposals are
    --   always allowed to have votes retracted and won't affect the Proposal datum,
    --   allowing 'Stake's to be unlocked.
    RetractVotes
  | -- | The owner can consume stake if nothing is changed about it.
    --   If the proposal token moves, this is equivalent to the owner consuming it.
    WitnessStake
  | -- | The owner can delegate the stake to another user, allowing the
    --    delegate to vote on prooposals with the stake.
    DelegateTo Credential
  | -- | Revoke the existing delegation.
    ClearDelegate
  deriving stock
    ( -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )

PlutusTx.makeIsDataIndexed
  ''StakeRedeemer
  [ ('DepositWithdraw, 0)
  , ('Destroy, 1)
  , ('PermitVote, 2)
  , ('RetractVotes, 3)
  , ('WitnessStake, 4)
  , ('DelegateTo, 5)
  , ('ClearDelegate, 6)
  ]

{- | Haskell-level datum for Stake scripts.

     @since 0.1.0
-}
data StakeDatum = StakeDatum
  { stakedAmount :: Tagged GTTag Integer
  -- ^ Tracks the amount of governance token staked in the datum.
  --   This also acts as the voting weight for 'Agora.Proposal.Proposal's.
  , owner :: Credential
  -- ^ The hash of the public key this stake belongs to.
  --
  -- TODO Support for MultiSig/Scripts is tracked here:
  --      https://github.com/Liqwid-Labs/agora/issues/45
  , delegatedTo :: Maybe Credential
  -- ^ To whom this stake has been delegated.
  , lockedBy :: [ProposalLock]
  -- ^ The current proposals locking this stake. This field must be empty
  --   for the stake to be usable for deposits and withdrawals.
  }
  deriving stock
    ( -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      SOP.Generic
    )
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
             , "owner" ':= PCredential
             , "delegatedTo" ':= PMaybeData (PAsData PCredential)
             , "lockedBy" ':= PBuiltinList (PAsData PProposalLock)
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
    )

instance DerivePlutusType PStakeDatum where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 0.1.0
instance Plutarch.Lift.PUnsafeLiftDecl PStakeDatum where
  type PLifted PStakeDatum = StakeDatum

-- | @since 0.1.0
deriving via
  (DerivePConstantViaDataList StakeDatum PStakeDatum)
  instance
    (Plutarch.Lift.PConstantDecl StakeDatum)

-- | @since 0.1.0
instance PTryFrom PData (PAsData PStakeDatum)

{- | Plutarch-level redeemer for Stake scripts.

     @since 0.1.0
-}
data PStakeRedeemer (s :: S)
  = -- | Deposit or withdraw a discrete amount of the staked governance token.
    PDepositWithdraw (Term s (PDataRecord '["delta" ':= PDiscrete GTTag]))
  | -- | Destroy a stake, retrieving its LQ, the minimum ADA and any other assets.
    PDestroy (Term s (PDataRecord '[]))
  | PPermitVote (Term s (PDataRecord '[]))
  | PRetractVotes (Term s (PDataRecord '[]))
  | PWitnessStake (Term s (PDataRecord '[]))
  | PDelegateTo (Term s (PDataRecord '["pkh" ':= PCredential]))
  | PClearDelegate (Term s (PDataRecord '[]))
  deriving stock
    ( -- | @since 0.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      SOP.Generic
    , -- | @since 0.1.0
      PlutusType
    , -- | @since 0.1.0
      PIsData
    )

instance DerivePlutusType PStakeRedeemer where
  type DPTStrat _ = PlutusTypeData

-- | @since 0.1.0
instance PTryFrom PData PStakeRedeemer

-- | @since 0.1.0
instance Plutarch.Lift.PUnsafeLiftDecl PStakeRedeemer where
  type PLifted PStakeRedeemer = StakeRedeemer

-- | @since 0.1.0
deriving via
  (DerivePConstantViaData StakeRedeemer PStakeRedeemer)
  instance
    (Plutarch.Lift.PConstantDecl StakeRedeemer)

{- | Plutarch-level version of 'ProposalLock'.

     @since 0.2.0
-}
data PProposalLock (s :: S)
  = PCreated
      ( Term
          s
          ( PDataRecord
              '["created" ':= PProposalId]
          )
      )
  | PVoted
      ( Term
          s
          ( PDataRecord
              '[ "votedOn" ':= PProposalId
               , "votedFor" ':= PResultTag
               ]
          )
      )
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
    , -- | @since 0.2.0
      PShow
    )

instance DerivePlutusType PProposalLock where
  type DPTStrat _ = PlutusTypeData

-- | @since 0.1.0
instance PTryFrom PData PProposalLock

-- | @since 0.2.0
instance PTryFrom PData (PAsData PProposalLock)

-- | @since 0.1.0
instance Plutarch.Lift.PUnsafeLiftDecl PProposalLock where
  type PLifted PProposalLock = ProposalLock

-- | @since 0.1.0
deriving via
  (DerivePConstantViaData ProposalLock PProposalLock)
  instance
    (Plutarch.Lift.PConstantDecl ProposalLock)

--------------------------------------------------------------------------------

{- | Check whether a Stake is locked. If it is locked, various actions are unavailable.

     @since 0.2.0
-}
pstakeLocked :: forall (s :: S). Term s (PStakeDatum :--> PBool)
pstakeLocked = phoistAcyclic $
  plam $ \stakeDatum ->
    pnotNull #$ pfield @"lockedBy" @(PBuiltinList _) # pto stakeDatum

{- | Get the number of *alive* proposals that were created by the given stake.

     @since 0.2.0
-}
pnumCreatedProposals ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PProposalLock) :--> PInteger)
pnumCreatedProposals =
  phoistAcyclic $
    plam $ \l ->
      pto $
        pfoldMap
          # plam
            ( \(pfromData -> lock) -> pmatch lock $ \case
                PCreated _ -> pcon $ PSum 1
                _ -> mempty
            )
          # l

{- | The role of a stake for a particular proposal. Scott-encoded.

     @since 0.2.0
-}
data PStakeRole (s :: S)
  = -- | The stake was used to vote on the proposal.
    PVoter
      (Term s PResultTag)
      -- ^ The option which was voted for.
  | -- | The stake was used to create the proposal.
    PCreator
  | -- | The stake was used to both create and vote on the proposal.
    PBoth
      (Term s PResultTag)
      -- ^ The option which was voted for.
  | -- | The stake has nothing to do with the given proposal.
    PIrrelevant
  deriving stock
    ( -- | @since 0.2.0
      Generic
    )
  deriving anyclass
    ( -- | @since 0.2.0
      PlutusType
    , -- | @since 0.2.0
      PEq
    )

instance DerivePlutusType PStakeRole where
  type DPTStrat _ = PlutusTypeScott

{- | Retutn true if the stake was used to voted on the proposal.

     @since 0.2.0
-}
pisVoter :: forall (s :: S). Term s (PStakeRole :--> PBool)
pisVoter = phoistAcyclic $
  plam $ \sr -> pmatch sr $ \case
    PVoter _ -> pconstant True
    PBoth _ -> pconstant True
    _ -> pconstant False

{- | Retutn true if the stake was used to create the proposal.

     @since 0.2.0
-}
pisCreator :: forall (s :: S). Term s (PStakeRole :--> PBool)
pisCreator = phoistAcyclic $
  plam $ \sr -> pmatch sr $ \case
    PCreator -> pconstant True
    PBoth _ -> pconstant True
    _ -> pconstant False

{- | Retutn true if the stake was used to create the proposal, but not vote on
     the proposal.

     @since 0.2.0
-}
pisPureCreator :: forall (s :: S). Term s (PStakeRole :--> PBool)
pisPureCreator = phoistAcyclic $
  plam $ \sr -> pmatch sr $ \case
    PCreator -> pconstant True
    _ -> pconstant False

{- | Return true if the stake isn't related to the proposal.

     @since 0.2.0
-}
pisIrrelevant :: forall (s :: S). Term s (PStakeRole :--> PBool)
pisIrrelevant = phoistAcyclic $
  plam $ \sr -> pmatch sr $ \case
    PIrrelevant -> pconstant True
    _ -> pconstant False

{- | Get the role of a stake for the proposal specified by the poroposal id,
      given the 'StakeDatum.lockedBy' field of the stake.

     Note that the list of locks is cosidered valid only if it contains at most
      two locks from the given proposal: one voter lock and one creator lock.

     @since 0.2.0
-}
pgetStakeRole :: forall (s :: S). Term s (PProposalId :--> PBuiltinList (PAsData PProposalLock) :--> PStakeRole)
pgetStakeRole = phoistAcyclic $
  plam $ \pid locks ->
    pfoldl
      # plam
        ( \role (pfromData -> lock) ->
            let thisRole = pmatch lock $ \case
                  PCreated ((pfield @"created" #) -> pid') ->
                    pif
                      (pid' #== pid)
                      (pcon PCreator)
                      (pcon PIrrelevant)
                  PVoted lock' -> pletAll lock' $ \lockF ->
                    pif
                      (lockF.votedOn #== pid)
                      (pcon $ PVoter lockF.votedFor)
                      (pcon PIrrelevant)
             in pcombineStakeRole # thisRole # role
        )
      # pcon PIrrelevant
      # locks
  where
    pcombineStakeRole :: forall (s :: S). Term s (PStakeRole :--> PStakeRole :--> PStakeRole)
    pcombineStakeRole = phoistAcyclic $
      plam $ \x y ->
        let cannotCombine = ptraceError "duplicate roles"
         in pmatch x $ \case
              PVoter r -> pmatch y $ \case
                PCreator -> pcon $ PBoth r
                PIrrelevant -> x
                _ -> cannotCombine
              PCreator -> pmatch y $ \case
                PVoter r -> pcon $ PBoth r
                PIrrelevant -> x
                _ -> cannotCombine
              PBoth _ -> cannotCombine
              PIrrelevant -> y

{- | Get the outcome that was voted for.

     @since 0.2.0
-}
pextractVoteOption :: forall (s :: S). Term s (PStakeRole :--> PResultTag)
pextractVoteOption = phoistAcyclic $
  plam $ \sr -> pmatch sr $ \case
    PVoter r -> r
    PBoth r -> r
    _ -> ptraceError "not voter"
