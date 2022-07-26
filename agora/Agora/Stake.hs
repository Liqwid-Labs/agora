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

import Agora.Plutarch.Orphans ()
import Agora.Proposal (PProposalId, PResultTag, ProposalId (..), ResultTag (..))
import Agora.SafeMoney (GTTag)
import Data.Tagged (Tagged (..))
import GHC.Generics qualified as GHC
import Generics.SOP (Generic, HasDatatypeInfo, I (I))
import Plutarch.Api.V1 (
  PPubKeyHash,
 )
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Extra.Field (pletAll)
import Plutarch.Extra.IsData (
  DerivePConstantViaDataList (..),
  ProductIsData (ProductIsData),
 )
import Plutarch.Extra.List (pnotNull)
import Plutarch.Extra.Other (DerivePNewtype' (..))
import Plutarch.Extra.Sum (PSum (..))
import Plutarch.Extra.Traversable (pfoldMap)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.SafeMoney (PDiscrete)
import Plutarch.Show (PShow (..))
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
      GHC.Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
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
  --   This also acts as the voting weight for 'Agora.Proposal.Proposal's.
  , owner :: PubKeyHash
  -- ^ The hash of the public key this stake belongs to.
  --
  -- TODO Support for MultiSig/Scripts is tracked here:
  --      https://github.com/Liqwid-Labs/agora/issues/45
  , lockedBy :: [ProposalLock]
  -- ^ The current proposals locking this stake. This field must be empty
  --   for the stake to be usable for deposits and withdrawals.
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
instance Plutarch.Lift.PUnsafeLiftDecl PStakeDatum where
  type PLifted PStakeDatum = StakeDatum

-- | @since 0.1.0
deriving via
  (DerivePConstantViaDataList StakeDatum PStakeDatum)
  instance
    (Plutarch.Lift.PConstantDecl StakeDatum)

-- | @since 0.1.0
deriving via
  PAsData (DerivePNewtype' PStakeDatum)
  instance
    PTryFrom PData (PAsData PStakeDatum)

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

-- | @since 0.1.0
deriving via
  PAsData (PIsDataReprInstances PStakeRedeemer)
  instance
    PTryFrom PData (PAsData PStakeRedeemer)

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
  = PCreated (Term s (PDataRecord '["created" ':= PProposalId]))
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
      GHC.Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      Generic
    , -- | @since 0.1.0
      HasDatatypeInfo
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
      PEq
    )
    via (PIsDataReprInstances PProposalLock)

-- | @since 0.1.0
deriving via
  PAsData (PIsDataReprInstances PProposalLock)
  instance
    PTryFrom PData (PAsData PProposalLock)

-- | @since 0.1.0
instance Plutarch.Lift.PUnsafeLiftDecl PProposalLock where
  type PLifted PProposalLock = ProposalLock

-- | @since 0.1.0
deriving via
  (DerivePConstantViaData ProposalLock PProposalLock)
  instance
    (Plutarch.Lift.PConstantDecl ProposalLock)

-- | @since 0.2.0
instance PShow PProposalLock where
  pshow' :: Bool -> Term s PProposalLock -> Term s PString
  pshow' True x = "(" <> pshow' False x <> ")"
  pshow' False lock = pmatch lock $ \case
    PCreated ((pfield @"created" #) -> pid) -> "PCreated " <> pshow' True pid
    PVoted x -> pletFields @'["votedOn", "votedFor"] x $ \xF ->
      "PVoted " <> pshow' True xF.votedOn <> " " <> pshow' True xF.votedFor

--------------------------------------------------------------------------------

{- | Check whether a Stake is locked. If it is locked, various actions are unavailable.

     @since 0.2.0
-}
pstakeLocked :: forall (s :: S). Term s (PStakeDatum :--> PBool)
pstakeLocked = phoistAcyclic $
  plam $ \stakeDatum ->
    let locks :: Term _ (PBuiltinList (PAsData PProposalLock))
        locks = pfield @"lockedBy" # stakeDatum
     in pnotNull # locks

{- | Get the number of *alive* proposals that were created by the given stake.

     @since 0.2.0
-}
pnumCreatedProposals :: Term s (PBuiltinList (PAsData PProposalLock) :--> PInteger)
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
      GHC.Generic
    )
  deriving anyclass
    ( -- | @since 0.2.0
      Generic
    , -- | @since 0.2.0
      PlutusType
    , -- | @since 0.2.0
      HasDatatypeInfo
    , -- | @since 0.2.0
      PEq
    )

{- | Retutn true if the stake was used to voted on the proposal.

     @since 0.2.0
-}
pisVoter :: Term s (PStakeRole :--> PBool)
pisVoter = phoistAcyclic $
  plam $ \sr -> pmatch sr $ \case
    PVoter _ -> pconstant True
    PBoth _ -> pconstant True
    _ -> pconstant False

{- | Retutn true if the stake was used to create the proposal.

     @since 0.2.0
-}
pisCreator :: Term s (PStakeRole :--> PBool)
pisCreator = phoistAcyclic $
  plam $ \sr -> pmatch sr $ \case
    PCreator -> pconstant True
    PBoth _ -> pconstant True
    _ -> pconstant False

{- | Retutn true if the stake was used to create the proposal, but not vote on
     the proposal.

     @since 0.2.0
-}
pisPureCreator :: Term s (PStakeRole :--> PBool)
pisPureCreator = phoistAcyclic $
  plam $ \sr -> pmatch sr $ \case
    PCreator -> pconstant True
    _ -> pconstant False

{- | Return true if the stake isn't related to the proposal.

     @since 0.2.0
-}
pisIrrelevant :: Term s (PStakeRole :--> PBool)
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
pgetStakeRole :: Term s (PProposalId :--> PBuiltinList (PAsData PProposalLock) :--> PStakeRole)
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
    pcombineStakeRole :: Term s (PStakeRole :--> PStakeRole :--> PStakeRole)
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
pextractVoteOption :: Term s (PStakeRole :--> PResultTag)
pextractVoteOption = phoistAcyclic $
  plam $ \sr -> pmatch sr $ \case
    PVoter r -> r
    PBoth r -> r
    _ -> ptraceError "not voter"
