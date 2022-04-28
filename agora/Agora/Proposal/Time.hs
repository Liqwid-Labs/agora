{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module     : Agora.Proposal.Time
Maintainer : emi@haskell.fyi
Description: Time functions for proposal phases.

Time functions for proposal phases.
-}
module Agora.Proposal.Time (
  -- * Haskell-land
  ProposalTime (..),
  ProposalTimingConfig (..),
  ProposalStartingTime (..),

  -- * Plutarch-land
  PProposalTime (..),
  PProposalTimingConfig (..),
  PProposalStartingTime (..),

  -- * Compute periods given config and starting time.
  currentProposalTime,
  isDraftPeriod,
  isVotingPeriod,
  isLockingPeriod,
  isExecutionPeriod,
) where

import Agora.Record (mkRecordConstr, (.&), (.=))
import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))
import Plutarch.Api.V1 (
  PExtended (PFinite),
  PInterval (PInterval),
  PLowerBound (PLowerBound),
  PPOSIXTime,
  PPOSIXTimeRange,
  PUpperBound (PUpperBound),
 )
import Plutarch.DataRepr (PDataFields, PIsDataReprInstances (..))
import Plutarch.Monadic qualified as P
import Plutarch.Numeric (AdditiveSemigroup ((+)))
import Plutarch.Unsafe (punsafeCoerce)
import Plutus.V1.Ledger.Time (POSIXTime)
import PlutusTx qualified
import Prelude hiding ((+))

--------------------------------------------------------------------------------

{- | == Establishing timing in Proposal interactions.

   In Plutus, it's impossible to determine time exactly. It's also impossible
   to get a single point in time, yet often we need to check
   various constraints on time.

   For the purposes of proposals, there's a single most important feature:
   The ability to determine if we can perform an action. In order to correctly
   determine if we are able to perform certain actions, we need to know what
   time it roughly is, compared to when the proposal was created.

   'ProposalTime' represents "the time according to the proposal".
   Its representation is opaque, and doesn't matter.

   Various functions work simply on 'ProposalTime' and 'ProposalTimingConfig'.
   In particular, 'currentProposalTime' is useful for extracting the time
   from the 'Plutus.V1.Ledger.Api.txInfoValidPeriod' field
   of 'Plutus.V1.Ledger.Api.TxInfo'.

   We avoid 'PPOSIXTimeRange' where we can in order to save on operations.
-}
data ProposalTime = ProposalTime
  { lowerBound :: POSIXTime
  , upperBound :: POSIXTime
  }
  deriving stock (Eq, Show, GHC.Generic)

PlutusTx.makeIsDataIndexed ''ProposalTime [('ProposalTime, 0)]

-- | Represents the starting time of the proposal.
newtype ProposalStartingTime = ProposalStartingTime
  { getProposalStartingTime :: POSIXTime
  }
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
  deriving stock (Eq, Show, GHC.Generic)

{- | Configuration of proposal timings.

 See: https://github.com/Liqwid-Labs/agora/blob/master/docs/tech-design/proposals.md#when-may-interactions-occur
-}
data ProposalTimingConfig = ProposalTimingConfig
  { draftTime :: POSIXTime
  -- ^ "D": the length of the draft period.
  , votingTime :: POSIXTime
  -- ^ "V": the length of the voting period.
  , lockingTime :: POSIXTime
  -- ^ "L": the length of the locking period.
  , executingTime :: POSIXTime
  -- ^ "E": the length of the execution period.
  }
  deriving stock (Eq, Show, GHC.Generic)

PlutusTx.makeIsDataIndexed ''ProposalTimingConfig [('ProposalTimingConfig, 0)]

--------------------------------------------------------------------------------

-- | Plutarch-level version of 'ProposalTime'.
newtype PProposalTime (s :: S)
  = PProposalTime
      ( Term
          s
          ( PDataRecord
              '[ "lowerBound" ':= PPOSIXTime
               , "upperBound" ':= PPOSIXTime
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via (PIsDataReprInstances PProposalTime)

-- | Plutarch-level version of 'ProposalStartingTime'.
newtype PProposalStartingTime (s :: S) = PProposalStartingTime (Term s PPOSIXTime)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PProposalStartingTime PPOSIXTime)

-- | Plutarch-level version of 'ProposalTimingConfig'.
newtype PProposalTimingConfig (s :: S) = PProposalTimingConfig
  { getProposalTimingConfig ::
    Term
      s
      ( PDataRecord
          '[ "draftTime" ':= PPOSIXTime
           , "votingTime" ':= PPOSIXTime
           , "lockingTime" ':= PPOSIXTime
           , "executingTime" ':= PPOSIXTime
           ]
      )
  }
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via (PIsDataReprInstances PProposalTimingConfig)

--------------------------------------------------------------------------------

-- FIXME: Orphan instance, move this to plutarch-extra.
instance AdditiveSemigroup (Term s PPOSIXTime) where
  (punsafeCoerce @_ @_ @PInteger -> x) + (punsafeCoerce @_ @_ @PInteger -> y) = punsafeCoerce $ x + y

{- | Get the current proposal time, from the 'Plutus.V1.Ledger.Api.txInfoValidPeriod' field.

 If it's impossible to get a fully-bounded time, (e.g. either end of the 'PPOSIXTimeRange' is
 an infinity) then we error out.
-}
currentProposalTime :: forall (s :: S). Term s (PPOSIXTimeRange :--> PProposalTime)
currentProposalTime = phoistAcyclic $
  plam $ \iv -> P.do
    PInterval iv' <- pmatch iv
    ivf <- pletFields @'["from", "to"] iv'
    PLowerBound lb <- pmatch ivf.from
    PUpperBound ub <- pmatch ivf.to
    lbf <- pletFields @'["_0", "_1"] lb
    ubf <- pletFields @'["_0", "_1"] ub
    mkRecordConstr PProposalTime $
      #lowerBound
        .= pmatch
          lbf._0
          ( \case
              PFinite ((pfield @"_0" #) -> d) -> d
              _ -> ptraceError "currentProposalTime: Can't get fully-bounded proposal time."
          )
        .& #upperBound
        .= pmatch
          ubf._0
          ( \case
              PFinite ((pfield @"_0" #) -> d) -> d
              _ -> ptraceError "currentProposalTime: Can't get fully-bounded proposal time."
          )

-- | Check if 'PProposalTime' is within two 'PPOSIXTime'. Inclusive.
proposalTimeWithin ::
  Term
    s
    ( PPOSIXTime
        :--> PPOSIXTime
        :--> PProposalTime
        :--> PBool
    )
proposalTimeWithin = phoistAcyclic $
  plam $ \l h proposalTime' -> P.do
    PProposalTime proposalTime <- pmatch proposalTime'
    ptf <- pletFields @'["lowerBound", "upperBound"] proposalTime
    foldr1
      (#&&)
      [ l #<= pfromData ptf.lowerBound
      , pfromData ptf.upperBound #<= h
      ]

-- | True if the 'PProposalTime' is in the draft period.
isDraftPeriod ::
  forall (s :: S).
  Term
    s
    ( PProposalTimingConfig
        :--> PProposalStartingTime
        :--> PProposalTime
        :--> PBool
    )
isDraftPeriod = phoistAcyclic $
  plam $ \config s' -> pmatch s' $ \(PProposalStartingTime s) ->
    proposalTimeWithin # s # (s + pfield @"draftTime" # config)

-- | True if the 'PProposalTime' is in the voting period.
isVotingPeriod ::
  forall (s :: S).
  Term
    s
    ( PProposalTimingConfig
        :--> PProposalStartingTime
        :--> PProposalTime
        :--> PBool
    )
isVotingPeriod = phoistAcyclic $
  plam $ \config s' -> pmatch s' $ \(PProposalStartingTime s) ->
    pletFields @'["draftTime", "votingTime"] config $ \f ->
      proposalTimeWithin # s # (s + f.draftTime + f.votingTime)

-- | True if the 'PProposalTime' is in the locking period.
isLockingPeriod ::
  forall (s :: S).
  Term
    s
    ( PProposalTimingConfig
        :--> PProposalStartingTime
        :--> PProposalTime
        :--> PBool
    )
isLockingPeriod = phoistAcyclic $
  plam $ \config s' -> pmatch s' $ \(PProposalStartingTime s) ->
    pletFields @'["draftTime", "votingTime", "lockingTime"] config $ \f ->
      proposalTimeWithin # s # (s + f.draftTime + f.votingTime + f.lockingTime)

-- | True if the 'PProposalTime' is in the execution period.
isExecutionPeriod ::
  forall (s :: S).
  Term
    s
    ( PProposalTimingConfig
        :--> PProposalStartingTime
        :--> PProposalTime
        :--> PBool
    )
isExecutionPeriod = phoistAcyclic $
  plam $ \config s' -> pmatch s' $ \(PProposalStartingTime s) ->
    pletFields @'["draftTime", "votingTime", "lockingTime", "executingTime"] config $ \f ->
      proposalTimeWithin # s
        # (s + f.draftTime + f.votingTime + f.lockingTime + f.executingTime)
