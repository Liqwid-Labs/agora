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
  ProposalTimingConfig (..),
  ProposalStartingTime (..),
  MaxTimeRangeWidth (..),

  -- * Plutarch-land
  PProposalTime (..),
  PProposalTimingConfig (..),
  PProposalStartingTime (..),
  PMaxTimeRangeWidth (..),

  -- * Compute periods given config and starting time.
  createProposalStartingTime,
  currentProposalTime,
  isDraftPeriod,
  isVotingPeriod,
  isLockingPeriod,
  isExecutionPeriod,
) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, HasDatatypeInfo, I (I))
import Plutarch.Api.V1 (
  PExtended (PFinite),
  PInterval (PInterval),
  PLowerBound (PLowerBound),
  PPOSIXTime,
  PPOSIXTimeRange,
  PUpperBound (PUpperBound),
 )
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
  PIsDataReprInstances (..),
 )
import Plutarch.Extra.TermCont (pguardC, pletFieldsC, pmatchC)
import Plutarch.Lift (
  DerivePConstantViaNewtype (..),
  PConstantDecl,
  PUnsafeLiftDecl (..),
 )
import Plutarch.Numeric.Additive (AdditiveSemigroup ((+)))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1.Time (POSIXTime)
import PlutusTx qualified
import Prelude hiding ((+))

--------------------------------------------------------------------------------

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

-- | Represents the maximum width of a 'PlutusLedgerApi.V1.Time.POSIXTimeRange'.
newtype MaxTimeRangeWidth = MaxTimeRangeWidth {getMaxWidth :: POSIXTime}
  deriving stock (Eq, Show, Ord, GHC.Generic)
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

--------------------------------------------------------------------------------

{- | == Establishing timing in Proposal interactions.

   In Plutus, it's impossible to determine time exactly. It's also impossible
   to get a single point in time, yet often we need to check
   various constraints on time.

   For the purposes of proposals, there's a single most important feature:
   The ability to determine if we can perform an action. In order to correctly
   determine if we are able to perform certain actions, we need to know what
   time it roughly is, compared to when the proposal was created.

   'PProposalTime' represents "the time according to the proposal".
   Its representation is opaque, and doesn't matter.

   Various functions work simply on 'PProposalTime' and 'ProposalTimingConfig'.
   In particular, 'currentProposalTime' is useful for extracting the time
   from the 'PlutusLedgerApi.V1.txInfoValidPeriod' field
   of 'PlutusLedgerApi.V1.TxInfo'.

   We avoid 'PPOSIXTimeRange' where we can in order to save on operations.

   Note: 'PProposalTime' doesn't need a Haskell-level equivalent because it
   is only used in scripts, and does not go in datums. It is also scott-encoded
   which is more efficient in usage.
-}
data PProposalTime (s :: S) = PProposalTime
  { lowerBound :: Term s PPOSIXTime
  , upperBound :: Term s PPOSIXTime
  }
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType, HasDatatypeInfo, PEq)

-- | Plutarch-level version of 'ProposalStartingTime'.
newtype PProposalStartingTime (s :: S) = PProposalStartingTime (Term s PPOSIXTime)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PProposalStartingTime PPOSIXTime)

instance PUnsafeLiftDecl PProposalStartingTime where
  type PLifted PProposalStartingTime = ProposalStartingTime
deriving via
  (DerivePConstantViaNewtype ProposalStartingTime PProposalStartingTime PPOSIXTime)
  instance
    (PConstantDecl ProposalStartingTime)

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

instance PUnsafeLiftDecl PProposalTimingConfig where
  type PLifted PProposalTimingConfig = ProposalTimingConfig
deriving via
  (DerivePConstantViaData ProposalTimingConfig PProposalTimingConfig)
  instance
    (PConstantDecl ProposalTimingConfig)

-- | Plutarch-level version of 'MaxTimeRangeWidth'.
newtype PMaxTimeRangeWidth (s :: S)
  = PMaxTimeRangeWidth (Term s PPOSIXTime)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PMaxTimeRangeWidth PPOSIXTime)

instance PUnsafeLiftDecl PMaxTimeRangeWidth where type PLifted PMaxTimeRangeWidth = MaxTimeRangeWidth
deriving via
  (DerivePConstantViaNewtype MaxTimeRangeWidth PMaxTimeRangeWidth PPOSIXTime)
  instance
    (PConstantDecl MaxTimeRangeWidth)

--------------------------------------------------------------------------------

-- FIXME: Orphan instance, move this to plutarch-extra.
instance AdditiveSemigroup (Term s PPOSIXTime) where
  (punsafeCoerce @_ @_ @PInteger -> x) + (punsafeCoerce @_ @_ @PInteger -> y) = punsafeCoerce $ x + y

{- | Get the starting time of a proposal, from the 'PlutusLedgerApi.V1.txInfoValidPeriod' field.
     For every proposal, this is only meant to run once upon creation. Given time range should be
     tight enough, meaning that the width of the time range should be less than the maximum value.
-}
createProposalStartingTime :: forall (s :: S). Term s (PMaxTimeRangeWidth :--> PPOSIXTimeRange :--> PProposalStartingTime)
createProposalStartingTime = phoistAcyclic $
  plam $ \(pto -> maxDuration) iv -> unTermCont $ do
    currentTimeF <- pmatchC $ currentProposalTime # iv

    -- Use the middle of the current time range as the starting time.
    let duration = currentTimeF.upperBound - currentTimeF.lowerBound

        startingTime =
          pdiv
            # (currentTimeF.lowerBound + currentTimeF.upperBound)
            # 2

    pguardC "createProposalStartingTime: given time range should be tight enough" $
      duration #<= maxDuration

    pure $ pcon $ PProposalStartingTime startingTime

{- | Get the current proposal time, from the 'PlutusLedgerApi.V1.txInfoValidPeriod' field.

 If it's impossible to get a fully-bounded time, (e.g. either end of the 'PPOSIXTimeRange' is
 an infinity) then we error out.
-}
currentProposalTime :: forall (s :: S). Term s (PPOSIXTimeRange :--> PProposalTime)
currentProposalTime = phoistAcyclic $
  plam $ \iv -> unTermCont $ do
    PInterval iv' <- pmatchC iv
    ivf <- pletFieldsC @'["from", "to"] iv'
    PLowerBound lb <- pmatchC ivf.from
    PUpperBound ub <- pmatchC ivf.to
    lbf <- pletFieldsC @'["_0", "_1"] lb
    ubf <- pletFieldsC @'["_0", "_1"] ub
    pure $
      pcon $
        PProposalTime
          { lowerBound =
              pmatch
                lbf._0
                ( \case
                    PFinite ((pfield @"_0" #) -> d) -> d
                    _ -> ptraceError "currentProposalTime: Can't get fully-bounded proposal time."
                )
          , upperBound =
              pmatch
                ubf._0
                ( \case
                    PFinite ((pfield @"_0" #) -> d) -> d
                    _ -> ptraceError "currentProposalTime: Can't get fully-bounded proposal time."
                )
          }

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
  plam $ \l h proposalTime' -> unTermCont $ do
    PProposalTime ut lt <- pmatchC proposalTime'
    pure $
      foldr1
        (#&&)
        [ l #<= lt
        , ut #<= h
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
    proposalTimeWithin # s # (s + (pfield @"draftTime" # config))

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
