{-# LANGUAGE TemplateHaskell #-}

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
  PProposalTime,
  PProposalTimingConfig (..),
  PProposalStartingTime (..),
  PMaxTimeRangeWidth (..),

  -- * Compute periods given config and starting time.
  validateProposalStartingTime,
  currentProposalTime,
  isDraftPeriod,
  isVotingPeriod,
  isLockingPeriod,
  isExecutionPeriod,
  pisProposalTimingConfigValid,
  pisMaxTimeRangeWidthValid,
) where

import Agora.Utils (pcurrentTimeDuration)
import Control.Composition ((.*))
import Optics.TH (makeFieldLabelsNoPrefix)
import Plutarch.Api.V1 (
  PExtended (PFinite),
  PInterval (PInterval),
  PLowerBound (PLowerBound),
  PPOSIXTime,
  PUpperBound (PUpperBound),
 )
import Plutarch.Api.V2 (PPOSIXTimeRange)
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
 )
import Plutarch.Extra.Applicative (PApply (pliftA2))
import Plutarch.Extra.Field (pletAll, pletAllC)
import Plutarch.Extra.Maybe (pjust, pmaybe, pnothing)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Extra.Time (
  PCurrentTime (PCurrentTime),
  pisCurrentTimeWithin,
  pisWithinCurrentTime,
 )
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
import PlutusLedgerApi.V1 (POSIXTime)
import PlutusTx qualified
import Prelude

--------------------------------------------------------------------------------

{- | Represents the starting time of the proposal.

     @since 0.1.0
-}
newtype ProposalStartingTime = ProposalStartingTime
  { getProposalStartingTime :: POSIXTime
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
    , -- | @since 0.1.0
      PlutusTx.UnsafeFromData
    )

makeFieldLabelsNoPrefix ''ProposalStartingTime

{- | Configuration of proposal timings.

     See: https://liqwid.notion.site/Proposals-589853145a994057aa77f397079f75e4#d25ea378768d4c76b52dd4c1b6bc0fcd

     @since 0.1.0
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
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )

makeFieldLabelsNoPrefix ''ProposalTimingConfig
PlutusTx.makeIsDataIndexed 'ProposalTimingConfig [('ProposalTimingConfig, 0)]

-- | Represents the maximum width of a 'PlutusLedgerApi.V1.Time.POSIXTimeRange'.
newtype MaxTimeRangeWidth = MaxTimeRangeWidth {getMaxWidth :: POSIXTime}
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

     @since 0.1.0
-}
type PProposalTime = PCurrentTime

-- | Plutarch-level version of 'ProposalStartingTime'.
newtype PProposalStartingTime (s :: S) = PProposalStartingTime (Term s PPOSIXTime)
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

instance DerivePlutusType PProposalStartingTime where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 0.1.0
instance PUnsafeLiftDecl PProposalStartingTime where
  type PLifted PProposalStartingTime = ProposalStartingTime

instance PTryFrom PData (PAsData PProposalStartingTime)

-- | @since 0.1.0
deriving via
  (DerivePConstantViaNewtype ProposalStartingTime PProposalStartingTime PPOSIXTime)
  instance
    (PConstantDecl ProposalStartingTime)

{- | Plutarch-level version of 'ProposalTimingConfig'.

     @since 0.1.0
-}
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

instance DerivePlutusType PProposalTimingConfig where
  type DPTStrat _ = PlutusTypeData

-- | @since 0.1.0
instance PTryFrom PData PProposalTimingConfig

-- | @since 0.1.0
instance PUnsafeLiftDecl PProposalTimingConfig where
  type PLifted PProposalTimingConfig = ProposalTimingConfig

-- | @since 0.1.0
deriving via
  (DerivePConstantViaData ProposalTimingConfig PProposalTimingConfig)
  instance
    (PConstantDecl ProposalTimingConfig)

-- | Plutarch-level version of 'MaxTimeRangeWidth'.
newtype PMaxTimeRangeWidth (s :: S)
  = PMaxTimeRangeWidth (Term s PPOSIXTime)
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
    )

instance DerivePlutusType PMaxTimeRangeWidth where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 0.1.0
instance PTryFrom PData (PAsData PMaxTimeRangeWidth)

-- | @since 0.1.0
instance PUnsafeLiftDecl PMaxTimeRangeWidth where type PLifted PMaxTimeRangeWidth = MaxTimeRangeWidth

-- | @since 0.1.0
deriving via
  (DerivePConstantViaNewtype MaxTimeRangeWidth PMaxTimeRangeWidth PPOSIXTime)
  instance
    (PConstantDecl MaxTimeRangeWidth)

--------------------------------------------------------------------------------

{- | Return true if the timing configuration is valid.

     @since 0.2.0
-}
pisProposalTimingConfigValid :: forall (s :: S). Term s (PProposalTimingConfig :--> PBool)
pisProposalTimingConfigValid = phoistAcyclic $
  plam $ \conf -> unTermCont $ do
    confF <- pletAllC conf

    -- everything is greater or equal 0
    pure $
      ptraceIfFalse "ge 0" $
        foldr
          ( \t ->
              (#&&)
                ( pconstant 0
                    #<= pfromData t
                )
          )
          (pconstant True)
          [ getField @"draftTime" confF
          , getField @"votingTime" confF
          , getField @"lockingTime" confF
          , getField @"executingTime" confF
          ]

{- | Return true if the maximum time width is greater than 0.

     @since 0.2.0
-}
pisMaxTimeRangeWidthValid :: Term s (PMaxTimeRangeWidth :--> PBool)
pisMaxTimeRangeWidthValid =
  phoistAcyclic $
    plam $
      ptraceIfFalse "greater than 0"
        . (pconstant (MaxTimeRangeWidth 0) #<)

{- | Validate starting time of a proposal, from the 'PlutusLedgerApi.V1.txInfoValidPeriod' field.
     For every proposal, this is only meant to run once upon creation. Given time range should be
     tight enough, meaning that the width of the time range should be less than the maximum value.

     @since 1.0.0
-}
validateProposalStartingTime ::
  forall (s :: S).
  Term
    s
    ( PMaxTimeRangeWidth
        :--> PPOSIXTimeRange
        :--> PProposalStartingTime
        :--> PBool
    )
validateProposalStartingTime = phoistAcyclic $
  plam $ \(pto -> maxDuration) iv (pto -> st) ->
    pmaybe
      # ptrace
        "validateProposalStartingTime: unable to get current time"
        (pconstant False)
      # plam
        ( \ct ->
            let duration = pcurrentTimeDuration # ct
                isTightEnough =
                  ptraceIfFalse
                    "createProposalStartingTime: given time range should be tight enough"
                    $ duration #<= maxDuration
                isInCurrentTimeRange =
                  ptraceIfFalse
                    "createProposalStartingTime: starting time should be in current time range"
                    $ pisWithinCurrentTime # st # ct
             in isTightEnough #&& isInCurrentTimeRange
        )
      # (currentProposalTime # iv)

{- | Get the current proposal time, given the 'PlutusLedgerApi.V1.txInfoValidPeriod' field.

     If it's impossible to get a fully-bounded time, (e.g. either end of the 'PPOSIXTimeRange' is
     an infinity) then we return nothing.

     Note that we ignore the inclusiveness of the upper bound. Due to the fact
      that there's no place in the Cardano domain transaction type to store the
      inclusiveness information, we can never get a time range with closed upper
      bound. See also the ledger implementation: https://bit.ly/3BDzW5R

     @since 0.1.0
-}
currentProposalTime :: forall (s :: S). Term s (PPOSIXTimeRange :--> PMaybe PProposalTime)
currentProposalTime = phoistAcyclic $
  plam $ \iv -> unTermCont $ do
    PInterval iv' <- pmatchC iv
    ivf <- pletAllC iv'
    PLowerBound lb <- pmatchC (getField @"from" ivf)
    PUpperBound ub <- pmatchC (getField @"to" ivf)

    let lowerBound = pletAll lb $ \f ->
          pif
            (getField @"_1" f)
            ( pmatch (getField @"_0" f) $ \case
                PFinite (pfromData . (pfield @"_0" #) -> d) -> pjust # d
                _ -> ptrace "currentProposalTime: time range should be bounded" pnothing
            )
            (ptrace "currentProposalTime: lower bound of the time range should be inclusive" pnothing)

        upperBound = pletAll ub $ \f ->
          pmatch (getField @"_0" f) $ \case
            PFinite (pfromData . (pfield @"_0" #) -> d) -> pjust # d
            _ -> ptrace "currentProposalTime: time range should be bounded" pnothing

        mkTime = phoistAcyclic $ plam $ pcon .* PCurrentTime
    pure $ pliftA2 # mkTime # lowerBound # upperBound

{- | True if the 'PProposalTime' is in the draft period.

     @since 0.1.0
-}
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
    pisCurrentTimeWithin # s # (s + (pfield @"draftTime" # config))

{- | True if the 'PProposalTime' is in the voting period.

     @since 0.1.0
-}
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
      pisCurrentTimeWithin # s # (s + getField @"draftTime" f + getField @"votingTime" f)

{- | True if the 'PProposalTime' is in the locking period.

     @since 0.1.0
-}
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
      pisCurrentTimeWithin # s # (s + getField @"draftTime" f + getField @"votingTime" f + getField @"lockingTime" f)

{- | True if the 'PProposalTime' is in the execution period.

     @since 0.1.0
-}
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
      pisCurrentTimeWithin # s
        # (s + getField @"draftTime" f + getField @"votingTime" f + getField @"lockingTime" f + getField @"executingTime" f)
