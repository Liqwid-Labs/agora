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
  PTimingRelation (..),
  PPeriod (..),

  -- * Compute periods given config and starting time.
  pvalidateProposalStartingTime,
  pcurrentProposalTime,
  pisProposalTimingConfigValid,
  pisMaxTimeRangeWidthValid,
  pgetRelation,
  pisWithin,
  psatisfyMaximumWidth,
) where

import Data.Functor ((<&>))
import Generics.SOP qualified as SOP
import Plutarch.Api.V1 (
  PExtended (PFinite),
  PInterval (PInterval),
  PLowerBound (PLowerBound),
  PPOSIXTime,
  PUpperBound (PUpperBound),
 )
import Plutarch.Api.V2 (PPOSIXTimeRange)
import Plutarch.DataRepr (
  PDataFields,
 )
import Plutarch.Extra.Applicative (PApply (pliftA2))
import Plutarch.Extra.Bool (passert)
import Plutarch.Extra.Field (pletAll, pletAllC)
import Plutarch.Extra.IsData (
  DerivePConstantViaDataList (DerivePConstantViaDataList),
  PlutusTypeEnumData,
  ProductIsData (ProductIsData),
 )
import Plutarch.Extra.Maybe (pjust, pmaybe, pnothing)
import Plutarch.Extra.Time (
  PFullyBoundedTimeRange (PFullyBoundedTimeRange),
  pisWithinTimeRange,
  ptimeRangeDuration,
 )
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Num (PNum)
import PlutusLedgerApi.V1 (POSIXTime)
import PlutusTx qualified
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pletC, pmatchC)

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
    )

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
    , -- | @since 1.0.0
      Num
    )

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
  , minStakeVotingTime :: POSIXTime
  -- ^ Minimum time from creating a voting lock until it can be destroyed.
  , votingTimeRangeMaxWidth :: MaxTimeRangeWidth
  -- ^ The maximum width of transaction time range while voting.
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
    ( -- | @since 1.0.0
      SOP.Generic
    )
  deriving
    ( -- | @since 1.0.0
      PlutusTx.ToData
    , -- | @since 1.0.0
      PlutusTx.FromData
    )
    via (ProductIsData ProposalTimingConfig)

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
type PProposalTime = PFullyBoundedTimeRange

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
             , "minStakeVotingTime" ':= PPOSIXTime
             , "votingTimeRangeMaxWidth" ':= PMaxTimeRangeWidth
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
    , -- | @since 0.2.1
      PShow
    )

instance DerivePlutusType PProposalTimingConfig where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 0.1.0
instance PTryFrom PData (PAsData PProposalTimingConfig)

-- | @since 0.1.0
instance PUnsafeLiftDecl PProposalTimingConfig where
  type PLifted PProposalTimingConfig = ProposalTimingConfig

-- | @since 0.1.0
deriving via
  (DerivePConstantViaDataList ProposalTimingConfig PProposalTimingConfig)
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
    , -- | @since 0.2.1
      PShow
    , -- | @since 1.0.0
      PNum
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
          [ confF.draftTime
          , confF.votingTime
          , confF.lockingTime
          , confF.executingTime
          , confF.minStakeVotingTime
          , pto confF.votingTimeRangeMaxWidth
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
pvalidateProposalStartingTime ::
  forall (s :: S).
  Term
    s
    ( PMaxTimeRangeWidth
        :--> PPOSIXTimeRange
        :--> PProposalStartingTime
        :--> PBool
    )
pvalidateProposalStartingTime = phoistAcyclic $
  plam $ \maxWidth iv (pto -> st) ->
    pmaybe
      # pconstant False
      # plam
        ( \ct ->
            let isTightEnough =
                  ptraceIfFalse
                    "createProposalStartingTime: given time range should be tight enough"
                    $ psatisfyMaximumWidth # maxWidth # ct
                isInCurrentTimeRange =
                  ptraceIfFalse
                    "createProposalStartingTime: starting time should be in current time range"
                    $ pisWithinTimeRange # st # ct
             in isTightEnough #&& isInCurrentTimeRange
        )
      # (pcurrentProposalTime # iv)

{- | Get the current proposal time, given the 'PlutusLedgerApi.V1.txInfoValidPeriod' field.

     If it's impossible to get a fully-bounded time, (e.g. either end of the 'PPOSIXTimeRange' is
     an infinity) then we return nothing.

     Note that we ignore the inclusiveness of the upper bound. Due to the fact
      that there's no place in the Cardano domain transaction type to store the
      inclusiveness information, we can never get a time range with closed upper
      bound. See also the ledger implementation: https://bit.ly/3BDzW5R

     @since 0.1.0
-}
pcurrentProposalTime :: forall (s :: S). Term s (PPOSIXTimeRange :--> PMaybe PProposalTime)
pcurrentProposalTime = phoistAcyclic $
  plam $ \iv -> unTermCont $ do
    PInterval iv' <- pmatchC iv
    ivf <- pletAllC iv'
    PLowerBound lb <- pmatchC ivf.from
    PUpperBound ub <- pmatchC ivf.to

    let lowerBound = pletAll lb $ \f ->
          pif
            f._1
            ( pmatch f._0 $ \case
                PFinite (pfromData . (pfield @"_0" #) -> d) -> pjust # d
                _ -> ptrace "currentProposalTime: time range should be bounded" pnothing
            )
            (ptrace "currentProposalTime: lower bound of the time range should be inclusive" pnothing)

        upperBound = pletAll ub $ \f ->
          pmatch f._0 $ \case
            PFinite (pfromData . (pfield @"_0" #) -> d) -> pjust # d
            _ -> ptrace "currentProposalTime: time range should be bounded" pnothing

        mkTime = phoistAcyclic $
          plam $ \lb ub ->
            passert
              "Upper bound bigger than lower bound"
              (lb #< ub)
              (pcon $ PFullyBoundedTimeRange lb ub)

    pure $ pliftA2 # mkTime # lowerBound # upperBound

{- | Represent relation between current time and a given period.

     Note that the "before" relation isn't present due to the fact that
     it's considered as an error in the proposal script.

     @since 1.0.0
-}
data PTimingRelation (s :: S)
  = PWithin
  | PAfter
  deriving stock
    ( -- | @since 1.0.0
      Generic
    , -- | @since 1.0.0
      Enum
    , -- | @since 1.0.0
      Bounded
    )
  deriving anyclass
    ( -- | @since 1.0.0
      PlutusType
    )

-- | @since 1.0.0
instance DerivePlutusType PTimingRelation where
  type DPTStrat _ = PlutusTypeEnumData

{- | Return true if a relation is 'PWithin'.

     @since 1.0.0
-}
pisWithin :: forall (s :: S). Term s (PTimingRelation :--> PBool)
pisWithin = phoistAcyclic $
  plam $
    flip pmatch $ \case
      PWithin -> pconstant True
      _ -> pconstant False

{- | Represent a proposal period.

     @since 1.0.0
-}
data PPeriod (s :: S)
  = PDraftingPeriod
  | PVotingPeriod
  | PLockingPeriod
  | PExecutingPeriod
  deriving stock
    ( -- | @since 1.0.0
      Generic
    , -- | @since 1.0.0
      Enum
    , -- | @since 1.0.0
      Bounded
    )
  deriving anyclass
    ( -- | @since 1.0.0
      PlutusType
    )

-- | @since 1.0.0
instance DerivePlutusType PPeriod where
  type DPTStrat _ = PlutusTypeEnumData

{- | Compute the relation between current time range and the given peroid,
     providing the starting time and timing configuration of a proposal. If the
     relation cannot be determined, error out.

     @since 1.0.0
-}
pgetRelation ::
  forall (s :: S).
  Term
    s
    ( PProposalTimingConfig
        :--> PProposalStartingTime
        :--> PProposalTime
        :--> PPeriod
        :--> PTimingRelation
    )
pgetRelation = phoistAcyclic $
  plam $ \config startingTime currentTime period -> unTermCont $ do
    configF <- pletAllC config

    PProposalStartingTime s <- pmatchC startingTime
    PFullyBoundedTimeRange lb ub <- pmatchC currentTime

    dub <- pletC $ s + configF.draftTime
    vub <- pletC $ dub + configF.votingTime
    lub <- pletC $ vub + configF.lockingTime
    eub <- pletC $ lub + configF.executingTime

    (plb, pub) <-
      pmatchC period
        <&> ( \case
                PDraftingPeriod -> (s, dub)
                PVotingPeriod -> (dub, vub)
                PLockingPeriod -> (vub, lub)
                PExecutingPeriod -> (lub, eub)
            )

    pure $
      pif (plb #<= lb #&& ub #<= pub) (pcon PWithin) $
        pif (pub #< lb) (pcon PAfter) $
          ptraceError "pgetRelation: too early or invalid current time"

{- | Return true if the width of given 'PProposalTime' is shorter than the
     maximum.

     @since 1.0.0
-}
psatisfyMaximumWidth ::
  forall (s :: S).
  Term
    s
    ( PMaxTimeRangeWidth
        :--> PProposalTime
        :--> PBool
    )
psatisfyMaximumWidth = phoistAcyclic $
  plam $ \maxWidth time ->
    let width = ptimeRangeDuration # time
        max = pto maxWidth
     in width #<= max
