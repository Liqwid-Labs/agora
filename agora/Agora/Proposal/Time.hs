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

  -- * Compute ranges given config and starting time.
  proposalDraftRange,

  -- * Upstreamables
  plowerBound,
  pupperBound,
  pstrictLowerBound,
  pstrictUpperBound,
) where

import Agora.Record (build, (.&), (.=))
import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))
import Plutarch.Api.V1 (PExtended (PFinite), PInterval (PInterval), PLowerBound (PLowerBound), PPOSIXTime, PPOSIXTimeRange, PUpperBound (PUpperBound))
import Plutarch.DataRepr (PDataFields, PIsDataReprInstances (..))
import Plutarch.Numeric (AdditiveSemigroup ((+)))
import Plutarch.Unsafe (punsafeCoerce)
import Plutus.V1.Ledger.Time (POSIXTime, POSIXTimeRange)
import PlutusTx qualified
import Prelude hiding ((+))

--------------------------------------------------------------------------------

-- | Represents the current time, as far as the proposal is concerned.
newtype ProposalTime = ProposalTime
  { getProposalTime :: POSIXTimeRange
  }
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
  deriving stock (Eq, Show, GHC.Generic)

-- | Represents the starting time of the proposal.
newtype ProposalStartingTime = ProposalStartingTime
  { getProposalStartingTime :: POSIXTime
  }
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
  deriving stock (Eq, Show, GHC.Generic)

-- | Configuration of proposal timings.
data ProposalTimingConfig = ProposalTimingConfig
  { draftTime :: POSIXTime
  -- ^ `D`: the length of the draft period.
  , votingTime :: POSIXTime
  -- ^ `V`: the length of the voting period.
  , lockingTime :: POSIXTime
  -- ^ `L`: the length of the locking period.
  , executingTime :: POSIXTime
  -- ^ `E`: the length of the execution period.
  }
  deriving stock (Eq, Show, GHC.Generic)

PlutusTx.makeIsDataIndexed ''ProposalTimingConfig [('ProposalTimingConfig, 0)]

--------------------------------------------------------------------------------

-- | Plutarch-level version of 'ProposalTime'.
newtype PProposalTime (s :: S) = PProposalTime (Term s PPOSIXTime)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PProposalTime PPOSIXTime)

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

-- -- Need to move these away from here
pstrictLowerBound :: PIsData a => Term s (a :--> PLowerBound a)
pstrictLowerBound = phoistAcyclic $
  plam $ \a ->
    pcon
      ( PLowerBound $
          build $
            #_0 .= pdata (pcon (PFinite $ build $ #_0 .= pdata a))
              .& #_1 .= pdata (pcon PFalse)
      )

pstrictUpperBound :: PIsData a => Term s (a :--> PUpperBound a)
pstrictUpperBound = phoistAcyclic $
  plam $ \a ->
    pcon
      ( PUpperBound $
          build $
            #_0 .= pdata (pcon (PFinite $ build $ #_0 .= pdata a))
              .& #_1 .= pdata (pcon PFalse)
      )

plowerBound :: PIsData a => Term s (a :--> PLowerBound a)
plowerBound = phoistAcyclic $
  plam $ \a ->
    pcon
      ( PLowerBound $
          build $
            #_0 .= pdata (pcon (PFinite $ build $ #_0 .= pdata a))
              .& #_1 .= pdata (pcon PTrue)
      )

pupperBound :: PIsData a => Term s (a :--> PUpperBound a)
pupperBound = phoistAcyclic $
  plam $ \a ->
    pcon
      ( PUpperBound $
          build $
            #_0 .= pdata (pcon (PFinite $ build $ #_0 .= pdata a))
              .& #_1 .= pdata (pcon PTrue)
      )

-- Move this to plutarch-extra.
instance AdditiveSemigroup (Term s PPOSIXTime) where
  (punsafeCoerce @_ @_ @PInteger -> x) + (punsafeCoerce @_ @_ @PInteger -> y) = punsafeCoerce $ x + y

-- | Compute the range of time during which cosigning is legal.
proposalDraftRange :: Term s (PPOSIXTime :--> PProposalTimingConfig :--> PPOSIXTimeRange)
proposalDraftRange = phoistAcyclic $
  plam $ \s config ->
    pcon
      ( PInterval $
          build $
            #from .= pdata (pstrictLowerBound # s)
              .& #to .= pdata (pstrictUpperBound #$ s + pfield @"draftTime" # config)
      )
