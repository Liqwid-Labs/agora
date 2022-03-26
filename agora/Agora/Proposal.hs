{- |
Module     : Agora.Proposal
Maintainer : emi@haskell.fyi
Description: Proposal scripts encoding effects that operate on the system.

Proposal scripts encoding effects that operate on the system.
-}
module Agora.Proposal (
  ProposalDatum (..),
  ProposalStatus (..),
  ResultTag (..),
) where

import Plutus.V1.Ledger.Api (DatumHash, PubKeyHash, ValidatorHash)

--------------------------------------------------------------------------------

{- | Encodes a result. Typically, for a Yes/No proposal, we encode it like this:

   "No"  ~ EffectTag 0
   "Yes" ~ EffectTag 1
-}
newtype ResultTag = ResultTag {getResultTag :: Integer}

{- | The 'status' of the proposal. This is only useful for __actual__
     state transitions, as opposed to time-based 'phases'.

     If the proposal is 'VotingReady', for instance, that doesn't necessarily
     mean that voting is possible, as this also requires the timing to be right.
-}
data ProposalStatus
  = -- | A draft proposal represents a proposal that has yet to be realized.
    --   In effect, this means one which didn't have enough LQ to be a full
    --   proposal, and needs cosigners to enable that to happen. This is
    --   similar to a "temperature check", but only useful if multiple people
    --   want to pool governance tokens together. If the proposal doesn't get to
    --   'VotingReady' on time, the proposal will **never** be able to get
    --   voted on.
    Draft
  | -- | The proposal has/had enough GT cosigned in order to be a fully fledged
    --   proposal. This means that once the timing requirements align,
    --   proposal will be able to be voted on.
    VotingReady
  | -- | The proposal has finished for whatever reason. This can mean it's been
    --   voted on and completed, but it can also mean the proposal failed due to
    --   time constraints or didn't get to 'VotingReady' first.
    --
    --   TODO: The owner of the proposal may choose to reclaim their proposal.
    Finished

data ProposalDatum = ProposalDatum
  { -- TODO: could we encode this more efficiently?
  -- This is shaped this way for future proofing.
  -- See https://github.com/Liqwid-Labs/agora/issues/39
  effects :: [(ResultTag, [(ValidatorHash, DatumHash)])]
  -- ^ Effect lookup table. First by result, then by
  , status :: ProposalStatus
  -- ^ The status the proposal is in.
  , proposers :: [PubKeyHash]
  -- ^ Who created the proposal initially.
  -- We may want to remove this.
  }
