module AgoraTypes (agoraTypes) where

--------------------------------------------------------------------------------

import Language.PureScript.Bridge (
  Language (Haskell),
  SumType,
  mkSumType,
 )

--------------------------------------------------------------------------------

import Agora.AuthorityToken qualified as AuthorityToken
import Agora.Effect.TreasuryWithdrawal qualified as TreasuryWithdrawalEffect
import Agora.Governor qualified as Governor
import Agora.MultiSig qualified as MultiSig
import Agora.Proposal qualified as Proposal
import Agora.Stake qualified as Stake
import Agora.Treasury qualified as Treasury

--------------------------------------------------------------------------------

agoraTypes :: [SumType 'Haskell]
agoraTypes =
  [ -- Proposal
    mkSumType @Proposal.ProposalId
  , mkSumType @Proposal.ResultTag
  , mkSumType @Proposal.ProposalStatus
  , mkSumType @Proposal.ProposalThresholds
  , mkSumType @Proposal.ProposalVotes
  , mkSumType @Proposal.ProposalDatum
  , mkSumType @Proposal.ProposalRedeemer
  , mkSumType @Proposal.Proposal
  , -- Governor
    mkSumType @Governor.GovernorDatum
  , mkSumType @Governor.GovernorRedeemer
  , mkSumType @Governor.Governor
  , -- MultiSig
    mkSumType @MultiSig.MultiSig
  , -- Stake
    mkSumType @Stake.Stake
  , mkSumType @Stake.ProposalLock
  , mkSumType @Stake.StakeRedeemer
  , mkSumType @Stake.StakeDatum
  , -- Treasury
    mkSumType @Treasury.TreasuryRedeemer
  , -- AuthorityToken
    mkSumType @AuthorityToken.AuthorityToken
  , -- Effects
    mkSumType @TreasuryWithdrawalEffect.TreasuryWithdrawalDatum
  ]
