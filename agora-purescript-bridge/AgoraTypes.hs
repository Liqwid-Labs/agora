module AgoraTypes (agoraTypes) where

--------------------------------------------------------------------------------

import Data.Proxy (Proxy (..))
import Language.PureScript.Bridge (
  Language (Haskell),
  SumType,
  mkSumType,
 )

--------------------------------------------------------------------------------

import Agora.AuthorityToken qualified as AuthorityToken
import Agora.Effect.GovernorMutation qualified as GovernorMutation
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
    mkSumType (Proxy @Proposal.ProposalId)
  , mkSumType (Proxy @Proposal.ResultTag)
  , mkSumType (Proxy @Proposal.ProposalStatus)
  , mkSumType (Proxy @Proposal.ProposalThresholds)
  , mkSumType (Proxy @Proposal.ProposalVotes)
  , mkSumType (Proxy @Proposal.ProposalDatum)
  , mkSumType (Proxy @Proposal.ProposalRedeemer)
  , mkSumType (Proxy @Proposal.Proposal)
  , -- Governor
    mkSumType (Proxy @Governor.GovernorDatum)
  , mkSumType (Proxy @Governor.GovernorRedeemer)
  , mkSumType (Proxy @Governor.Governor)
  , -- MultiSig
    mkSumType (Proxy @MultiSig.MultiSig)
  , -- Stake
    mkSumType (Proxy @Stake.Stake)
  , mkSumType (Proxy @Stake.ProposalLock)
  , mkSumType (Proxy @Stake.StakeRedeemer)
  , mkSumType (Proxy @Stake.StakeDatum)
  , -- Treasury
    mkSumType (Proxy @Treasury.TreasuryRedeemer)
  , -- AuthorityToken
    mkSumType (Proxy @AuthorityToken.AuthorityToken)
  , -- Effects
    mkSumType (Proxy @TreasuryWithdrawalEffect.TreasuryWithdrawalDatum)
  , mkSumType (Proxy @GovernorMutation.MutateGovernorDatum)
  ]
