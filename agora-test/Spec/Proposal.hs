{-# LANGUAGE QuasiQuotes #-}

{- |
Module     : Spec.Proposal
Maintainer : emi@haskell.fyi
Description: Tests for Proposal policy and validator

Tests for Proposal policy and validator
-}
module Spec.Proposal (tests) where

--------------------------------------------------------------------------------

import Agora.Proposal (
  ProposalDatum (ProposalDatum),
  ProposalId (ProposalId),
  ProposalRedeemer (Cosign),
  ProposalStatus (Draft),
  ProposalVotes (ProposalVotes),
  ResultTag (ResultTag),
  cosigners,
  effects,
  proposalId,
  status,
  thresholds,
  votes,
 )
import Agora.Proposal.Scripts (
  proposalPolicy,
  proposalValidator,
 )
import Agora.Stake (StakeDatum (StakeDatum), StakeRedeemer (WitnessStake))
import Agora.Stake.Scripts (stakeValidator)
import Plutarch.SafeMoney (Tagged (Tagged))
import Plutus.V1.Ledger.Api (ScriptContext (..), ScriptPurpose (..))
import PlutusTx.AssocMap qualified as AssocMap
import Spec.Sample.Proposal qualified as Proposal
import Spec.Sample.Shared (signer, signer2)
import Spec.Sample.Shared qualified as Shared
import Spec.Util (policySucceedsWith, validatorSucceedsWith)
import Test.Tasty (TestTree, testGroup)

--------------------------------------------------------------------------------

-- | Stake tests.
tests :: [TestTree]
tests =
  [ testGroup
      "policy"
      [ policySucceedsWith
          "proposalCreation"
          (proposalPolicy Shared.proposal)
          ()
          Proposal.proposalCreation
      ]
  , testGroup
      "validator"
      [ testGroup
          "cosignature"
          [ validatorSucceedsWith
              "proposal"
              (proposalValidator Shared.proposal)
              ( ProposalDatum
                  { proposalId = ProposalId 0
                  , effects =
                      AssocMap.fromList
                        [ (ResultTag 0, [])
                        , (ResultTag 1, [])
                        ]
                  , status = Draft
                  , cosigners = [signer]
                  , thresholds = Shared.defaultProposalThresholds
                  , votes = ProposalVotes AssocMap.empty
                  }
              )
              (Cosign [signer2])
              (ScriptContext (Proposal.cosignProposal [signer2]) (Spending Proposal.proposalRef))
          , validatorSucceedsWith
              "stake"
              (stakeValidator Shared.stake)
              (StakeDatum (Tagged 50_000_000) signer2 [])
              WitnessStake
              (ScriptContext (Proposal.cosignProposal [signer2]) (Spending Proposal.stakeRef))
          ]
      ]
  ]
