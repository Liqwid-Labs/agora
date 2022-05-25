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
  Proposal (..),
  ProposalDatum (..),
  ProposalId (ProposalId),
  ProposalRedeemer (Cosign, Vote),
  ProposalStatus (Draft, VotingReady),
  ProposalVotes (ProposalVotes),
  ResultTag (ResultTag),
  cosigners,
  effects,
  emptyVotesFor,
  proposalId,
  status,
  thresholds,
  votes,
 )
import Agora.Proposal.Scripts (
  proposalPolicy,
  proposalValidator,
 )
import Agora.Proposal.Time (ProposalStartingTime (ProposalStartingTime))
import Agora.Stake (
  ProposalLock (ProposalLock),
  StakeDatum (StakeDatum),
  StakeRedeemer (PermitVote, WitnessStake),
 )
import Agora.Stake.Scripts (stakeValidator)
import Plutarch.SafeMoney (Tagged (Tagged))
import Plutus.V1.Ledger.Api (ScriptContext (..), ScriptPurpose (..))
import PlutusTx.AssocMap qualified as AssocMap
import Sample.Proposal qualified as Proposal
import Sample.Shared (signer, signer2)
import Sample.Shared qualified as Shared
import Test.Tasty (TestTree, testGroup)
import Test.Util (policySucceedsWith, validatorSucceedsWith)

--------------------------------------------------------------------------------

-- | Stake tests.
tests :: [TestTree]
tests =
  [ testGroup
      "policy"
      [ policySucceedsWith
          "proposalCreation"
          (proposalPolicy Shared.proposal.governorSTAssetClass)
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
                        [ (ResultTag 0, AssocMap.empty)
                        , (ResultTag 1, AssocMap.empty)
                        ]
                  , status = Draft
                  , cosigners = [signer]
                  , thresholds = Shared.defaultProposalThresholds
                  , votes =
                      emptyVotesFor $
                        AssocMap.fromList
                          [ (ResultTag 0, AssocMap.empty)
                          , (ResultTag 1, AssocMap.empty)
                          ]
                  , timingConfig = Shared.defaultProposalTimingConfig
                  , startingTime = ProposalStartingTime 0
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
      , testGroup
          "voting"
          [ validatorSucceedsWith
              "proposal"
              (proposalValidator Shared.proposal)
              ( ProposalDatum
                  { proposalId = ProposalId 42
                  , effects =
                      AssocMap.fromList
                        [ (ResultTag 0, AssocMap.empty)
                        , (ResultTag 1, AssocMap.empty)
                        ]
                  , status = VotingReady
                  , cosigners = [signer]
                  , thresholds = Shared.defaultProposalThresholds
                  , votes =
                      ProposalVotes
                        ( AssocMap.fromList
                            [ (ResultTag 0, 42)
                            , (ResultTag 1, 4242)
                            ]
                        )
                  , timingConfig = Shared.defaultProposalTimingConfig
                  , startingTime = ProposalStartingTime 0
                  }
              )
              (Vote (ResultTag 0))
              ( ScriptContext
                  ( Proposal.voteOnProposal
                      Proposal.VotingParameters
                        { Proposal.voteFor = ResultTag 0
                        , Proposal.voteCount = 27
                        }
                  )
                  (Spending Proposal.proposalRef)
              )
          , validatorSucceedsWith
              "stake"
              (stakeValidator Shared.stake)
              ( StakeDatum
                  (Tagged 27)
                  signer
                  [ ProposalLock (ResultTag 0) (ProposalId 0)
                  , ProposalLock (ResultTag 2) (ProposalId 1)
                  ]
              )
              (PermitVote $ ProposalLock (ResultTag 0) (ProposalId 42))
              ( ScriptContext
                  ( Proposal.voteOnProposal
                      Proposal.VotingParameters
                        { Proposal.voteFor = ResultTag 0
                        , Proposal.voteCount = 27
                        }
                  )
                  (Spending Proposal.stakeRef)
              )
          ]
      ]
  ]
