{-# LANGUAGE QuasiQuotes #-}

{- |
Module     : Spec.Proposal
Maintainer : emi@haskell.fyi
Description: Tests for Proposal policy and validator

Tests for Proposal policy and validator
-}
module Spec.Proposal (specs) where

import Agora.Proposal (
  Proposal (..),
  ProposalDatum (..),
  ProposalId (ProposalId),
  ProposalRedeemer (..),
  ProposalStatus (..),
  ProposalThresholds (..),
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
import Agora.Proposal.Scripts (proposalPolicy, proposalValidator)
import Agora.Proposal.Time (
  ProposalStartingTime (ProposalStartingTime),
 )
import Agora.Stake (
  ProposalLock (ProposalLock),
  StakeDatum (StakeDatum),
  StakeRedeemer (PermitVote, WitnessStake),
 )
import Agora.Stake.Scripts (stakeValidator)
import Data.Default.Class (Default (def))
import Data.Tagged (Tagged (Tagged), untag)
import PlutusLedgerApi.V1 (ScriptContext (..), ScriptPurpose (..))
import PlutusTx.AssocMap qualified as AssocMap
import Sample.Proposal qualified as Proposal
import Sample.Proposal.UnlockStake qualified as UnlockStake
import Sample.Shared (signer, signer2)
import Sample.Shared qualified as Shared (proposal, stake)
import Test.Specification (
  SpecificationTree,
  group,
  policySucceedsWith,
  validatorFailsWith,
  validatorSucceedsWith,
 )

-- | Stake specs.
specs :: [SpecificationTree]
specs =
  [ group
      "policy"
      [ policySucceedsWith
          "proposalCreation"
          (proposalPolicy Shared.proposal.governorSTAssetClass)
          ()
          Proposal.proposalCreation
      ]
  , group
      "validator"
      [ group
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
                  , thresholds = def
                  , votes =
                      emptyVotesFor $
                        AssocMap.fromList
                          [ (ResultTag 0, AssocMap.empty)
                          , (ResultTag 1, AssocMap.empty)
                          ]
                  , timingConfig = def
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
      , group
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
                  , thresholds = def
                  , votes =
                      ProposalVotes
                        ( AssocMap.fromList
                            [ (ResultTag 0, 42)
                            , (ResultTag 1, 4242)
                            ]
                        )
                  , timingConfig = def
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
      , group
          "advancing"
          [ group "successfully advance to next state" $
              map
                ( \(name, initialState) ->
                    validatorSucceedsWith
                      name
                      (proposalValidator Shared.proposal)
                      ( ProposalDatum
                          { proposalId = ProposalId 0
                          , effects =
                              AssocMap.fromList
                                [ (ResultTag 0, AssocMap.empty)
                                , (ResultTag 1, AssocMap.empty)
                                ]
                          , status = initialState
                          , cosigners = [signer]
                          , thresholds = def
                          , votes =
                              ProposalVotes
                                ( AssocMap.fromList
                                    [
                                      ( ResultTag 0
                                      , case initialState of
                                          Draft -> 0
                                          _ -> untag (def :: ProposalThresholds).execute + 1
                                      )
                                    , (ResultTag 1, 0)
                                    ]
                                )
                          , timingConfig = def
                          , startingTime = ProposalStartingTime 0
                          }
                      )
                      AdvanceProposal
                      ( ScriptContext
                          ( Proposal.advanceProposalSuccess
                              Proposal.TransitionParameters
                                { Proposal.initialProposalStatus = initialState
                                , Proposal.proposalStartingTime = ProposalStartingTime 0
                                }
                          )
                          (Spending Proposal.proposalRef)
                      )
                )
                [ ("Draft -> VotringReady", Draft)
                , ("VotingReady -> Locked", VotingReady)
                , ("Locked -> Finished", Locked)
                ]
          , group "successfully advance to failed state: timeout" $
              map
                ( \(name, initialState) ->
                    validatorSucceedsWith
                      name
                      (proposalValidator Shared.proposal)
                      ( ProposalDatum
                          { proposalId = ProposalId 0
                          , effects =
                              AssocMap.fromList
                                [ (ResultTag 0, AssocMap.empty)
                                , (ResultTag 1, AssocMap.empty)
                                ]
                          , status = initialState
                          , cosigners = [signer]
                          , thresholds = def
                          , votes =
                              ProposalVotes
                                ( AssocMap.fromList
                                    [
                                      ( ResultTag 0
                                      , case initialState of
                                          Draft -> 0
                                          _ -> untag (def :: ProposalThresholds).vote + 1
                                      )
                                    , (ResultTag 1, 0)
                                    ]
                                )
                          , timingConfig = def
                          , startingTime = ProposalStartingTime 0
                          }
                      )
                      AdvanceProposal
                      ( ScriptContext
                          ( Proposal.advanceProposalFailureTimeout
                              Proposal.TransitionParameters
                                { Proposal.initialProposalStatus = initialState
                                , Proposal.proposalStartingTime = ProposalStartingTime 0
                                }
                          )
                          (Spending Proposal.proposalRef)
                      )
                )
                [ ("Draft -> Finished", Draft)
                , ("VotingReady -> Finished", VotingReady)
                , ("Locked -> Finished", Locked)
                ]
          , validatorFailsWith
              "illegal: insufficient votes"
              (proposalValidator Shared.proposal)
              ( ProposalDatum
                  { proposalId = ProposalId 0
                  , effects =
                      AssocMap.fromList
                        [ (ResultTag 0, AssocMap.empty)
                        , (ResultTag 1, AssocMap.empty)
                        ]
                  , status = VotingReady
                  , cosigners = [signer]
                  , thresholds = def
                  , votes =
                      ProposalVotes
                        ( AssocMap.fromList
                            [ (ResultTag 0, 1)
                            , (ResultTag 1, 0)
                            ]
                        )
                  , timingConfig = def
                  , startingTime = ProposalStartingTime 0
                  }
              )
              AdvanceProposal
              ( ScriptContext
                  Proposal.advanceProposalInsufficientVotes
                  (Spending Proposal.proposalRef)
              )
          , validatorFailsWith
              "illegal: initial state is Finished"
              (proposalValidator Shared.proposal)
              ( ProposalDatum
                  { proposalId = ProposalId 0
                  , effects =
                      AssocMap.fromList
                        [ (ResultTag 0, AssocMap.empty)
                        , (ResultTag 1, AssocMap.empty)
                        ]
                  , status = Finished
                  , cosigners = [signer]
                  , thresholds = def
                  , votes =
                      ProposalVotes
                        ( AssocMap.fromList
                            [ (ResultTag 0, untag (def :: ProposalThresholds).vote + 1)
                            , (ResultTag 1, 0)
                            ]
                        )
                  , timingConfig = def
                  , startingTime = ProposalStartingTime 0
                  }
              )
              AdvanceProposal
              ( ScriptContext
                  Proposal.advanceFinishedProposal
                  (Spending Proposal.proposalRef)
              )
          , validatorFailsWith
              "illegal: with stake input"
              (proposalValidator Shared.proposal)
              ( ProposalDatum
                  { proposalId = ProposalId 0
                  , effects =
                      AssocMap.fromList
                        [ (ResultTag 0, AssocMap.empty)
                        , (ResultTag 1, AssocMap.empty)
                        ]
                  , status = VotingReady
                  , cosigners = [signer]
                  , thresholds = def
                  , votes =
                      ProposalVotes
                        ( AssocMap.fromList
                            [ (ResultTag 0, 0)
                            , (ResultTag 1, 0)
                            ]
                        )
                  , timingConfig = def
                  , startingTime = ProposalStartingTime 0
                  }
              )
              AdvanceProposal
              ( ScriptContext
                  Proposal.advanceProposalWithInvalidOutputStake
                  (Spending Proposal.proposalRef)
              )
          ]
      , group "unlocking" $ do
          proposalCount <- [1, 42]

          let legalGroup = group "legal" $ do
                let voterRetractVotesAndUnlockStakeWhileVoting =
                      UnlockStake.mkProposalValidatorTestCase
                        UnlockStake.UnlockStakeParameters
                          { UnlockStake.proposalCount = proposalCount
                          , UnlockStake.stakeUsage = UnlockStake.Voter
                          , UnlockStake.retractVotes = True
                          , UnlockStake.proposalStatus = VotingReady
                          }
                        True
                    creatorUnlockStakeWhileFinished =
                      UnlockStake.mkProposalValidatorTestCase
                        UnlockStake.UnlockStakeParameters
                          { UnlockStake.proposalCount = proposalCount
                          , UnlockStake.stakeUsage = UnlockStake.Creator
                          , UnlockStake.retractVotes = False
                          , UnlockStake.proposalStatus = Finished
                          }
                        True

                let voterUnlockStakeAfterVoting = group "voter unlocks stake after voting" $ do
                      status <- [Finished, Locked]

                      pure $
                        UnlockStake.mkProposalValidatorTestCase
                          UnlockStake.UnlockStakeParameters
                            { UnlockStake.proposalCount = proposalCount
                            , UnlockStake.stakeUsage = UnlockStake.Voter
                            , UnlockStake.retractVotes = False
                            , UnlockStake.proposalStatus = status
                            }
                          True

                [ voterRetractVotesAndUnlockStakeWhileVoting
                  , creatorUnlockStakeWhileFinished
                  , voterUnlockStakeAfterVoting
                  ]

          let illegalGroup = group "illegal" $ do
                let retractsVotesWhileNotVotingReady =
                      group "voter retracts votes while not voting" $ do
                        status <- [Draft, Locked, Finished]

                        pure $
                          UnlockStake.mkProposalValidatorTestCase
                            UnlockStake.UnlockStakeParameters
                              { UnlockStake.proposalCount = proposalCount
                              , UnlockStake.stakeUsage = UnlockStake.Voter
                              , UnlockStake.retractVotes = True
                              , UnlockStake.proposalStatus = status
                              }
                            False

                    unlockIrrelevantStake =
                      group "unlock an irrelevant stake" $ do
                        status <- [Draft, VotingReady, Locked, Finished]
                        shouldRetractVotes <- [True, False]

                        pure $
                          UnlockStake.mkProposalValidatorTestCase
                            UnlockStake.UnlockStakeParameters
                              { UnlockStake.proposalCount = proposalCount
                              , UnlockStake.stakeUsage = UnlockStake.Irrelevant
                              , UnlockStake.retractVotes = shouldRetractVotes
                              , UnlockStake.proposalStatus = status
                              }
                            False

                    unlockCreatorStakeBeforeFinished =
                      group "unlock creator stake before finished" $ do
                        status <- [Draft, VotingReady, Locked]

                        pure $
                          UnlockStake.mkProposalValidatorTestCase
                            UnlockStake.UnlockStakeParameters
                              { UnlockStake.proposalCount = proposalCount
                              , UnlockStake.stakeUsage = UnlockStake.Creator
                              , UnlockStake.retractVotes = False
                              , UnlockStake.proposalStatus = status
                              }
                            False
                    retractVotesWithCreatorStake =
                      group "creator stake retracts votes" $ do
                        status <- [Draft, VotingReady, Locked, Finished]

                        pure $
                          UnlockStake.mkProposalValidatorTestCase
                            UnlockStake.UnlockStakeParameters
                              { UnlockStake.proposalCount = proposalCount
                              , UnlockStake.stakeUsage = UnlockStake.Creator
                              , UnlockStake.retractVotes = True
                              , UnlockStake.proposalStatus = status
                              }
                            False

                [ retractsVotesWhileNotVotingReady
                  , unlockIrrelevantStake
                  , unlockCreatorStakeBeforeFinished
                  , retractVotesWithCreatorStake
                  ]

          [legalGroup, illegalGroup]
      ]
  ]
