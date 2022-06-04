{-# LANGUAGE QuasiQuotes #-}

{- |
Module     : Spec.Proposal
Maintainer : emi@haskell.fyi
Description: Tests for Proposal policy and validator

Tests for Proposal policy and validator
-}
module Spec.Proposal (specs) where

--------------------------------------------------------------------------------

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
import Data.Default.Class (Default (def))
import Data.Tagged (Tagged (Tagged), untag)
import PlutusLedgerApi.V1 (ScriptContext (..), ScriptPurpose (..))
import PlutusTx.AssocMap qualified as AssocMap
import Sample.Proposal qualified as Proposal
import Sample.Shared (signer, signer2)
import Sample.Shared qualified as Shared
import Test.Specification (
  SpecificationTree,
  group,
  policySucceedsWith,
  validatorFailsWith,
  validatorSucceedsWith,
 )

--------------------------------------------------------------------------------

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
                                          _ -> untag (def :: ProposalThresholds).countVoting + 1
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
                                          _ -> untag (def :: ProposalThresholds).countVoting + 1
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
                            [ (ResultTag 0, untag (def :: ProposalThresholds).countVoting + 1)
                            , (ResultTag 1, 0)
                            ]
                        )
                  , timingConfig = def
                  , startingTime = ProposalStartingTime 0
                  }
              )
              AdvanceProposal
              ( ScriptContext
                  Proposal.advanceFinishedPropsoal
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
                  Proposal.advancePropsoalWithsStake
                  (Spending Proposal.proposalRef)
              )
          ]
      ]
  ]
