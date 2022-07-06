{- |
Module     : Spec.Proposal
Maintainer : emi@haskell.fyi
Description: Tests for Proposal policy and validator

Tests for Proposal policy and validator
-}
module Spec.Proposal (specs) where

import Agora.Proposal (
  Proposal (..),
  ProposalStatus (..),
 )
import Agora.Proposal.Scripts (proposalPolicy)
import Sample.Proposal qualified as Proposal
import Sample.Proposal.Advance qualified as Advance
import Sample.Proposal.Cosign qualified as Cosign
import Sample.Proposal.UnlockStake qualified as UnlockStake
import Sample.Proposal.Vote qualified as Vote
import Sample.Shared qualified as Shared (proposal)
import Test.Specification (
  SpecificationTree,
  group,
  policySucceedsWith,
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
          $ let cosignerCases = [1, 5, 10]

                mkLegalGroup nCosigners =
                  Cosign.mkTestTree
                    ("with " <> show nCosigners <> " cosigners")
                    (Cosign.validCosignNParameters nCosigners)
                    True
                legalGroup =
                  group "legal" $
                    map mkLegalGroup cosignerCases

                mkIllegalStatusNotDraftGroup nCosigners =
                  group ("with " <> show nCosigners <> " cosigners") $
                    map
                      ( \ps ->
                          Cosign.mkTestTree
                            ("status: " <> show ps.proposalStatus)
                            ps
                            False
                      )
                      (Cosign.statusNotDraftCosignNParameters nCosigners)
                illegalStatusNotDraftGroup =
                  group "proposal status not Draft" $
                    map mkIllegalStatusNotDraftGroup cosignerCases

                illegalGroup =
                  group
                    "illegal"
                    [ Cosign.mkTestTree
                        "duplicate cosigners"
                        Cosign.duplicateCosignersParameters
                        False
                    , Cosign.mkTestTree
                        "altered output stake"
                        Cosign.invalidStakeOutputParameters
                        False
                    , illegalStatusNotDraftGroup
                    ]
             in [legalGroup, illegalGroup]
      , group
          "voting"
          [ Vote.mkTestTree "legal" Vote.validVoteParameters True
          -- TODO: add negative test cases
          ]
      , group "advancing" $
          let mkFromDraft nCosigners =
                let name = "with " <> show nCosigners <> " cosigner(s)"

                    legalGroup =
                      group
                        "legal"
                        [ Advance.mkTestTree
                            "to next state"
                            ( head $
                                Advance.advanceToNextStateInTimeParameters
                                  nCosigners
                            )
                            True
                        , Advance.mkTestTree
                            "to failed state"
                            ( head $
                                Advance.advanceToFailedStateDueToTimeoutParameters
                                  nCosigners
                            )
                            True
                        ]

                    illegalGroup =
                      group
                        "illegal"
                        [ Advance.mkTestTree
                            "insufficient cosigns"
                            (Advance.insufficientCosignsParameters nCosigners)
                            False
                        , Advance.mkTestTree
                            "invalid stake output"
                            (head $ Advance.invalidOutputStakeParameters nCosigners)
                            False
                        ]
                 in group name [legalGroup, illegalGroup]

              draftGroup = group "from draft" $ map mkFromDraft [1, 5, 10]

              legalGroup =
                group
                  "legal"
                  [ group "advance to next state" $
                      map
                        ( \ps ->
                            let name = "from: " <> show ps.fromStatus
                             in Advance.mkTestTree name ps True
                        )
                        (tail $ Advance.advanceToNextStateInTimeParameters 1)
                  , group "advance to failed state" $
                      map
                        ( \ps ->
                            let name = "from: " <> show ps.fromStatus
                             in Advance.mkTestTree name ps True
                        )
                        (tail $ Advance.advanceToFailedStateDueToTimeoutParameters 1)
                  ]

              illegalGroup =
                group
                  "illegal"
                  [ Advance.mkTestTree
                      "insufficient votes"
                      Advance.insufficientVotesParameters
                      False
                  , Advance.mkTestTree
                      "initial state is Finished"
                      Advance.advanceFromFinishedParameters
                      False
                  , group
                      "invalid stake output"
                      $ do
                        nStake <- [1, 5]
                        ps <- tail $ Advance.invalidOutputStakeParameters nStake

                        let name =
                              "from " <> show ps.fromStatus <> "with "
                                <> show nStake
                                <> " stakes"

                        pure $ Advance.mkTestTree name ps False
                  ]
           in [draftGroup, legalGroup, illegalGroup]
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
