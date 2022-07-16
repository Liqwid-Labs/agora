{- |
Module     : Spec.Proposal
Maintainer : emi@haskell.fyi
Description: Tests for Proposal policy and validator

Tests for Proposal policy and validator
-}
module Spec.Proposal (specs) where

import Sample.Proposal.Advance qualified as Advance
import Sample.Proposal.Cosign qualified as Cosign
import Sample.Proposal.Create qualified as Create
import Sample.Proposal.UnlockStake qualified as UnlockStake
import Sample.Proposal.Vote qualified as Vote
import Test.Specification (
  SpecificationTree,
  group,
 )

-- | Stake specs.
specs :: [SpecificationTree]
specs =
  [ group
      "policy (proposal creation)"
      [ Create.mkTestTree
          "legal"
          Create.totallyValidParameters
          True
          True
          True
      , group
          "illegal"
          [ Create.mkTestTree
              "invalid next proposal id"
              Create.invalidOutputGovernorDatumParameters
              True
              False
              True
          , Create.mkTestTree
              "use other's stake"
              Create.useStakeOwnBySomeoneElseParameters
              True
              True
              False
          , Create.mkTestTree
              "altered stake"
              Create.invalidOutputStakeParameters
              True
              False
              False
          , Create.mkTestTree
              "invalid stake locks"
              Create.addInvalidLocksParameters
              True
              False
              True
          , Create.mkTestTree
              "has reached maximum proposals limit"
              Create.exceedMaximumProposalsParameters
              True
              False
              True
          , Create.mkTestTree
              "loose time range"
              Create.timeRangeNotTightParameters
              True
              False
              True
          , Create.mkTestTree
              "open time range"
              Create.timeRangeNotClosedParameters
              True
              False
              True
          , group "invalid proposal status" $
              map
                ( \ps ->
                    Create.mkTestTree
                      (show ps.proposalStatus)
                      ps
                      True
                      False
                      True
                )
                Create.invalidProposalStatusParameters
          ]
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
                            Nothing
                        , Advance.mkTestTree
                            "to failed state"
                            ( head $
                                Advance.advanceToFailedStateDueToTimeoutParameters
                                  nCosigners
                            )
                            True
                            Nothing
                        ]

                    illegalGroup =
                      group
                        "illegal"
                        [ Advance.mkTestTree
                            "insufficient cosigns"
                            (Advance.insufficientCosignsParameters nCosigners)
                            False
                            Nothing
                        , Advance.mkTestTree
                            "invalid stake output"
                            (head $ Advance.invalidOutputStakeParameters nCosigners)
                            False
                            Nothing
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
                             in Advance.mkTestTree name ps True (Just True)
                        )
                        (tail $ Advance.advanceToNextStateInTimeParameters 1)
                  , group "advance to failed state" $
                      map
                        ( \ps ->
                            let name = "from: " <> show ps.fromStatus
                             in Advance.mkTestTree name ps True (Just True)
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
                      Nothing
                  , Advance.mkTestTree
                      "initial state is Finished"
                      Advance.advanceFromFinishedParameters
                      False
                      Nothing
                  , group
                      "invalid stake output"
                      $ do
                        nStake <- [1, 5]
                        ps <- tail $ Advance.invalidOutputStakeParameters nStake

                        let name =
                              "from " <> show ps.fromStatus <> "with "
                                <> show nStake
                                <> " stakes"

                        pure $ Advance.mkTestTree name ps False (Just True)
                  ]
           in [draftGroup, legalGroup, illegalGroup]
      , group "unlocking" $
          let proposalCountCases = [1, 5, 10, 42]

              mkSubgroupName nProposals = "with " <> show nProposals <> " proposals"

              mkLegalGroup nProposals =
                group
                  (mkSubgroupName nProposals)
                  [ UnlockStake.mkTestTree
                      "voter: retract votes while voting"
                      (UnlockStake.mkVoterRetractVotesWhileVotingParameters nProposals)
                      True
                  , UnlockStake.mkTestTree
                      "voter/creator: retract votes while voting"
                      (UnlockStake.mkVoterCreatorRetractVotesWhileVotingParameters nProposals)
                      True
                  , UnlockStake.mkTestTree
                      "creator: remove creator locks when finished"
                      (UnlockStake.mkCreatorRemoveCreatorLocksWhenFinishedParameters nProposals)
                      True
                  , UnlockStake.mkTestTree
                      "voter/creator: remove all locks when finished"
                      (UnlockStake.mkVoterCreatorRemoveAllLocksWhenFinishedParameters nProposals)
                      True
                  , group "voter: unlock after voting" $
                      map
                        ( \ps ->
                            let name = show ps.proposalStatus
                             in UnlockStake.mkTestTree name ps True
                        )
                        (UnlockStake.mkVoterUnlockStakeAfterVotingParameters nProposals)
                  , UnlockStake.mkTestTree
                      "voter/creator: remove vote locks when locked"
                      (UnlockStake.mkVoterCreatorRemoveVoteLocksWhenLockedParameters nProposals)
                      True
                  ]

              mkIllegalGroup nProposals =
                group
                  (mkSubgroupName nProposals)
                  [ group "retract votes while not voting" $
                      map
                        ( \ps ->
                            let name =
                                  "role: " <> show ps.stakeRole
                                    <> ", status: "
                                    <> show ps.proposalStatus
                             in UnlockStake.mkTestTree name ps False
                        )
                        (UnlockStake.mkRetractVotesWhileNotVoting nProposals)
                  , group "unlock an irrelevant stake" $
                      map
                        ( \ps ->
                            let name =
                                  "status: " <> show ps.proposalStatus
                                    <> "retract votes: "
                                    <> show ps.retractVotes
                             in UnlockStake.mkTestTree name ps False
                        )
                        (UnlockStake.mkUnockIrrelevantStakeParameters nProposals)
                  , group "remove creator too early" $
                      map
                        ( \ps ->
                            let name =
                                  "status: " <> show ps.proposalStatus
                             in UnlockStake.mkTestTree name ps False
                        )
                        (UnlockStake.mkRemoveCreatorLockBeforeFinishedParameters nProposals)
                  , UnlockStake.mkTestTree
                      "creator: retract votes"
                      (UnlockStake.mkRetractVotesWithCreatorStakeParamaters nProposals)
                      False
                  , group "alter output stake datum" $
                      map
                        ( \ps ->
                            let name =
                                  "role: " <> show ps.stakeRole
                                    <> ", status: "
                                    <> show ps.proposalStatus
                             in UnlockStake.mkTestTree name ps False
                        )
                        (UnlockStake.mkAlterStakeParameters nProposals)
                  ]

              legalGroup = group "legal" $ map mkLegalGroup proposalCountCases
              illegalGroup = group "illegal" $ map mkIllegalGroup proposalCountCases
           in [legalGroup, illegalGroup]
      ]
  ]
