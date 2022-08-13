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
                    (unwords ["with", show nCosigners, "cosigners"])
                    (Cosign.validCosignNParameters nCosigners)
                    True
                legalGroup =
                  group "legal" $
                    map mkLegalGroup cosignerCases

                mkIllegalStatusNotDraftGroup nCosigners =
                  group (unwords ["with", show nCosigners, "cosigners"]) $
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
          [ group
              "legal"
              [ Vote.mkTestTree "ordinary" Vote.validVoteParameters True
              , Vote.mkTestTree "delegate" Vote.validVoteAsDelegateParameters True
              ]
              -- TODO: add negative test cases
          ]
      , group
          "advancing"
          $ let possibleCosigners = [1, 5, 10]
                possibleEffects = [1, 2, 5]
             in do
                  cs <- possibleCosigners
                  es <- possibleEffects

                  let groupName =
                        unwords
                          [ "with"
                          , show cs
                          , "cosigners"
                          , "and"
                          , show es
                          , "effects"
                          ]

                  pure $
                    group
                      groupName
                      [ group
                          "legal"
                          $ let allValid =
                                  Advance.Validity
                                    { forProposalValidator = True
                                    , forStakeValidator = True
                                    , forGovernorValidator = Just True
                                    , forAuthorityTokenPolicy = Just True
                                    }
                                mkName b =
                                  unwords
                                    [ "from"
                                    , show b.proposalParameters.fromStatus
                                    , "to"
                                    , show b.proposalParameters.toStatus
                                    ]
                             in [ Advance.mkTestTree'
                                    "to next state"
                                    mkName
                                    (Advance.mkValidToNextStateBundles cs es)
                                    allValid
                                , Advance.mkTestTree'
                                    "to failed state"
                                    mkName
                                    (Advance.mkValidToFailedStateBundles cs es)
                                    allValid
                                ]
                      , group
                          "illegal"
                          [ Advance.mkTestTree'
                              "advance finished proposals"
                              (const "(negative test)")
                              (Advance.mkFromFinishedBundles cs es)
                              Advance.Validity
                                { forProposalValidator = False
                                , forStakeValidator = True
                                , forGovernorValidator = Just False
                                , forAuthorityTokenPolicy = Just True
                                }
                          , Advance.mkTestTree
                              "insufficient cosigns"
                              (Advance.mkInsufficientCosignsBundle cs es)
                              Advance.Validity
                                { forProposalValidator = False
                                , forStakeValidator = True
                                , forGovernorValidator = Nothing
                                , forAuthorityTokenPolicy = Nothing
                                }
                          , Advance.mkTestTree
                              "insufficient votes"
                              (Advance.mkInsufficientVotesBundle cs es)
                              Advance.Validity
                                { forProposalValidator = False
                                , forStakeValidator = True
                                , forGovernorValidator = Nothing
                                , forAuthorityTokenPolicy = Nothing
                                }
                          , Advance.mkTestTree
                              "ambiguous winning effect"
                              (Advance.mkAmbiguousWinnerBundle cs es)
                              Advance.Validity
                                { forProposalValidator = False
                                , forStakeValidator = True
                                , forGovernorValidator = Nothing
                                , forAuthorityTokenPolicy = Nothing
                                }
                          , Advance.mkTestTree'
                              "to next state too late"
                              (\b -> unwords ["from", show b.proposalParameters.fromStatus])
                              (Advance.mkToNextStateTooLateBundles cs es)
                              Advance.Validity
                                { forProposalValidator = False
                                , forStakeValidator = True
                                , forGovernorValidator = Just True
                                , forAuthorityTokenPolicy = Just True
                                }
                          , Advance.mkTestTree'
                              "altered output stake datum"
                              (\b -> unwords ["from", show b.proposalParameters.fromStatus])
                              (Advance.mkInvalidOutputStakeBundles cs es)
                              Advance.Validity
                                { forProposalValidator = False
                                , forStakeValidator = False
                                , forGovernorValidator = Just True
                                , forAuthorityTokenPolicy = Just True
                                }
                          , Advance.mkTestTree
                              "forget to mint GATs"
                              (Advance.mkNoGATMintedBundle cs es)
                              Advance.Validity
                                { forProposalValidator = True
                                , forStakeValidator = True
                                , forGovernorValidator = Just False
                                , forAuthorityTokenPolicy = Nothing
                                }
                          , Advance.mkTestTree
                              "mint GATs for wrong validators"
                              (Advance.mkMintGATsForWrongEffectsBundle cs es)
                              Advance.Validity
                                { forProposalValidator = True
                                , forStakeValidator = True
                                , forGovernorValidator = Just False
                                , forAuthorityTokenPolicy = Just True
                                }
                          , Advance.mkTestTree
                              "mint GATs with bad token name"
                              (Advance.mkMintGATsWithoutTagBundle cs es)
                              Advance.Validity
                                { forProposalValidator = True
                                , forStakeValidator = True
                                , forGovernorValidator = Just False
                                , forAuthorityTokenPolicy = Just False
                                }
                          , Advance.mkTestTree
                              "wrong GAT datum"
                              (Advance.mkGATsWithWrongDatumBundle cs es)
                              Advance.Validity
                                { forProposalValidator = True
                                , forStakeValidator = True
                                , forGovernorValidator = Just False
                                , forAuthorityTokenPolicy = Just True
                                }
                          , Advance.mkTestTree
                              "invalid governor output datum"
                              (Advance.mkBadGovernorOutputDatumBundle cs es)
                              Advance.Validity
                                { forProposalValidator = True
                                , forStakeValidator = True
                                , forGovernorValidator = Just False
                                , forAuthorityTokenPolicy = Just True
                                }
                          ]
                      ]
      , group "unlocking" $
          let proposalCountCases = [1, 5, 10, 42]

              mkSubgroupName nProposals = unwords ["with", show nProposals, "proposals"]

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
                                  unwords
                                    [ "role:"
                                    , show ps.stakeRole
                                    , ","
                                    , "status:"
                                    , show ps.proposalStatus
                                    ]
                             in UnlockStake.mkTestTree name ps False
                        )
                        (UnlockStake.mkRetractVotesWhileNotVoting nProposals)
                  , group "unlock an irrelevant stake" $
                      map
                        ( \ps ->
                            let name =
                                  unwords
                                    [ "status:"
                                    , show ps.proposalStatus
                                    , "retract votes:"
                                    , show ps.retractVotes
                                    ]
                             in UnlockStake.mkTestTree name ps False
                        )
                        (UnlockStake.mkUnockIrrelevantStakeParameters nProposals)
                  , group "remove creator too early" $
                      map
                        ( \ps ->
                            let name =
                                  unwords
                                    ["status:", show ps.proposalStatus]
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
                                  unwords
                                    [ "role:"
                                    , show ps.stakeRole
                                    , ","
                                    , "status:"
                                    , show ps.proposalStatus
                                    ]
                             in UnlockStake.mkTestTree name ps False
                        )
                        (UnlockStake.mkAlterStakeParameters nProposals)
                  ]

              legalGroup = group "legal" $ map mkLegalGroup proposalCountCases
              illegalGroup = group "illegal" $ map mkIllegalGroup proposalCountCases
           in [legalGroup, illegalGroup]
      ]
  ]
