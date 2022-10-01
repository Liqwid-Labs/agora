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
import Sample.Proposal.Unlock qualified as Unlock
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
              True
              False
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
                    , illegalStatusNotDraftGroup
                    ]
             in [legalGroup, illegalGroup]
      , group
          "voting"
          [ group
              "legal"
              [ group "different number of stakes" $
                  map
                    ( \s ->
                        group
                          (unwords [show s, "stakes"])
                          [ Vote.mkTestTree
                              "by owner"
                              (Vote.mkValidOwnerVoteBundle s)
                              (Vote.Validity True True)
                          , Vote.mkTestTree
                              "by delegatee"
                              (Vote.mkValidDelegateeVoteBundle s)
                              (Vote.Validity True True)
                          ]
                    )
                    [1, 3, 5, 7, 9]
              , Vote.mkTestTree
                  "transparent non-GT tokens"
                  Vote.transparentAssets
                  (Vote.Validity True True)
              ]
          , group
              "illegal"
              [ Vote.mkTestTree
                  "vote for nonexistent outcome"
                  Vote.voteForNonexistentOutcome
                  (Vote.Validity False True)
              , Vote.mkTestTree
                  "unauthorized tx"
                  Vote.transactionNotAuthorized
                  (Vote.Validity True False)
              , Vote.mkTestTree
                  "no proposal"
                  Vote.noProposal
                  (Vote.Validity False False)
              , Vote.mkTestTree
                  "more than one proposals"
                  Vote.voteForNonexistentOutcome
                  (Vote.Validity False True)
              , Vote.mkTestTree
                  "locks not added"
                  Vote.invalidLocks
                  (Vote.Validity True False)
              , Vote.mkTestTree
                  "attempt to burn stakes"
                  Vote.destroyStakes
                  (Vote.Validity True False)
              ]
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
                                , forAuthorityTokenPolicy = Just True
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
          let stakeCountCases = [1, 3, 5, 7, 9, 11]

              mkSubgroupName nStakes = unwords ["with", show nStakes, "stakes"]

              mkLegalGroup nStakes =
                group
                  (mkSubgroupName nStakes)
                  [ Unlock.mkTestTree
                      "voter: retract votes while voting"
                      (Unlock.mkValidVoterRetractVotes nStakes)
                      (Unlock.Validity True True)
                  , Unlock.mkTestTree
                      "voter: retract votes while voting by delegatee"
                      (Unlock.mkValidDelegateeRetractVotes nStakes)
                      (Unlock.Validity True True)
                  , Unlock.mkTestTree
                      "voter/creator: retract votes while voting"
                      (Unlock.mkValidVoterCreatorRetractVotes nStakes)
                      (Unlock.Validity True True)
                  , Unlock.mkTestTree
                      "creator: remove creator lock after voting"
                      (Unlock.mkValidCreatorRemoveLock nStakes)
                      (Unlock.Validity True True)
                  , Unlock.mkTestTree
                      "Voter: remove lock after voting"
                      (Unlock.mkValidVoterRemoveLockAfterVoting nStakes)
                      (Unlock.Validity True True)
                  ]

              mkIllegalGroup nStakes =
                group
                  (mkSubgroupName nStakes)
                  [ group "retract votes while not voting" $
                      map
                        ( \c ->
                            Unlock.mkTestTree
                              "(negative test)"
                              c
                              (Unlock.Validity False True)
                        )
                        (Unlock.mkRetractVotesWhileNotVoting nStakes)
                  , group "remove creator too early" $
                      map
                        ( \c ->
                            Unlock.mkTestTree
                              "(negative test)"
                              c
                              (Unlock.Validity True False)
                        )
                        (Unlock.mkRemoveCreatorLockBeforeFinished nStakes)
                  , Unlock.mkTestTree
                      "unlock an irrelevant stake"
                      (Unlock.mkUnockIrrelevantStakes nStakes)
                      (Unlock.Validity False True)
                  , Unlock.mkTestTree
                      "creator: retract votes"
                      (Unlock.mkCreatorRetractVotes nStakes)
                      (Unlock.Validity False True)
                  , Unlock.mkTestTree
                      "change output stake value"
                      (Unlock.mkChangeOutputStakeValue nStakes)
                      (Unlock.Validity True False)
                  ]

              legalGroup = group "legal" $ map mkLegalGroup stakeCountCases
              illegalGroup = group "illegal" $ map mkIllegalGroup stakeCountCases
           in [legalGroup, illegalGroup]
      ]
  ]
