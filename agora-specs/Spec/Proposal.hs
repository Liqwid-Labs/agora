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
import Sample.Proposal.PrivilegeEscalate qualified as PrivilegeEscalate
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
          , Create.mkTestTree
              "fake SST"
              Create.fakeSSTParameters
              True
              False
              False
          ]
      ]
  , group
      "validator"
      [ group
          "cosignature"
          [ Cosign.mkTestTree
              "legal"
              Cosign.totallyValid
              (Cosign.Validity True True)
          , group
              "illegal"
              [ Cosign.mkTestTree
                  "insufficient staked amount"
                  Cosign.insufficientStakedAmount
                  (Cosign.Validity False True)
              , Cosign.mkTestTree
                  "proposal locks not updated"
                  Cosign.locksNotUpdated
                  (Cosign.Validity True False)
              , Cosign.mkTestTree
                  "duplicate cosigners"
                  Cosign.duplicateCosigners
                  (Cosign.Validity False True)
              , Cosign.mkTestTree
                  "cosigners not updated"
                  Cosign.cosignersNotUpdated
                  (Cosign.Validity False True)
              , group "cosign after draft" $
                  map
                    ( \b ->
                        Cosign.mkTestTree
                          "(negative test)"
                          b
                          (Cosign.Validity False True)
                    )
                    Cosign.cosignAfterDraft
              ]
          ]
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
              , Vote.mkTestTree
                  "insufficient staked amount"
                  Vote.insufficientAmount
                  (Vote.Validity False True)
              , Vote.mkTestTree
                  "insufficient staked amount"
                  Vote.insufficientAmount1
                  (Vote.Validity False True)
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
                          , Advance.mkTestTree'
                              "fastforward to finished"
                              (\b -> unwords ["from", show b.proposalParameters.fromStatus])
                              (Advance.mkFastforwardToFinishBundles cs es)
                              Advance.Validity
                                { forProposalValidator = False
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
                  , Unlock.mkTestTree
                      "use fake stake"
                      (Unlock.mkUseFakeStakes nStakes)
                      (Unlock.Validity False False)
                  ]

              legalGroup = group "legal" $ map mkLegalGroup stakeCountCases
              illegalGroup = group "illegal" $ map mkIllegalGroup stakeCountCases
           in [legalGroup, illegalGroup]
      ]
  , group
      "privilege escalate"
      [ PrivilegeEscalate.mkTestTree
          "vote"
          PrivilegeEscalate.Voting
          (PrivilegeEscalate.Validity False False)
      , PrivilegeEscalate.mkTestTree
          "retract votes"
          PrivilegeEscalate.RetractingVotes
          (PrivilegeEscalate.Validity False False)
      ]
  ]
