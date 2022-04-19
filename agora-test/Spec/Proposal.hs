{-# LANGUAGE QuasiQuotes #-}

{- |
Module     : Spec.Proposal
Maintainer : emi@haskell.fyi
Description: Tests for Proposal policy and validator

Tests for Proposal policy and validator
-}
module Spec.Proposal (tests) where

--------------------------------------------------------------------------------

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
  proposalPolicy,
  proposalValidator,
  status,
  thresholds,
  votes,
 )
import PlutusTx.AssocMap qualified as AssocMap
import Spec.Sample.Proposal (propThresholds, signer, signer2)
import Spec.Sample.Proposal qualified as Proposal
import Spec.Util (policySucceedsWith, validatorSucceedsWith)
import Test.Tasty (TestTree, testGroup)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- | Stake tests.
tests :: [TestTree]
tests =
  [ testGroup
      "policy"
      [ policySucceedsWith
          "stakeCreation"
          (proposalPolicy Proposal.proposal)
          ()
          Proposal.proposalCreation
      ]
  , testGroup
      "validator"
      [ validatorSucceedsWith
          "stakeCreation"
          (proposalValidator Proposal.proposal)
          ( ProposalDatum
              { proposalId = ProposalId 0
              , effects =
                  AssocMap.fromList
                    [ (ResultTag 0, [])
                    , (ResultTag 1, [])
                    ]
              , status = Draft
              , cosigners = [signer]
              , thresholds = propThresholds
              , votes = ProposalVotes AssocMap.empty
              }
          )
          (Cosign [signer2])
          (Proposal.cosignProposal [signer2])
      ]
  ]
