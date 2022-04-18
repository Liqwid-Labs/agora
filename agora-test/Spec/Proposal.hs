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

import Agora.Proposal (proposalPolicy)
import Spec.Sample.Proposal qualified as Proposal
import Spec.Util (policySucceedsWith)
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
  ]
