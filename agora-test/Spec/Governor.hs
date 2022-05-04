{- |
Module     : Spec.Governor
Maintainer : connor@mlabs.city
Description: Tests for Governor policy and validator

Tests for Governor policy and validator
-}
module Spec.Governor (tests) where

import Agora.Governor (GovernorDatum (..), GovernorRedeemer (..))
import Agora.Governor.Scripts (governorPolicy, governorValidator)
import Agora.Proposal (ProposalId (..))
import Spec.Sample.Governor (createProposal, mintGST)
import Spec.Sample.Shared qualified as Shared
import Spec.Util (policySucceedsWith, validatorSucceedsWith)
import Test.Tasty (TestTree, testGroup)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
  [ testGroup
      "policy"
      [ policySucceedsWith
          "GST minting"
          (governorPolicy Shared.governor)
          ()
          mintGST
      ]
  , testGroup
      "validator"
      [ validatorSucceedsWith
          "proposal creation"
          (governorValidator Shared.governor)
          (GovernorDatum Shared.defaultProposalThresholds (ProposalId 0))
          CreateProposal
          createProposal
      ]
  ]
