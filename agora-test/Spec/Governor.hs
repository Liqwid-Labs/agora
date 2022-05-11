{- |
Module     : Spec.Governor
Maintainer : connor@mlabs.city
Description: Tests for Agora governor.

Thie module exports `tests`, a list of `TestTree`s, which ensure
that Agora's governor component workds as intended.

Tests should pass when the validator or policy is given one of the
valid script contexts, which are defined in 'Agora.Sample.Governor'.

TODO: add negative test cases.
-}
module Spec.Governor (tests) where

import Agora.Governor (GovernorDatum (..), GovernorRedeemer (..))
import Agora.Governor.Scripts (governorPolicy, governorValidator)
import Agora.Proposal (ProposalId (..))
import Spec.Sample.Governor (createProposal, mintGATs, mintGST, mutateState)
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
      , validatorSucceedsWith
          "GATs minting"
          (governorValidator Shared.governor)
          (GovernorDatum Shared.defaultProposalThresholds (ProposalId 5))
          MintGATs
          mintGATs
      , validatorSucceedsWith
          "mutate governor state"
          (governorValidator Shared.governor)
          (GovernorDatum Shared.defaultProposalThresholds (ProposalId 5))
          MutateGovernor
          mutateState
      ]
  ]
