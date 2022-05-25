import Prelude

--------------------------------------------------------------------------------

import Test.Tasty (defaultMain, testGroup)

--------------------------------------------------------------------------------

import Spec.AuthorityToken qualified as AuthorityToken
import Spec.Effect.GovernorMutation qualified as GovernorMutation
import Spec.Effect.TreasuryWithdrawal qualified as TreasuryWithdrawal
import Spec.Governor qualified as Governor
import Spec.Model.MultiSig qualified as MultiSig
import Spec.Proposal qualified as Proposal
import Spec.Stake qualified as Stake
import Spec.Treasury qualified as Treasury
import Spec.Utils qualified as Utils

-- | The Agora test suite.
main :: IO ()
main =
  defaultMain $
    testGroup
      "test suite"
      [ testGroup
          "Effects"
          [ testGroup
              "Treasury Withdrawal Effect"
              TreasuryWithdrawal.tests
          , testGroup
              "Governor Mutation Effect"
              GovernorMutation.tests
          ]
      , testGroup
          "Stake tests"
          Stake.tests
      , testGroup
          "Proposal tests"
          Proposal.tests
      , testGroup
          "AuthorityToken tests"
          AuthorityToken.tests
      , testGroup
          "Treasury tests"
          Treasury.tests
      , testGroup
          "AuthorityToken tests"
          AuthorityToken.tests
      , testGroup
          "Governor tests"
          Governor.tests
      , testGroup
          "Utility tests"
          Utils.tests
      , testGroup
          "Multisig tests"
          [ testGroup
              "MultiSig"
              [ MultiSig.plutarchTests
              , MultiSig.genTests
              ]
          ]
      ]
