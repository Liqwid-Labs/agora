import Prelude

--------------------------------------------------------------------------------

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (defaultMain, testGroup)

--------------------------------------------------------------------------------

import Property.Governor qualified as Governer
import Spec.AuthorityToken qualified as AuthorityToken
import Spec.Effect.GovernorMutation qualified as GovernorMutation
import Spec.Effect.TreasuryWithdrawal qualified as TreasuryWithdrawal
import Spec.Governor qualified as Governor
import Spec.Proposal qualified as Proposal
import Spec.Stake qualified as Stake
import Spec.Treasury qualified as Treasury
import Spec.Utils qualified as Utils

import Test.Specification (group, toTestTree)

-- | The Agora test suite.
main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "test suite"
      [ testGroup
          "Effects"
          [ toTestTree $ group "Treasury Withdrawal Effect" TreasuryWithdrawal.specs
          , toTestTree $ group "Governor Mutation Effect" GovernorMutation.specs
          ]
      , toTestTree $ group "Stake tests" Stake.specs
      , toTestTree $ group "Proposal tests" Proposal.specs
      , toTestTree $ group "AuthorityToken tests" AuthorityToken.specs
      , toTestTree $ group "Treasury tests" Treasury.specs
      , toTestTree $ group "AuthorityToken tests" AuthorityToken.specs
      , toTestTree $ group "Governor tests" Governor.specs
      , testGroup "Governor properties" Governer.props
      , testGroup
          "Utility tests"
          Utils.tests
      ]
