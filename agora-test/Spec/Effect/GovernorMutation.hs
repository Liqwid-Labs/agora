module Spec.Effect.GovernorMutation (tests) where

import Agora.Effect.GovernorMutation (MutateGovernorDatum (..), mutateGovernorValidator)
import Agora.Governor (GovernorDatum (..))
import Agora.Proposal (ProposalId (..))
import Plutus.V1.Ledger.Api (TxOutRef (..))
import Sample.Effect.GovernorMutation (validContext)
import Sample.Shared (defaultProposalThresholds, governor)
import Test.Tasty (TestTree, testGroup)
import Test.Util (effectSucceedsWith)

tests :: [TestTree]
tests =
  [ testGroup
      "validator"
      [ effectSucceedsWith
          "Simple"
          (mutateGovernorValidator governor)
          ( MutateGovernorDatum
              { governorRef = TxOutRef "614481d2159bfb72350222d61fce17e548e0fc00e5a1f841ff1837c431346ce7" 1
              , newDatum =
                  GovernorDatum
                    { nextProposalId = ProposalId 42
                    , proposalThresholds = defaultProposalThresholds
                    }
              }
          )
          validContext
      ]
  ]
