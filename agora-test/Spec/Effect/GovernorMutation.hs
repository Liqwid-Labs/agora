module Spec.Effect.GovernorMutation (tests) where

import Agora.Effect.GovernorMutation (MutateGovernorDatum (..), mutateGovernorValidator)
import Agora.Governor (GovernorDatum (..))
import Agora.Proposal (ProposalId (..))
import Plutus.V1.Ledger.Api (TxOutRef (..))
import Spec.Sample.Effect.GovernorMutation
import Spec.Sample.Shared
import Spec.Util (effectSucceedsWith)
import Test.Tasty (TestTree, testGroup)

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
