module Spec.Effect.GovernorMutation (tests) where

import Agora.Effect.GovernorMutation (mutateGovernorValidator)
import Agora.Governor (GovernorDatum (..), GovernorRedeemer (MutateGovernor))
import Agora.Governor.Scripts (governorValidator)
import Agora.Proposal (ProposalId (..))
import Plutus.V1.Ledger.Api (ScriptContext (ScriptContext), ScriptPurpose (Spending))
import Sample.Effect.GovernorMutation (
  effectRef,
  govRef,
  invalidNewGovernorDatum,
  mkEffectDatum,
  mkEffectTransaction,
  validNewGovernorDatum,
 )
import Sample.Shared qualified as Shared
import Test.Tasty (TestTree, testGroup)
import Test.Util (effectFailsWith, effectSucceedsWith, validatorFailsWith, validatorSucceedsWith)

tests :: [TestTree]
tests =
  [ testGroup
      "validator"
      [ testGroup
          "valid new governor datum"
          [ validatorSucceedsWith
              "governor"
              (governorValidator Shared.governor)
              ( GovernorDatum
                  { proposalThresholds = Shared.defaultProposalThresholds
                  , nextProposalId = ProposalId 0
                  }
              )
              MutateGovernor
              ( ScriptContext
                  (mkEffectTransaction validNewGovernorDatum)
                  (Spending govRef)
              )
          , effectSucceedsWith
              "effect"
              (mutateGovernorValidator Shared.governor)
              (mkEffectDatum validNewGovernorDatum)
              (ScriptContext (mkEffectTransaction validNewGovernorDatum) (Spending effectRef))
          ]
      , testGroup
          "invalid new governor datum"
          [ validatorFailsWith
              "governor"
              (governorValidator Shared.governor)
              ( GovernorDatum
                  { proposalThresholds = Shared.defaultProposalThresholds
                  , nextProposalId = ProposalId 0
                  }
              )
              MutateGovernor
              ( ScriptContext
                  (mkEffectTransaction invalidNewGovernorDatum)
                  (Spending govRef)
              )
          , effectFailsWith
              "effect"
              (mutateGovernorValidator Shared.governor)
              (mkEffectDatum validNewGovernorDatum)
              (ScriptContext (mkEffectTransaction invalidNewGovernorDatum) (Spending effectRef))
          ]
      ]
  ]
