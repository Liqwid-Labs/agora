module Spec.Effect.GovernorMutation (tests) where

import Agora.Effect.GovernorMutation (mutateGovernorValidator)
import Agora.Governor (GovernorDatum (..), GovernorRedeemer (MutateGovernor))
import Agora.Governor.Scripts (governorValidator)
import Agora.Proposal (ProposalId (..))
import Data.Default.Class (Default (def))
import Plutus.V1.Ledger.Api (ScriptContext (ScriptContext), ScriptPurpose (Spending))
import Sample.Effect.GovernorMutation (
  effectRef,
  govRef,
  invalidNewGovernorDatum,
  mkEffectDatum,
  mkEffectTxInfo,
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
              "governor validator should pass"
              (governorValidator Shared.governor)
              ( GovernorDatum
                  Shared.defaultProposalThresholds
                  (ProposalId 0)
                  def
                  def
              )
              MutateGovernor
              ( ScriptContext
                  (mkEffectTxInfo validNewGovernorDatum)
                  (Spending govRef)
              )
          , effectSucceedsWith
              "effect validator should pass"
              (mutateGovernorValidator Shared.governor)
              (mkEffectDatum validNewGovernorDatum)
              (ScriptContext (mkEffectTxInfo validNewGovernorDatum) (Spending effectRef))
          ]
      , testGroup
          "invalid new governor datum"
          [ validatorFailsWith
              "governor validator should fail"
              (governorValidator Shared.governor)
              ( GovernorDatum
                  Shared.defaultProposalThresholds
                  (ProposalId 0)
                  def
                  def
              )
              MutateGovernor
              ( ScriptContext
                  (mkEffectTxInfo invalidNewGovernorDatum)
                  (Spending govRef)
              )
          , effectFailsWith
              "effect validator should fail"
              (mutateGovernorValidator Shared.governor)
              (mkEffectDatum validNewGovernorDatum)
              (ScriptContext (mkEffectTxInfo invalidNewGovernorDatum) (Spending effectRef))
          ]
      ]
  ]
