module Spec.Effect.GovernorMutation (specs) where

import Agora.Effect.GovernorMutation (mutateGovernorValidator)
import Agora.Governor (GovernorDatum (..), GovernorRedeemer (MutateGovernor))
import Agora.Proposal (ProposalId (..))
import Agora.Scripts (AgoraScripts (..))
import Data.Default.Class (Default (def))
import PlutusLedgerApi.V1 (ScriptContext (ScriptContext), ScriptPurpose (Spending))
import Sample.Effect.GovernorMutation (
  effectRef,
  govRef,
  invalidNewGovernorDatum,
  mkEffectDatum,
  mkEffectTxInfo,
  validNewGovernorDatum,
 )
import Sample.Shared (agoraScripts, mkEffect)
import Test.Specification (
  SpecificationTree,
  effectFailsWith,
  effectSucceedsWith,
  group,
  validatorFailsWith,
  validatorSucceedsWith,
 )

specs :: [SpecificationTree]
specs =
  [ group
      "validator"
      [ group
          "valid new governor datum"
          [ validatorSucceedsWith
              "governor validator should pass"
              agoraScripts.compiledGovernorValidator
              ( GovernorDatum
                  def
                  (ProposalId 0)
                  def
                  def
                  3
              )
              MutateGovernor
              ( ScriptContext
                  (mkEffectTxInfo validNewGovernorDatum)
                  (Spending govRef)
              )
          , effectSucceedsWith
              "effect validator should pass"
              (mkEffect $ mutateGovernorValidator agoraScripts)
              (mkEffectDatum validNewGovernorDatum)
              (ScriptContext (mkEffectTxInfo validNewGovernorDatum) (Spending effectRef))
          ]
      , group
          "invalid new governor datum"
          [ validatorFailsWith
              "governor validator should fail"
              agoraScripts.compiledGovernorValidator
              ( GovernorDatum
                  def
                  (ProposalId 0)
                  def
                  def
                  3
              )
              MutateGovernor
              ( ScriptContext
                  (mkEffectTxInfo invalidNewGovernorDatum)
                  (Spending govRef)
              )
          , effectFailsWith
              "effect validator should fail"
              (mkEffect $ mutateGovernorValidator agoraScripts)
              (mkEffectDatum validNewGovernorDatum)
              (ScriptContext (mkEffectTxInfo invalidNewGovernorDatum) (Spending effectRef))
          ]
      ]
  ]
