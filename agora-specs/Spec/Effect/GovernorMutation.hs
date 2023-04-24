module Spec.Effect.GovernorMutation (specs) where

import Agora.Governor (GovernorDatum (..), GovernorRedeemer (MutateGovernor))
import Agora.Proposal (ProposalId (..))
import Data.Default.Class (Default (def))
import PlutusLedgerApi.V2 (ScriptContext (ScriptContext), ScriptPurpose (Spending))
import Sample.Effect.GovernorMutation (
  effectRef,
  effectValidator,
  govRef,
  invalidNewGovernorDatum,
  mkEffectDatum,
  mkEffectTxInfo,
  validNewGovernorDatum,
 )
import Sample.Shared (governorValidator)
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
              governorValidator
              ( GovernorDatum
                  def
                  nextProposalId
                  def
                  def
                  3
              )
              MutateGovernor
              ( ScriptContext
                  (mkEffectTxInfo validNewGovernorDatum')
                  (Spending govRef)
              )
          , effectSucceedsWith
              "effect validator should pass"
              effectValidator
              ( mkEffectDatum
                  ( GovernorDatum
                      def
                      nextProposalId
                      def
                      def
                      3
                  )
                  validNewGovernorDatum
              )
              (ScriptContext (mkEffectTxInfo validNewGovernorDatum') (Spending effectRef))
          ]
      , group
          "invalid new governor datum"
          [ validatorFailsWith
              "governor validator should fail"
              governorValidator
              ( GovernorDatum
                  def
                  nextProposalId
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
              effectValidator
              ( mkEffectDatum
                  ( GovernorDatum
                      def
                      nextProposalId
                      def
                      def
                      3
                  )
                  validNewGovernorDatum
              )
              (ScriptContext (mkEffectTxInfo invalidNewGovernorDatum) (Spending effectRef))
          ]
      ]
  ]
  where
    validNewGovernorDatum' :: GovernorDatum
    validNewGovernorDatum' = validNewGovernorDatum {nextProposalId}
    -- \^ The datum value pinned by the effect, disregarding the proposal ID and
    --   taking this field from the governor input instead

    nextProposalId :: ProposalId
    nextProposalId = ProposalId 0
