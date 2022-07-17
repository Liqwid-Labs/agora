{- |
Module     : Spec.Governor
Maintainer : connor@mlabs.city
Description: Tests for Agora governor.

Thie module exports `specs`, a list of `TestTree`s, which ensure
that Agora's governor component workds as intended.

Tests should pass when the validator or policy is given one of the
valid script contexts, which are defined in 'Agora.Sample.Governor'.

TODO: Add negative test cases, see [#76](https://github.com/Liqwid-Labs/agora/issues/76).
-}
module Spec.Governor (specs) where

import Agora.Governor (GovernorDatum (..), GovernorRedeemer (..))
import Agora.Governor.Scripts (governorValidator)
import Agora.Proposal (ProposalId (..))
import Data.Default.Class (Default (def))
import Sample.Governor (mintGATs, mutateState)
import Sample.Governor.Initialize qualified as GST
import Sample.Shared qualified as Shared
import Test.Specification (
  SpecificationTree,
  group,
  validatorSucceedsWith,
 )

-- | The SpecificationTree exported by this module.
specs :: [SpecificationTree]
specs =
  [ group
      "policy"
      [ GST.mkTestCase "totally legal" GST.totallyValidParameters True
      , group
          "illegal"
          [ GST.mkTestCase "invalid thresholds" GST.invalidDatumThresholdsParameters False
          , GST.mkTestCase
              "invalid max time range width for proposal creation"
              GST.invalidDatumMaxTimeRangeWidthParameters
              False
          , GST.mkTestCase "invalid timings" GST.invalidDatumTimingConfigParameters False
          , GST.mkTestCase "no governor datum" GST.withoutGovernorDatumParameters False
          , GST.mkTestCase "no witness UTXO" GST.witnessNotPresentedParameters False
          , GST.mkTestCase "mint more than one GST" GST.mintMoreThanOneGSTParameters False
          , GST.mkTestCase "GST has non-empty name" GST.mintGSTWithNoneEmptyNameParameters False
          ]
      ]
  , group
      "validator"
      [ validatorSucceedsWith
          "GATs minting"
          (governorValidator Shared.governor)
          ( GovernorDatum
              def
              (ProposalId 5)
              def
              def
              3
          )
          MintGATs
          mintGATs
      , validatorSucceedsWith
          "mutate governor state"
          (governorValidator Shared.governor)
          ( GovernorDatum
              def
              (ProposalId 5)
              def
              def
              3
          )
          MutateGovernor
          mutateState
      ]
  ]
