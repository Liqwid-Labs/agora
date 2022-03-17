module Spec.Stake (tests) where

--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Test.Tasty (TestTree, testGroup)

--------------------------------------------------------------------------------

import Plutarch.Builtin (pforgetData)

--------------------------------------------------------------------------------

import Agora.Stake (stakePolicy)

--------------------------------------------------------------------------------

import Spec.Sample.Stake qualified as Stake
import Spec.Util (policyFailsWith, policySucceedsWith)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
  [ testGroup
      "policy"
      [ policySucceedsWith
          "stakeCreation"
          (stakePolicy Stake.stake)
          (pforgetData (pconstantData ()))
          Stake.stakeCreation
      , policyFailsWith
          "stakeCreationWrongDatum"
          (stakePolicy Stake.stake)
          (pforgetData (pconstantData ()))
          Stake.stakeCreationWrongDatum
      , policyFailsWith
          "stakeCreationUnsigned"
          (stakePolicy Stake.stake)
          (pforgetData (pconstantData ()))
          Stake.stakeCreationUnsigned
      ]
  ]
