module Spec.Stake (tests) where

--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Test.Tasty (TestTree, testGroup)

--------------------------------------------------------------------------------

import Plutarch (compile)
import Plutarch.Builtin (pforgetData)

--------------------------------------------------------------------------------

import Agora.Stake (stakePolicy)

--------------------------------------------------------------------------------

import Spec.Sample.Stake qualified as Stake
import Spec.Util (scriptTest)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
  [ testGroup
      "policy"
      [ scriptTest "minting" (compile $ stakePolicy Stake.stake # pforgetData (pconstantData ()) # pconstant Stake.stakeCreation)
      ]
  ]
