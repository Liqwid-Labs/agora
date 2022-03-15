module Spec.Stake (tests) where

--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

--------------------------------------------------------------------------------

import Plutarch (compile)
import Plutarch.Evaluate (evalScript)
import Plutus.V1.Ledger.Scripts (Script)

--------------------------------------------------------------------------------

import Agora.Stake (stakePolicy)

--------------------------------------------------------------------------------

import Plutarch.Builtin (pforgetData)
import Spec.Sample.Stake qualified as Stake

--------------------------------------------------------------------------------
tests :: [TestTree]
tests =
  [ testGroup
      "policy"
      [ scriptTest "minting" (compile $ stakePolicy Stake.stake # pforgetData (pconstantData ()) # pconstant Stake.stakeCreation)
      ]
  ]

scriptTest :: String -> Script -> TestTree
scriptTest name script = testCase name $ do
  let (res, _budget, traces) = evalScript script
  case res of
    Left e -> do
      assertFailure (show e <> " Traces: " <> show traces)
    Right _v -> pure ()
