module Spec.Util (scriptTest) where

--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)

--------------------------------------------------------------------------------

import Plutarch.Evaluate (evalScript)
import Plutus.V1.Ledger.Scripts (Script)

--------------------------------------------------------------------------------

scriptTest :: String -> Script -> TestTree
scriptTest name script = testCase name $ do
  let (res, _budget, traces) = evalScript script
  case res of
    Left e -> do
      assertFailure (show e <> " Traces: " <> show traces)
    Right _v -> pure ()
