module Spec.Util (
  scriptSucceeds,
  scriptFails,
  policySucceedsWith,
  policyFailsWith,
) where

--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)

--------------------------------------------------------------------------------

import Plutarch
import Plutarch.Api.V1 (PMintingPolicy)
import Plutarch.Evaluate (evalScript)
import Plutarch.Prelude ()
import Plutus.V1.Ledger.Scripts (Script)

--------------------------------------------------------------------------------

policySucceedsWith :: String -> ClosedTerm PMintingPolicy -> ClosedTerm PData -> _ -> TestTree
policySucceedsWith tag policy redeemer scriptContext =
  scriptSucceeds tag $ compile (policy # redeemer # pconstant scriptContext)

policyFailsWith :: String -> ClosedTerm PMintingPolicy -> ClosedTerm PData -> _ -> TestTree
policyFailsWith tag policy redeemer scriptContext =
  scriptFails tag $ compile (policy # redeemer # pconstant scriptContext)

scriptSucceeds :: String -> Script -> TestTree
scriptSucceeds name script = testCase name $ do
  let (res, _budget, traces) = evalScript script
  case res of
    Left e -> do
      assertFailure $
        show e <> " Traces: " <> show traces
    Right _v ->
      pure ()

scriptFails :: String -> Script -> TestTree
scriptFails name script = testCase name $ do
  let (res, _budget, traces) = evalScript script
  case res of
    Left _e ->
      pure ()
    Right v ->
      assertFailure $
        "Expected failure, but succeeded. " <> show v <> " Traces: " <> show traces
