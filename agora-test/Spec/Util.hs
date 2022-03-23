module Spec.Util (
  -- * Testing utils
  scriptSucceeds,
  scriptFails,
  policySucceedsWith,
  policyFailsWith,
  validatorSucceedsWith,
  validatorFailsWith,

  -- * Plutus land utils
  datumHash,
  toDatum,
  toDatumHash,
  datumPair,
) where

--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as LBS

--------------------------------------------------------------------------------

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)

--------------------------------------------------------------------------------

import Plutarch
import Plutarch.Api.V1 (PMintingPolicy, PValidator)
import Plutarch.Builtin (pforgetData)
import Plutarch.Crypto (pblake2b_256)
import Plutarch.Evaluate (evalScript)
import Plutarch.Lift (PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude ()
import Plutus.V1.Ledger.Contexts (ScriptContext)
import Plutus.V1.Ledger.Scripts (Datum (Datum), DatumHash (DatumHash), Script)
import PlutusTx.Builtins qualified as PlutusTx
import PlutusTx.IsData qualified as PlutusTx

--------------------------------------------------------------------------------

policySucceedsWith ::
  ( PLift redeemer
  , PlutusTx.ToData (PLifted redeemer)
  ) =>
  String ->
  ClosedTerm PMintingPolicy ->
  PLifted redeemer ->
  ScriptContext ->
  TestTree
policySucceedsWith tag policy redeemer scriptContext =
  scriptSucceeds tag $
    compile
      ( policy
          # pforgetData (pconstantData redeemer)
          # pconstant scriptContext
      )

policyFailsWith ::
  ( PLift redeemer
  , PlutusTx.ToData (PLifted redeemer)
  ) =>
  String ->
  ClosedTerm PMintingPolicy ->
  PLifted redeemer ->
  ScriptContext ->
  TestTree
policyFailsWith tag policy redeemer scriptContext =
  scriptFails tag $
    compile
      ( policy
          # pforgetData (pconstantData redeemer)
          # pconstant scriptContext
      )

validatorSucceedsWith ::
  ( PLift datum
  , PlutusTx.ToData (PLifted datum)
  , PLift redeemer
  , PlutusTx.ToData (PLifted redeemer)
  ) =>
  String ->
  ClosedTerm PValidator ->
  PLifted datum ->
  PLifted redeemer ->
  ScriptContext ->
  TestTree
validatorSucceedsWith tag policy datum redeemer scriptContext =
  scriptSucceeds tag $
    compile
      ( policy
          # pforgetData (pconstantData datum)
          # pforgetData (pconstantData redeemer)
          # pconstant scriptContext
      )

validatorFailsWith ::
  ( PLift datum
  , PlutusTx.ToData (PLifted datum)
  , PLift redeemer
  , PlutusTx.ToData (PLifted redeemer)
  ) =>
  String ->
  ClosedTerm PValidator ->
  PLifted datum ->
  PLifted redeemer ->
  ScriptContext ->
  TestTree
validatorFailsWith tag policy datum redeemer scriptContext =
  scriptFails tag $
    compile
      ( policy
          # pforgetData (pconstantData datum)
          # pforgetData (pconstantData redeemer)
          # pconstant scriptContext
      )

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

--------------------------------------------------------------------------------

{- | Create a pair from data for use in 'txInfoData'

   Example:
   @
     myTxInfo { 'txInfoData' = ['datumPair' myDatum] }
   @
-}
datumPair :: PlutusTx.ToData a => a -> (DatumHash, Datum)
datumPair = (,) <$> toDatumHash <*> toDatum

datumHash :: Datum -> DatumHash
datumHash (Datum data') = toDatumHash data'

toDatum :: PlutusTx.ToData a => a -> Datum
toDatum = Datum . PlutusTx.toBuiltinData

-- Shamelessly go through plutus.
toDatumHash :: PlutusTx.ToData a => a -> DatumHash
toDatumHash datum =
  DatumHash $
    PlutusTx.toBuiltin $
      plift $
        pblake2b_256
          # pconstant (LBS.toStrict $ serialise $ PlutusTx.toData datum)
