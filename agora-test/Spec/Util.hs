{- |
Module     : Spec.Util
Maintainer : emi@haskell.fyi
Description: Utility functions for testing Plutarch scripts with ScriptContext

Utility functions for testing Plutarch scripts with ScriptContext
-}
module Spec.Util (
  -- * Testing utils
  scriptSucceeds,
  scriptFails,
  policySucceedsWith,
  policyFailsWith,
  validatorSucceedsWith,
  validatorFailsWith,
  effectSucceedsWith,
  effectFailsWith,

  -- * Plutus-land utils
  datumHash,
  toDatum,
  toDatumHash,
  datumPair,
) where

--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as ByteString.Lazy

--------------------------------------------------------------------------------

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)

--------------------------------------------------------------------------------

import Plutarch.Api.V1 (PMintingPolicy, PValidator)
import Plutarch.Builtin (pforgetData)
import Plutarch.Crypto (pblake2b_256)
import Plutarch.Evaluate (evalScript)
import Plutarch.Lift (PUnsafeLiftDecl (PLifted))
import Plutus.V1.Ledger.Contexts (ScriptContext)
import Plutus.V1.Ledger.Scripts (Datum (Datum), DatumHash (DatumHash), Script)
import PlutusTx.Builtins qualified as PlutusTx
import PlutusTx.IsData qualified as PlutusTx

--------------------------------------------------------------------------------

-- | Check that a policy script succeeds, given a name and arguments.
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

-- | Check that a policy script fails, given a name and arguments.
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

-- | Check that a validator script succeeds, given a name and arguments.
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

-- | Check that a validator script fails, given a name and arguments.
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


-- | Check that a validator script succeeds, given a name and arguments.
effectSucceedsWith ::
  ( PLift datum
  , PlutusTx.ToData (PLifted datum)
  ) =>
  String ->
  ClosedTerm PValidator ->
  PLifted datum ->
  ScriptContext ->
  TestTree
effectSucceedsWith tag eff datum scriptContext =
  scriptSucceeds tag $
    compile
      ( eff
          # pforgetData (pconstantData datum)
          # pforgetData (pconstantData ())
          # pconstant scriptContext
      )

-- | Check that a validator script fails, given a name and arguments.
effectFailsWith ::
  ( PLift datum
  , PlutusTx.ToData (PLifted datum)
  ) =>
  String ->
  ClosedTerm PValidator ->
  PLifted datum ->
  ScriptContext ->
  TestTree
effectFailsWith tag eff datum scriptContext =
  scriptFails tag $
    compile
      ( eff
          # pforgetData (pconstantData datum)
          # pforgetData (pconstantData ())
          # pconstant scriptContext
      )

-- | Check that an arbitrary script doesn't error when evaluated, given a name.
scriptSucceeds :: String -> Script -> TestTree
scriptSucceeds name script = testCase name $ do
  let (res, _budget, traces) = evalScript script
  case res of
    Left e -> do
      assertFailure $
        show e <> " Traces: " <> show traces
    Right _v ->
      pure ()

-- | Check that an arbitrary script **does** error when evaluated, given a name.
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

{- | Create a pair from data for use in 'txInfoData'.

   Example:
   @
     myTxInfo { 'txInfoData' = ['datumPair' myDatum] }
   @
-}
datumPair :: PlutusTx.ToData a => a -> (DatumHash, Datum)
datumPair = (,) <$> toDatumHash <*> toDatum

-- | Calculate the blake2b-256 hash of a Datum.
datumHash :: Datum -> DatumHash
datumHash (Datum data') = toDatumHash data'

-- | Convenience function to create a Datum from any type that implements ToData.
toDatum :: PlutusTx.ToData a => a -> Datum
toDatum = Datum . PlutusTx.toBuiltinData

{- | Calculate the blake2b-256 hash of any type that implements ToData

     Shamelessly go through plutus.
-}
toDatumHash :: PlutusTx.ToData a => a -> DatumHash
toDatumHash datum =
  DatumHash $
    PlutusTx.toBuiltin $
      plift $
        pblake2b_256
          # pconstant (ByteString.Lazy.toStrict $ serialise $ PlutusTx.toData datum)
