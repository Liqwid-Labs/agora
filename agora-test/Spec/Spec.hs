module Spec.Spec (
  Specification (..),
  SpecificationExpectation (..),
  SpecificationTree (..),
  group,
  toTestTree,
  getSpecification,
  getSpecificationTree,
  scriptSucceeds,
  scriptFails,
  policySucceedsWith,
  policyFailsWith,
  validatorSucceedsWith,
  validatorFailsWith,
  effectSucceedsWith,
  effectFailsWith,
) where

import Data.Maybe (catMaybes)
import Plutarch.Api.V1 (PMintingPolicy, PValidator)
import Plutarch.Builtin (pforgetData)
import Plutarch.Evaluate (evalScript)
import Plutarch.Lift (PUnsafeLiftDecl (PLifted))
import Plutus.V1.Ledger.Api (Script, ScriptContext)
import PlutusTx.IsData qualified as PlutusTx (ToData)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

data SpecificationExpectation
  = Success
  | Failure
  | FailureWith String
  deriving stock (Show)

data Specification = Specification
  { sName :: String
  , sExpectation :: SpecificationExpectation
  , sScript :: Script
  }
  deriving stock (Show)

data SpecificationTree
  = Terminal Specification
  | Group String [SpecificationTree]
  deriving stock (Show)

exists :: String -> SpecificationTree -> Bool
exists s (Terminal (Specification name _ _)) = s == name
exists s (Group name st) = or (exists s <$> st) || s == name

group :: String -> [SpecificationTree] -> SpecificationTree
group name st
  | or $ exists name <$> st = error "Name already exists"
  | otherwise = Group name st

getSpecification :: String -> SpecificationTree -> Maybe Specification
getSpecification name (Terminal spec@(Specification sn _ _))
  | name == sn = Just spec
  | otherwise = Nothing
getSpecification name (Group _ st)
  | length specs == 1 = Just $ head specs
  | otherwise = Nothing
  where
    specs = catMaybes $ getSpecification name <$> st

getSpecificationTree :: String -> SpecificationTree -> Maybe SpecificationTree
getSpecificationTree name specTree@(Group gn st)
  | gn == name = Just specTree
  | length trees == 1 = Just $ head trees
  | otherwise = Nothing
  where
    trees = catMaybes $ getSpecificationTree name <$> st
getSpecificationTree _ _ = Nothing

toTestTree :: SpecificationTree -> TestTree
toTestTree (Group name st) = testGroup name $ toTestTree <$> st
toTestTree (Terminal (Specification name expectation script)) =
  testCase name $ do
    case expectation of
      Success -> onSuccess
      Failure -> onFailure
      FailureWith s -> onFailureWith s
  where
    (res, _budget, traces) = evalScript script
    ts = " Traces: " <> show traces
    onSuccess = case res of
      Left e ->
        assertFailure $
          show e <> ts
      _ -> pure ()
    onFailure = case res of
      Right v ->
        assertFailure $
          "Expected failure, but succeeded. "
            <> show v
            <> ts
      _ -> pure ()
    onFailureWith _s = case res of -- TODO: check Trace for this
      Right v ->
        assertFailure $
          "Expected failure, but succeeded. "
            <> show v
            <> ts
      _ -> pure ()

scriptSucceeds :: String -> Script -> SpecificationTree
scriptSucceeds name script = Terminal $ Specification name Success script

scriptFails :: String -> Script -> SpecificationTree
scriptFails name script = Terminal $ Specification name Failure script

policySucceedsWith ::
  ( PLift redeemer
  , PlutusTx.ToData (PLifted redeemer)
  ) =>
  String ->
  ClosedTerm PMintingPolicy ->
  PLifted redeemer ->
  ScriptContext ->
  SpecificationTree
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
  SpecificationTree
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
  SpecificationTree
validatorSucceedsWith tag validator datum redeemer scriptContext =
  scriptSucceeds tag $
    compile
      ( validator
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
  SpecificationTree
validatorFailsWith tag validator datum redeemer scriptContext =
  scriptFails tag $
    compile
      ( validator
          # pforgetData (pconstantData datum)
          # pforgetData (pconstantData redeemer)
          # pconstant scriptContext
      )

effectSucceedsWith ::
  ( PLift datum
  , PlutusTx.ToData (PLifted datum)
  ) =>
  String ->
  ClosedTerm PValidator ->
  PLifted datum ->
  ScriptContext ->
  SpecificationTree
effectSucceedsWith tag eff datum = validatorSucceedsWith tag eff datum ()

effectFailsWith ::
  ( PLift datum
  , PlutusTx.ToData (PLifted datum)
  ) =>
  String ->
  ClosedTerm PValidator ->
  PLifted datum ->
  ScriptContext ->
  SpecificationTree
effectFailsWith tag eff datum = validatorFailsWith tag eff datum ()
