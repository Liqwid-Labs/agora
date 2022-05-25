{- |
Module     : Spec.Specification
Maintainer : seungheon.ooh@gmail.com
Description: Helpers to build Specification for testing and bench-marking

Constructors for building a specification for Plutarch scripts:

  - 'policySucceedsWith': checks that a minting policy succeeds.

  - 'policyFailsWith': checks that a minting policy fails.

  - 'validatorSucceedsWith': checks that validator succeeds.

  - 'validatorFailsWith': checks that validator fails.

  - 'effectSucceedsWith': checks that effect succeeds.

  - 'effectFailsWith': checks that effect fails.

  - 'scriptSucceeds': checks that an arbitrary script does not
    `perror`.

  - 'scriptFails': checks that an arbitrary script `perror`s out.
-}
module Spec.Specification (
  -- * Structures
  Specification (..),
  SpecificationExpectation (..),
  SpecificationTree (..),

  -- * Spec helpers
  group,
  getSpecification,
  getSpecificationTree,

  -- * Spec builders
  scriptSucceeds,
  scriptFails,
  policySucceedsWith,
  policyFailsWith,
  validatorSucceedsWith,
  validatorFailsWith,
  effectSucceedsWith,
  effectFailsWith,

  -- * Converters
  toTestTree,
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

{- | Expectations upon execution of script
 @Success@ indicates a successful execution.
 @Failure@ inidcates a faulty execution.
 @FailureWith@ indicates a faulty execution but with expected reason for failure.
-}
data SpecificationExpectation
  = Success
  | Failure
  | FailureWith String
  deriving stock (Show)

{- | Unit of specification. @Specification@ holds name, expectation, and
  script to be tested or executed later on.
-}
data Specification = Specification
  { sName :: String
  , sExpectation :: SpecificationExpectation
  , sScript :: Script
  }
  deriving stock (Show)

-- | Tree-structure to group alike specifications--modeled after @TestTree@ from tasty.
data SpecificationTree
  = Terminal Specification
  | Group String [SpecificationTree]
  deriving stock (Show)

{- | Checks if given name exists in @SpecificationTree@ as either
 group name or specification name.
-}
exists :: String -> SpecificationTree -> Bool
exists s (Terminal (Specification name _ _)) = s == name
exists s (Group name st) = or (exists s <$> st) || s == name

-- | Groups alike @SpecificationTree@s into a bigger tree.
group :: String -> [SpecificationTree] -> SpecificationTree
group name st
  | or $ exists name <$> st = error $ "Name already exists: " <> name
  | otherwise = Group name st

-- | Query specific @Specification@ from a tree.
getSpecification :: String -> SpecificationTree -> Maybe Specification
getSpecification name (Terminal spec@(Specification sn _ _))
  | name == sn = Just spec
  | otherwise = Nothing
getSpecification name (Group _ st)
  | length specs == 1 = Just $ head specs
  | otherwise = Nothing
  where
    specs = catMaybes $ getSpecification name <$> st

-- | Query specific @SpecificationTree@ from a tree.
getSpecificationTree :: String -> SpecificationTree -> Maybe SpecificationTree
getSpecificationTree name specTree@(Group gn st)
  | gn == name = Just specTree
  | length trees == 1 = Just $ head trees
  | otherwise = Nothing
  where
    trees = catMaybes $ getSpecificationTree name <$> st
getSpecificationTree _ _ = Nothing

-- | Convert @SpecificationTree@ into @TestTree@ to be used as a unit test.
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

-- | Check that an arbitrary script doesn't error when evaluated, given a name.
scriptSucceeds :: String -> Script -> SpecificationTree
scriptSucceeds name script = Terminal $ Specification name Success script

-- | Check that an arbitrary script **does** error when evaluated, given a name.
scriptFails :: String -> Script -> SpecificationTree
scriptFails name script = Terminal $ Specification name Failure script

-- | Check that a policy script succeeds, given a name and arguments.
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

-- | Check that a policy script fails, given a name and arguments.
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
  SpecificationTree
validatorSucceedsWith tag validator datum redeemer scriptContext =
  scriptSucceeds tag $
    compile
      ( validator
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
  SpecificationTree
validatorFailsWith tag validator datum redeemer scriptContext =
  scriptFails tag $
    compile
      ( validator
          # pforgetData (pconstantData datum)
          # pforgetData (pconstantData redeemer)
          # pconstant scriptContext
      )

-- | Check that an effect succeeds, given a name and argument.
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

-- | Check that an effect fails, given a name and argument.
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
