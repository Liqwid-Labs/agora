{- |
Module     : Test.Specification
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
module Test.Specification (
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
  testValidator,
  testPolicy,

  -- * Converters
  toTestTree,
) where

import Agora.Utils (
  CompiledEffect (..),
  CompiledMintingPolicy (..),
  CompiledValidator (..),
 )
import Control.Composition ((.**), (.***))
import Data.Coerce (coerce)
import Data.Text qualified as Text
import Plutarch.Evaluate (evalScript)
import PlutusLedgerApi.V1.Scripts (
  Context (..),
  applyMintingPolicyScript,
  applyValidator,
 )
import PlutusLedgerApi.V2 (
  Datum (..),
  Redeemer (Redeemer),
  Script,
  ScriptContext,
  ToData (toBuiltinData),
 )
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
getSpecification :: String -> SpecificationTree -> [Specification]
getSpecification name (Terminal spec@(Specification sn _ _))
  | name == sn = [spec]
  | otherwise = []
getSpecification name (Group _ st) = mconcat $ getSpecification name <$> st

-- | Query specific @SpecificationTree@ from a tree.
getSpecificationTree :: String -> SpecificationTree -> [SpecificationTree]
getSpecificationTree name specTree@(Group gn st)
  | gn == name = [specTree]
  | otherwise = mconcat $ getSpecificationTree name <$> st
getSpecificationTree _ _ = []

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
    beautifyTraces =
      Text.unpack
        . Text.intercalate "\n"
        . map ("  " <>)
    (res, _budget, traces) = evalScript script
    ts = " Traces:\n" <> beautifyTraces traces
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

mkContext :: ScriptContext -> Context
mkContext = Context . toBuiltinData

mkRedeemer ::
  forall redeemer.
  (PlutusTx.ToData redeemer) =>
  redeemer ->
  Redeemer
mkRedeemer = Redeemer . toBuiltinData

mkDatum ::
  forall datum.
  (PlutusTx.ToData datum) =>
  datum ->
  Datum
mkDatum = Datum . toBuiltinData

applyMintingPolicy' ::
  (PlutusTx.ToData redeemer) =>
  CompiledMintingPolicy redeemer ->
  redeemer ->
  ScriptContext ->
  Script
applyMintingPolicy' policy redeemer scriptContext =
  applyMintingPolicyScript
    (mkContext scriptContext)
    policy.getCompiledMintingPolicy
    (mkRedeemer redeemer)

applyValidator' ::
  ( PlutusTx.ToData datum
  , PlutusTx.ToData redeemer
  ) =>
  CompiledValidator datum redeemer ->
  datum ->
  redeemer ->
  ScriptContext ->
  Script
applyValidator' validator datum redeemer scriptContext =
  applyValidator
    (mkContext scriptContext)
    validator.getCompiledValidator
    (mkDatum datum)
    (mkRedeemer redeemer)

-- | Check that a policy script succeeds, given a name and arguments.
policySucceedsWith ::
  (PlutusTx.ToData redeemer) =>
  String ->
  CompiledMintingPolicy redeemer ->
  redeemer ->
  ScriptContext ->
  SpecificationTree
policySucceedsWith tag =
  scriptSucceeds tag .** applyMintingPolicy'

-- | Check that a policy script fails, given a name and arguments.
policyFailsWith ::
  (PlutusTx.ToData redeemer) =>
  String ->
  CompiledMintingPolicy redeemer ->
  redeemer ->
  ScriptContext ->
  SpecificationTree
policyFailsWith tag =
  scriptFails tag .** applyMintingPolicy'

-- | Check that a validator script succeeds, given a name and arguments.
validatorSucceedsWith ::
  ( PlutusTx.ToData datum
  , PlutusTx.ToData redeemer
  ) =>
  String ->
  CompiledValidator datum redeemer ->
  datum ->
  redeemer ->
  ScriptContext ->
  SpecificationTree
validatorSucceedsWith tag =
  scriptSucceeds tag .*** applyValidator'

-- | Check that a validator script fails, given a name and arguments.
validatorFailsWith ::
  ( PlutusTx.ToData datum
  , PlutusTx.ToData redeemer
  ) =>
  String ->
  CompiledValidator datum redeemer ->
  datum ->
  redeemer ->
  ScriptContext ->
  SpecificationTree
validatorFailsWith tag =
  scriptFails tag .*** applyValidator'

-- | Check that an effect succeeds, given a name and argument.
effectSucceedsWith ::
  ( PlutusTx.ToData datum
  ) =>
  String ->
  CompiledEffect datum ->
  datum ->
  ScriptContext ->
  SpecificationTree
effectSucceedsWith tag eff datum = validatorSucceedsWith tag (coerce eff) datum ()

-- | Check that an effect fails, given a name and argument.
effectFailsWith ::
  ( PlutusTx.ToData datum
  ) =>
  String ->
  CompiledEffect datum ->
  datum ->
  ScriptContext ->
  SpecificationTree
effectFailsWith tag eff datum = validatorFailsWith tag (coerce eff) datum ()

-- | Test a validator, given the expectation as a boolean value.
testValidator ::
  forall datum redeemer.
  (PlutusTx.ToData datum, PlutusTx.ToData redeemer) =>
  -- | Is this test case expected to succeed?
  Bool ->
  String ->
  CompiledValidator datum redeemer ->
  datum ->
  redeemer ->
  ScriptContext ->
  SpecificationTree
testValidator isValid =
  if isValid
    then validatorSucceedsWith
    else validatorFailsWith

-- | Test a policy, given the expectation as a boolean value.
testPolicy ::
  forall redeemer.
  (PlutusTx.ToData redeemer) =>
  -- | Is this test case expected to succeed?
  Bool ->
  String ->
  CompiledMintingPolicy redeemer ->
  redeemer ->
  ScriptContext ->
  SpecificationTree
testPolicy isValid =
  if isValid
    then policySucceedsWith
    else policyFailsWith
