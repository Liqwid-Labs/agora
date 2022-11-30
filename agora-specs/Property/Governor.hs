{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

{- |
Module     : Property.Governor
Maintainer : seungheon.ooh@gmail.com
Description: Property tests for 'Governor' related functions.

Property model and tests for 'Governor' related functions
-}
module Property.Governor (props) where

import Agora.Governor (
  GovernorDatum (
    GovernorDatum,
    createProposalTimeRangeMaxWidth,
    maximumCreatedProposalsPerStake,
    nextProposalId,
    proposalThresholds,
    proposalTimings
  ),
  PGovernorDatum,
  pisGovernorDatumValid,
 )
import Agora.Governor.Scripts (governorPolicy)
import Agora.Proposal (
  ProposalId (ProposalId),
  ProposalThresholds (
    ProposalThresholds
  ),
 )
import Agora.Proposal.Time (
  MaxTimeRangeWidth (MaxTimeRangeWidth),
  ProposalTimingConfig (ProposalTimingConfig),
 )
import Data.Default (def)
import Data.Tagged (Tagged (Tagged))
import Data.Universe (Universe)
import Data.Universe.Class (Finite)
import Generics.SOP.NP (NP (Nil, (:*)))
import Optics (view)
import Plutarch.Api.V2 (PScriptContext)
import Plutarch.Builtin (pforgetData)
import Plutarch.Context (
  MintingBuilder,
  buildMinting',
  input,
  mint,
  output,
  script,
  withDatum,
  withMinting,
  withRef,
  withValue,
 )
import Plutarch.Evaluate (evalTerm)
import Plutarch.Extra.AssetClass (assetClassValue)
import Plutarch.Extra.Compile (mustCompile)
import Plutarch.Script (Script)
import Plutarch.Test.QuickCheck (
  Equality (OnPEq),
  Partiality (ByComplete),
  TestableTerm (TestableTerm),
  haskEquiv,
  pconstantT,
  shouldCrash,
  shouldRun,
 )
import PlutusLedgerApi.V2 (ScriptContext)
import Property.Generator (genInput, genOutput)
import Sample.Shared (
  deterministicTracingConfig,
  governor,
  governorAssetClass,
  governorScriptHash,
  governorSymbol,
  gstUTXORef,
 )
import Test.QuickCheck (
  Arbitrary (arbitrary),
  Gen,
  Property,
  arbitraryBoundedEnum,
  checkCoverage,
  choose,
  chooseInteger,
  cover,
  forAll,
  listOf1,
 )
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)

data GovernorDatumCases
  = ExecuteLE0
  | CreateLE0
  | ToVotingLE0
  | VoteLE0
  | CosignLE0
  | Correct
  deriving stock (Eq, Show, Enum, Bounded)
  deriving anyclass (Universe, Finite)

instance Arbitrary GovernorDatumCases where
  arbitrary = arbitraryBoundedEnum

{- | Property that checks `pisGovernorDatumValid` behaves as intended by
     comparing it to a simple haskell implementation.
-}
governorDatumValidProperty :: Property
governorDatumValidProperty =
  haskEquiv @('OnPEq) @('ByComplete)
    isValidModelImpl
    (TestableTerm pisGovernorDatumValid)
    (genDatum :* Nil)
  where
    genDatum :: Gen (TestableTerm PGovernorDatum)
    genDatum = pconstantT <$> (arbitrary >>= genDatumForCase)
      where
        genDatumForCase :: GovernorDatumCases -> Gen GovernorDatum
        genDatumForCase c = do
          thres <- genProposalThresholds c

          let timing = ProposalTimingConfig 0 0 0 0 0 0
          pure $
            GovernorDatum thres (ProposalId 0) timing (MaxTimeRangeWidth 1) 3
          where
            taggedInteger p = Tagged <$> chooseInteger p
            genProposalThresholds :: GovernorDatumCases -> Gen ProposalThresholds
            genProposalThresholds c = do
              let validGT = taggedInteger (0, 1000000000)
              execute <- validGT
              create <- validGT
              toVoting <- validGT
              vote <- validGT
              cosign <- validGT
              le0 <- taggedInteger (-1000, -1)

              case c of
                ExecuteLE0 ->
                  -- execute < 0
                  return $ ProposalThresholds le0 create toVoting vote cosign
                CreateLE0 ->
                  -- c < 0
                  return $ ProposalThresholds execute le0 toVoting vote cosign
                ToVotingLE0 ->
                  return $ ProposalThresholds execute create le0 vote cosign
                VoteLE0 ->
                  -- vote < 0
                  return $ ProposalThresholds execute create toVoting le0 cosign
                CosignLE0 ->
                  return $ ProposalThresholds execute create toVoting vote le0
                Correct ->
                  return $ ProposalThresholds execute create toVoting vote cosign

    -- \| This is a model Haskell implementation of `pisGovernorDatumValid`.
    isValidModelImpl :: GovernorDatum -> Bool
    isValidModelImpl = correctCase . classifier
      where
        correctCase = \case
          Correct -> True
          _ -> False

        classifier :: GovernorDatum -> GovernorDatumCases
        classifier
          ( view #proposalThresholds ->
              ProposalThresholds
                execute
                create
                toVoting
                vote
                cosign
            )
            | execute < 0 = ExecuteLE0
            | create < 0 = CreateLE0
            | toVoting < 0 = ToVotingLE0
            | vote < 0 = VoteLE0
            | cosign < 0 = CosignLE0
            | otherwise = Correct

--------------------------------------------------------------------------------

data GovernorPolicyCases
  = ReferenceUTXONotSpent
  | IncorrectAmountOfTokenMinted
  | GovernorOutputNotFound
  deriving stock (Eq, Show)

governorMintingPolicyTests :: [TestTree]
governorMintingPolicyTests =
  [ mkGovMintingCasePropertyTest
      "Reference input spend test"
      ReferenceUTXONotSpent
      "Spent"
      "Not spent"
  , mkGovMintingCasePropertyTest
      "Amount of token minted test"
      IncorrectAmountOfTokenMinted
      "Correct"
      "Incorrect"
  , mkGovMintingCasePropertyTest
      "Governor output presense"
      GovernorOutputNotFound
      "Present"
      "Absent"
  ]

{- | Creates a property by compiling governorPolicy script with given arguments
   and checking if it runs as expected by a test.
-}
governorPolicyValid :: ScriptContext -> Bool -> Property
governorPolicyValid ctx shouldSucceed =
  let mp = mkPolicyScript ctx in if shouldSucceed then shouldRun mp else shouldCrash mp

{-# INLINEABLE mkPolicyScript #-}
mkPolicyScript :: ScriptContext -> Script
mkPolicyScript ctx = mustCompile (go # pconstant ctx)
  where
    go :: forall (s :: S). Term s (PScriptContext :--> POpaque)
    go = loudEval $
      plam $ \sc ->
        governorPolicy
          # pconstant (view #gstOutRef governor)
          # pforgetData (pconstantData ())
          # sc

-- | Prepares a minting policy test for given policy error case.
mkGovMintingCasePropertyTest ::
  String ->
  GovernorPolicyCases ->
  String ->
  String ->
  TestTree
mkGovMintingCasePropertyTest name case' positiveCaseName negativeCaseName =
  testProperty name $
    forAll (gen case') $
      \(ctx, valid) ->
        checkCoverage $
          cover 48 valid positiveCaseName $
            cover 48 (not valid) negativeCaseName $
              governorPolicyValid ctx valid
  where
    gen :: GovernorPolicyCases -> Gen (ScriptContext, Bool)
    gen c = do
      inputs <- fmap mconcat . listOf1 $ genInput @MintingBuilder
      outputs <- fmap mconcat . listOf1 $ genOutput @MintingBuilder
      toks <- choose (2, 100)

      valid <- arbitrary
      let comp =
            if valid
              then referencedInput <> outputToGov <> mintAmount 1
              else case c of
                ReferenceUTXONotSpent -> outputToGov <> mintAmount 1
                IncorrectAmountOfTokenMinted ->
                  referencedInput
                    <> outputToGov
                    <> mintAmount toks
                GovernorOutputNotFound -> referencedInput <> mintAmount 1

      let ctx =
            buildMinting' $
              inputs
                <> outputs
                <> comp
                <> withMinting
                  governorSymbol
      pure (ctx, valid)
      where
        govDatum :: GovernorDatum
        govDatum =
          GovernorDatum
            { proposalThresholds = def
            , nextProposalId = ProposalId 0
            , proposalTimings = def
            , createProposalTimeRangeMaxWidth = def
            , maximumCreatedProposalsPerStake = 3
            }

        gst = assetClassValue governorAssetClass 1
        mintAmount x = mint . mconcat $ replicate x gst
        referencedInput = input $ withRef gstUTXORef
        outputToGov =
          output $
            mconcat
              [ script governorScriptHash
              , withValue gst
              , withDatum govDatum
              ]

props :: [TestTree]
props =
  [ adjustOption go . testProperty "governorDatumValid" $ governorDatumValidProperty
  , testGroup "governorPolicy" governorMintingPolicyTests
  ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 20_000

loudEval ::
  forall (p :: S -> Type).
  ClosedTerm p ->
  ClosedTerm p
loudEval x =
  case evalTerm deterministicTracingConfig x of
    Right (Right t, _, _) -> t
    Right (Left err, _, trace) -> error $ show err <> show trace
    Left err -> error $ show err
