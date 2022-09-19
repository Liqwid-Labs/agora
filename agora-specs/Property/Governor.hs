{- |
Module     : Property.Governor
Maintainer : seungheon.ooh@gmail.com
Description: Property tests for 'Governor' related functions.

Property model and tests for 'Governor' related functions
-}
module Property.Governor (props) where

import Agora.Governor (Governor (gstOutRef), GovernorDatum (..), pisGovernorDatumValid)
import Agora.Governor.Scripts (governorPolicy)
import Agora.Proposal (
  ProposalId (ProposalId),
  ProposalThresholds (ProposalThresholds),
 )
import Agora.Proposal.Time (
  MaxTimeRangeWidth (MaxTimeRangeWidth),
  ProposalTimingConfig (ProposalTimingConfig),
 )
import Data.Default.Class (Default (def))
import Data.Tagged (Tagged (Tagged), untag)
import Data.Universe (Finite (..), Universe (..))
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
import PlutusLedgerApi.V1.Value (assetClassValue)
import PlutusLedgerApi.V2 (
  ScriptContext (scriptContextTxInfo),
  TxInInfo (txInInfoOutRef),
  TxInfo (txInfoInputs, txInfoMint, txInfoOutputs),
  TxOut (txOutValue),
 )
import Property.Generator (genInput, genOutput)
import Sample.Shared (
  govAssetClass,
  govSymbol,
  govValidatorHash,
  governor,
  gstUTXORef,
 )
import Test.Tasty (TestTree)
import Test.Tasty.Plutarch.Property (classifiedPropertyNative)
import Test.Tasty.QuickCheck (
  Gen,
  Property,
  choose,
  chooseInteger,
  listOf1,
  testProperty,
 )

data GovernorDatumCases
  = ExecuteLE0
  | CreateLE0
  | VoteLE0
  | Correct
  deriving stock (Eq, Show)

instance Universe GovernorDatumCases where
  universe =
    [ ExecuteLE0
    , CreateLE0
    , VoteLE0
    , Correct
    ]

instance Finite GovernorDatumCases where
  universeF = universe
  cardinality = Tagged 6

{- | Property that checks `governorDatumValid`.
 `governorDatumValid` determines if given governor datum is valid or not. This property
 ensures `governorDatumValid` is checking the datum correctly and ruling out improper datum.
-}
governorDatumValidProperty :: Property
governorDatumValidProperty =
  classifiedPropertyNative gen (const []) expected classifier pisGovernorDatumValid
  where
    classifier :: GovernorDatum -> GovernorDatumCases
    classifier ((.proposalThresholds) -> ProposalThresholds e c v)
      | e < 0 = ExecuteLE0
      | c < 0 = CreateLE0
      | v < 0 = VoteLE0
      | otherwise = Correct

    expected :: GovernorDatum -> Maybe Bool
    expected c = Just $ classifier c == Correct

    gen :: GovernorDatumCases -> Gen GovernorDatum
    gen c = do
      thres <- genProposalThresholds c

      let timing = ProposalTimingConfig 0 0 0 0
      return $ GovernorDatum thres (ProposalId 0) timing (MaxTimeRangeWidth 1) 3
      where
        taggedInteger p = Tagged <$> chooseInteger p
        genProposalThresholds :: GovernorDatumCases -> Gen ProposalThresholds
        genProposalThresholds c = do
          let validGT = taggedInteger (0, 1000000000)
          execute <- validGT
          create <- validGT
          vote <- validGT
          le0 <- taggedInteger (-1000, -1)

          case c of
            ExecuteLE0 ->
              -- execute < 0
              return $ ProposalThresholds le0 create vote
            CreateLE0 ->
              -- c < 0
              return $ ProposalThresholds execute le0 vote
            VoteLE0 ->
              -- vote < 0
              return $ ProposalThresholds execute create le0
            Correct -> do
              -- c <= vote < execute
              nv <- taggedInteger (0, untag execute - 1)
              nc <- taggedInteger (0, untag nv)
              return $ ProposalThresholds execute nc nv

data GovernorPolicyCases
  = ReferenceUTXONotSpent
  | IncorrectAmountOfTokenMinted
  | GovernorOutputNotFound
  | GovernorPolicyCorrect
  deriving stock (Eq, Show)

instance Universe GovernorPolicyCases where
  universe =
    [ ReferenceUTXONotSpent
    , IncorrectAmountOfTokenMinted
    , GovernorOutputNotFound
    , GovernorPolicyCorrect
    ]

instance Finite GovernorPolicyCases where
  universeF = universe
  cardinality = Tagged 4

governorMintingProperty :: Property
governorMintingProperty =
  classifiedPropertyNative gen (const []) expected classifier actual
  where
    {- Note:
    I don't think it's easily possible to randomize orefs. We can't really pass pass `Governor` type to `actual` function.
    -}
    gst = assetClassValue govAssetClass 1
    mintAmount x = mint . mconcat $ replicate x gst
    outputToGov =
      output $
        mconcat
          [ script govValidatorHash
          , withValue gst
          , withDatum govDatum
          ]
    referencedInput = input $ withRef gstUTXORef

    govDatum :: GovernorDatum
    govDatum =
      GovernorDatum
        { proposalThresholds = def
        , nextProposalId = ProposalId 0
        , proposalTimings = def
        , createProposalTimeRangeMaxWidth = def
        , maximumProposalsPerStake = 3
        }

    gen :: GovernorPolicyCases -> Gen ScriptContext
    gen c = do
      inputs <- fmap mconcat . listOf1 $ genInput @MintingBuilder
      outputs <- fmap mconcat . listOf1 $ genOutput @MintingBuilder
      toks <- choose (2, 100)

      let comp =
            case c of
              ReferenceUTXONotSpent -> outputToGov <> mintAmount 1
              IncorrectAmountOfTokenMinted -> referencedInput <> outputToGov <> mintAmount toks
              GovernorOutputNotFound -> referencedInput <> mintAmount 1
              GovernorPolicyCorrect -> referencedInput <> outputToGov <> mintAmount 1

      return . buildMinting' $ inputs <> outputs <> comp <> withMinting govSymbol

    expected :: ScriptContext -> Maybe ()
    expected sc =
      case classifier sc of
        GovernorPolicyCorrect -> Just ()
        _ -> Nothing

    opaqueToUnit :: Term s (POpaque :--> PUnit)
    opaqueToUnit = plam $ \_ -> pconstant ()

    actual :: Term s (PScriptContext :--> PUnit)
    actual = plam $ \sc -> opaqueToUnit #$ governorPolicy governor.gstOutRef # pforgetData (pconstantData ()) # sc

    classifier :: ScriptContext -> GovernorPolicyCases
    classifier sc
      | minted /= gst = IncorrectAmountOfTokenMinted
      | refInputNotExists = ReferenceUTXONotSpent
      | govOutputNotExists = GovernorOutputNotFound
      | otherwise = GovernorPolicyCorrect
      where
        txinfo = scriptContextTxInfo sc
        minted = txInfoMint txinfo
        refInputNotExists = gstUTXORef `notElem` (txInInfoOutRef <$> txInfoInputs txinfo)
        govOutputNotExists = gst `notElem` (txOutValue <$> txInfoOutputs txinfo)

props :: [TestTree]
props =
  [ testProperty "governorDatumValid" governorDatumValidProperty
  , testProperty "governorPolicy" governorMintingProperty
  ]
