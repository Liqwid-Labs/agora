{- |
Module     : Property.Governor
Maintainer : seungheon.ooh@gmail.com
Description: Property tests for 'Governor' related functions.

Property model and tests for 'Governor' related functions
-}
module Property.Governor (props) where

import Test.Tasty (TestTree)
import Test.QuickCheck (Property, Gen, Arbitrary (arbitrary), arbitraryBoundedEnum, chooseInteger)
import Test.Tasty.QuickCheck (testProperty)
import Data.Universe (Universe)
import Data.Universe.Class (Finite)
import Plutarch.Test.QuickCheck (Equality (OnPEq), Partiality (ByComplete), haskEquiv, TestableTerm (TestableTerm), pconstantT)
import Agora.Governor (pisGovernorDatumValid, GovernorDatum(GovernorDatum), PGovernorDatum)
import Agora.Proposal (ProposalThresholds(ProposalThresholds), ProposalId (ProposalId))
import Agora.Proposal.Time (ProposalTimingConfig(ProposalTimingConfig), MaxTimeRangeWidth (MaxTimeRangeWidth))
import Data.Tagged (Tagged(Tagged))
import Generics.SOP.NP (NP(Nil, (:*)))
import Optics (view)


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

{- | Property that checks `pisGovernorDatumValid` behaves as intended by comparing it
  to a simple haskell implementation.
-}
governorDatumValidProperty :: Property
governorDatumValidProperty =
  haskEquiv @'OnPEq @'ByComplete
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

          let timing = ProposalTimingConfig 0 0 0 0
          return $ GovernorDatum thres (ProposalId 0) timing (MaxTimeRangeWidth 1) 3
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

    -- | This is a model Haskell implementation of `pisGovernorDatumValid`.
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

---

-- data GovernorPolicyCases
--   = ReferenceUTXONotSpent
--   | IncorrectAmountOfTokenMinted
--   | GovernorOutputNotFound
--   | GovernorPolicyCorrect
--   deriving stock (Eq, Show)

-- instance Universe GovernorPolicyCases where
--   universe =
--     [ ReferenceUTXONotSpent
--     , IncorrectAmountOfTokenMinted
--     , GovernorOutputNotFound
--     , GovernorPolicyCorrect
--     ]

-- instance Finite GovernorPolicyCases where
--   universeF = universe
--   cardinality = Tagged 4

-- governorMintingProperty :: Property
-- governorMintingProperty =
--   classifiedPropertyNative gen (const []) expected classifier actual
--   where
--     {- Note:
--     I don't think it's easily possible to randomize orefs. We can't really pass pass `Governor` type to `actual` function.
--     -}
--     gst = assetClassValue governorAssetClass 1
--     mintAmount x = mint . mconcat $ replicate x gst
--     outputToGov =
--       output $
--         mconcat
--           [ script governorValidatorHash
--           , withValue gst
--           , withDatum govDatum
--           ]
--     referencedInput = input $ withRef gstUTXORef

--     govDatum :: GovernorDatum
--     govDatum =
--       GovernorDatum
--         { proposalThresholds = def
--         , nextProposalId = ProposalId 0
--         , proposalTimings = def
--         , createProposalTimeRangeMaxWidth = def
--         , maximumProposalsPerStake = 3
--         }

--     gen :: GovernorPolicyCases -> Gen ScriptContext
--     gen c = do
--       inputs <- fmap mconcat . listOf1 $ genInput @MintingBuilder
--       outputs <- fmap mconcat . listOf1 $ genOutput @MintingBuilder
--       toks <- choose (2, 100)

--       let comp =
--             case c of
--               ReferenceUTXONotSpent -> outputToGov <> mintAmount 1
--               IncorrectAmountOfTokenMinted -> referencedInput <> outputToGov <> mintAmount toks
--               GovernorOutputNotFound -> referencedInput <> mintAmount 1
--               GovernorPolicyCorrect -> referencedInput <> outputToGov <> mintAmount 1

--       return . buildMinting' $ inputs <> outputs <> comp <> withMinting governorSymbol

--     expected :: ScriptContext -> Maybe ()
--     expected sc =
--       case classifier sc of
--         GovernorPolicyCorrect -> Just ()
--         _ -> Nothing

--     opaqueToUnit :: Term s (POpaque :--> PUnit)
--     opaqueToUnit = plam $ \_ -> pconstant ()

--     actual :: Term s (PScriptContext :--> PUnit)
--     actual = plam $ \sc -> opaqueToUnit #$ governorPolicy # pconstant governor.gstOutRef # pforgetData (pconstantData ()) # sc

--     classifier :: ScriptContext -> GovernorPolicyCases
--     classifier sc
--       | minted /= gst = IncorrectAmountOfTokenMinted
--       | refInputNotExists = ReferenceUTXONotSpent
--       | govOutputNotExists = GovernorOutputNotFound
--       | otherwise = GovernorPolicyCorrect
--       where
--         txinfo = scriptContextTxInfo sc
--         minted = txInfoMint txinfo
--         refInputNotExists = gstUTXORef `notElem` (txInInfoOutRef <$> txInfoInputs txinfo)
--         govOutputNotExists = gst `notElem` (txOutValue <$> txInfoOutputs txinfo)

props :: [TestTree]
props =
  [ testProperty "governorDatumValid" governorDatumValidProperty
  -- , testProperty "governorPolicy" governorMintingProperty
  ]

-- props :: [TestTree]
-- props = []
