module Sample.Governor.Mutate (
  -- * Testing Utilities
  GovernorOutputDatumValidity (..),
  GATValidity (..),
  GovernorParameters (..),
  MockEffectParameters (..),
  ParameterBundle (..),

  -- * Testing Utilities
  Validity (..),
  mutate,
  mkTestCase,

  -- * Parameters Bundles
  totallyValidBundle,
  invalidBundles,
) where

import Agora.Effect.NoOp (noOpValidator)
import Agora.Governor (GovernorDatum (..), GovernorRedeemer (MutateGovernor))
import Agora.Proposal (ProposalId (ProposalId), ProposalThresholds (..))
import Agora.Scripts (AgoraScripts (..))
import Agora.Utils (scriptHashToTokenName)
import Data.Default (def)
import Plutarch.Api.V2 (PMintingPolicy, PValidator, mintingPolicySymbol, mkMintingPolicy, mkValidator, validatorHash)
import Plutarch.Context (
  input,
  mint,
  output,
  pubKey,
  script,
  withDatum,
  withRef,
  withValue,
 )
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 (
  CurrencySymbol (CurrencySymbol),
  Data,
  ScriptHash (ScriptHash),
  TxOutRef (TxOutRef),
  ValidatorHash,
  Value,
  toData,
 )
import Sample.Shared (
  agoraScripts,
  authorityTokenSymbol,
  govAssetClass,
  govValidatorHash,
  minAda,
 )
import Test.Specification (SpecificationTree, testValidator)
import Test.Util (
  CombinableBuilder,
  mkSpending,
  pubKeyHashes,
  sortValue,
 )

--------------------------------------------------------------------------------

-- | Represent the validity property of the governor output datum.
data GovernorOutputDatumValidity
  = DatumValid
  | ValueInvalid
  | WrongType
  | NoDatum
  deriving stock (Bounded, Enum)

-- | Represent the validity property of the authority token UTxO.
data GATValidity
  = GATValid
  | WrongTag
  | NoGAT
  deriving stock (Bounded, Enum)

data GovernorParameters = GovernorParameters
  { governorOutputDatumValidity :: GovernorOutputDatumValidity
  , stealGST :: Bool
  -- ^ Send the GST to somewhere else other than the govenor validator.
  }

data MockEffectParameters = MockEffectParameters
  { gatValidity :: GATValidity
  , burnGAT :: Bool
  -- ^ Whether to burn the GAT in the transaction or not.
  }

data ParameterBundle = ParameterBundle
  { governorParameters :: GovernorParameters
  , mockEffectParameters :: MockEffectParameters
  }

newtype Validity = Validity {forGovernorValidator :: Bool}

--------------------------------------------------------------------------------

governorInputDatum :: GovernorDatum
governorInputDatum =
  GovernorDatum
    { proposalThresholds = def
    , nextProposalId = ProposalId 0
    , proposalTimings = def
    , createProposalTimeRangeMaxWidth = def
    , maximumProposalsPerStake = 3
    }

mkGovernorOutputDatum ::
  GovernorOutputDatumValidity ->
  Maybe Data
mkGovernorOutputDatum DatumValid =
  Just $
    toData $
      governorInputDatum
        { maximumProposalsPerStake = 4
        }
mkGovernorOutputDatum ValueInvalid =
  let invalidProposalThresholds =
        ProposalThresholds
          { execute = -1
          , create = -1
          , toVoting = -1
          , vote = -1
          }
   in Just $
        toData $
          governorInputDatum
            { proposalThresholds =
                invalidProposalThresholds
            }
mkGovernorOutputDatum WrongType = Just $ toData ()
mkGovernorOutputDatum NoDatum = Nothing

governorRef :: TxOutRef
governorRef =
  TxOutRef
    "6cce6dfbb697f9e2c4fe9786bb576eb7bd6cbcf7801a4ba13d596006c2d5b957"
    1

governorRedeemer :: GovernorRedeemer
governorRedeemer = MutateGovernor

mkGovernorBuilder :: forall b. CombinableBuilder b => GovernorParameters -> b
mkGovernorBuilder ps =
  let gst = Value.assetClassValue govAssetClass 1
      value = sortValue $ gst <> minAda
      gstOutput =
        if ps.stealGST
          then pubKey $ head pubKeyHashes
          else script govValidatorHash
      withGSTDatum =
        maybe mempty withDatum $
          mkGovernorOutputDatum ps.governorOutputDatumValidity
   in mconcat
        [ input $
            mconcat
              [ script govValidatorHash
              , withDatum governorInputDatum
              , withValue value
              , withRef governorRef
              ]
        , output $
            mconcat
              [ gstOutput
              , withGSTDatum
              , withValue value
              ]
        ]

--------------------------------------------------------------------------------

mockEffectValidator :: ClosedTerm PValidator
mockEffectValidator = noOpValidator authorityTokenSymbol

mockEffectValidatorHash :: ValidatorHash
mockEffectValidatorHash = validatorHash $ mkValidator def mockEffectValidator

mockAuthScript :: ClosedTerm PMintingPolicy
mockAuthScript = plam $ \_ _ -> popaque $ pcon PUnit

mockAuthScriptHash :: ScriptHash
mockAuthScriptHash =
  let CurrencySymbol h = mintingPolicySymbol $ mkMintingPolicy def mockAuthScript
   in ScriptHash h

mkGATValue :: GATValidity -> Integer -> Value
mkGATValue NoGAT _ = mempty
mkGATValue v q =
  let authScript = case v of
        GATValid -> mockAuthScriptHash
        WrongTag -> ""
   in Value.singleton
        authorityTokenSymbol
        (scriptHashToTokenName authScript)
        q

mkMockEffectBuilder :: forall b. CombinableBuilder b => MockEffectParameters -> b
mkMockEffectBuilder ps =
  let mkGATValue' = mkGATValue ps.gatValidity
      inputValue = mkGATValue' 1
      outputValue = inputValue <> burnt
      burnt =
        if ps.burnGAT
          then mkGATValue' (-1)
          else mempty
   in mconcat
        [ mint burnt
        , input $
            mconcat
              [ script mockEffectValidatorHash
              , withValue inputValue
              ]
        , output $
            mconcat
              [ script mockEffectValidatorHash
              , withValue outputValue
              ]
        ]

--------------------------------------------------------------------------------

mutate :: forall b. CombinableBuilder b => ParameterBundle -> b
mutate pb =
  mconcat
    [ mkGovernorBuilder pb.governorParameters
    , mkMockEffectBuilder pb.mockEffectParameters
    ]

--------------------------------------------------------------------------------

-- | Run the governor to test the mutation functionality.
mkTestCase :: String -> ParameterBundle -> Validity -> SpecificationTree
mkTestCase name pb (Validity forGov) =
  testValidator
    forGov
    name
    agoraScripts.compiledGovernorValidator
    governorInputDatum
    governorRedeemer
    (mkSpending mutate pb governorRef)

--------------------------------------------------------------------------------

-- | The only one valid combination of all the parameters.
totallyValidBundle :: ParameterBundle
totallyValidBundle =
  ParameterBundle
    { governorParameters =
        GovernorParameters
          { governorOutputDatumValidity = DatumValid
          , stealGST = False
          }
    , mockEffectParameters =
        MockEffectParameters
          { gatValidity = GATValid
          , burnGAT = True
          }
    }

--------------------------------------------------------------------------------

{- | All the invalid combination of the parameters.
   TODO: use 'Gen'?
-}
invalidBundles :: [ParameterBundle]
invalidBundles = do
  gdv <- enumFrom ValueInvalid
  sg <- [True, False]
  gtv <- enumFrom WrongTag
  bgt <- [True, False]

  pure $
    ParameterBundle
      { governorParameters =
          GovernorParameters
            { governorOutputDatumValidity = gdv
            , stealGST = sg
            }
      , mockEffectParameters =
          MockEffectParameters
            { gatValidity = gtv
            , burnGAT = bgt
            }
      }
