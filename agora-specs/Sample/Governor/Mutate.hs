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
import Agora.Governor.Scripts (governorValidator)
import Agora.Proposal (ProposalId (ProposalId), ProposalThresholds (..))
import Agora.Utils (validatorHashToTokenName)
import Data.Default (def)
import Plutarch.Api.V1 (PValidator, mkValidator, validatorHash)
import Plutarch.Context (
  input,
  mint,
  output,
  pubKey,
  script,
  withDatum,
  withOutRef,
  withValue,
 )
import PlutusLedgerApi.V1 (
  Data,
  TxOutRef (TxOutRef),
  ValidatorHash,
  Value,
  toData,
 )
import PlutusLedgerApi.V1.Value qualified as Value
import Sample.Shared (
  authorityTokenSymbol,
  govAssetClass,
  govValidatorHash,
  governor,
  minAda,
 )
import Test.Specification (SpecificationTree, testValidator)
import Test.Util (CombinableBuilder, mkSpending, pubKeyHashes, sortValue, validatorHashes, withOptional)

--------------------------------------------------------------------------------

data GovernorOutputDatumValidity
  = DatumValid
  | ValueInvalid
  | WrongType
  | NoDatum
  deriving stock (Bounded, Enum)

data GATValidity
  = GATValid
  | WrongTag
  | NoGAT
  deriving stock (Bounded, Enum)

data GovernorParameters = GovernorParameters
  { governorOutputDatumValidity :: GovernorOutputDatumValidity
  , stealGST :: Bool
  }

data MockEffectParameters = MockEffectParameters
  { gatValidity :: GATValidity
  , burnGAT :: Bool
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
        withOptional withDatum $
          mkGovernorOutputDatum ps.governorOutputDatumValidity
   in mconcat
        [ input $
            script govValidatorHash
              . withDatum governorInputDatum
              . withValue value
              . withOutRef governorRef
        , output $
            gstOutput
              . withGSTDatum
              . withValue value
        ]

--------------------------------------------------------------------------------

mockEffectValidator :: ClosedTerm PValidator
mockEffectValidator = noOpValidator authorityTokenSymbol

mockEffectValidatorHash :: ValidatorHash
mockEffectValidatorHash = validatorHash $ mkValidator mockEffectValidator

mkGATValue :: GATValidity -> Integer -> Value
mkGATValue NoGAT _ = mempty
mkGATValue v q =
  let gatOwner = case v of
        GATValid -> mockEffectValidatorHash
        WrongTag -> head validatorHashes
   in Value.singleton
        authorityTokenSymbol
        (validatorHashToTokenName gatOwner)
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
            script mockEffectValidatorHash
              . withValue inputValue
        , output $
            script mockEffectValidatorHash
              . withValue outputValue
        ]

--------------------------------------------------------------------------------

mutate :: forall b. CombinableBuilder b => ParameterBundle -> b
mutate pb =
  mconcat
    [ mkGovernorBuilder pb.governorParameters
    , mkMockEffectBuilder pb.mockEffectParameters
    ]

--------------------------------------------------------------------------------

mkTestCase :: String -> ParameterBundle -> Validity -> SpecificationTree
mkTestCase name pb (Validity forGov) =
  testValidator
    forGov
    name
    (governorValidator governor)
    governorInputDatum
    governorRedeemer
    (mkSpending mutate pb governorRef)

--------------------------------------------------------------------------------

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
