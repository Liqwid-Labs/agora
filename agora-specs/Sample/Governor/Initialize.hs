{- |
Module     : Sample.Governor.Initialize
Maintainer : connor@mlabs.city
Description: Generate sample data for testing the functionalities of minting GST.

Sample and utilities for testing the functionalities of minting GST.
-}
module Sample.Governor.Initialize (
  mintGST,
  Parameters (..),
  totallyValidParameters,
  invalidDatumTimingConfigParameters,
  invalidDatumMaxTimeRangeWidthParameters,
  invalidDatumThresholdsParameters,
  withoutGovernorDatumParameters,
  witnessNotPresentedParameters,
  mintMoreThanOneGSTParameters,
  mintGSTWithNoneEmptyNameParameters,
  mkTestCase,
) where

import Agora.Bootstrap (agoraScripts)
import Agora.Governor (Governor (..), GovernorDatum (..))
import Agora.Proposal (ProposalId (..), ProposalThresholds (..))
import Agora.Proposal.Time (
  MaxTimeRangeWidth (MaxTimeRangeWidth),
  ProposalTimingConfig (ProposalTimingConfig),
 )
import Agora.Scripts (
  AgoraScripts (compiledGovernorPolicy),
  governorSTAssetClass,
  governorSTSymbol,
  governorValidatorHash,
 )
import Data.Default (Default (..))
import Plutarch.Context (
  input,
  mint,
  output,
  pubKey,
  script,
  signedWith,
  txId,
  withDatum,
  withRef,
  withValue,
 )
import PlutusLedgerApi.V1.Value (AssetClass (..))
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  TxOutRef (TxOutRef),
  ValidatorHash,
 )
import Sample.Shared (
  minAda,
 )
import Sample.Shared qualified as Shared
import Test.Specification (SpecificationTree, testPolicy)
import Test.Util (CombinableBuilder, mkMinting, pubKeyHashes, sortValue)

-- | The parameters that control the generation of the transaction.
data Parameters = Parameters
  { datumThresholdsValid :: Bool
  -- ^ Whether the 'GovernorDatum.proposalThresholds' field of the output
  --    governor datum is valid or not.
  , datumMaxTimeRangeWidthValid :: Bool
  -- ^ Whether the 'GovernorDatum.maximumProposalsPerStake'field of the
  --    output governor datum is valid or not.
  , datumTimingConfigValid :: Bool
  -- ^ Whether the 'GovernorDatum.proposalTimings'field  of the output
  --    governor datum is valid or not.
  , withGovernorDatum :: Bool
  -- ^ Whether the output GST UTxO will carry the governor datum.
  , presentWitness :: Bool
  -- ^ Whether to spend the UTxO referenced by 'Governor.gstOutRef'.
  , mintMoreThanOneStateToken :: Bool
  -- ^ More than one GST will be minted if this is set to true.
  , mintStateTokenWithName :: Bool
  -- ^ The token name of the GST won't be empty if this is set to true.
  }

--------------------------------------------------------------------------------

validGovernorOutputDatum :: GovernorDatum
validGovernorOutputDatum =
  GovernorDatum
    { proposalThresholds = def
    , nextProposalId = ProposalId 0
    , proposalTimings = def
    , createProposalTimeRangeMaxWidth = def
    , maximumProposalsPerStake = 3
    }

invalidProposalThresholds :: ProposalThresholds
invalidProposalThresholds = ProposalThresholds (-1) (-1) (-1)

invalidMaxTimeRangeWidth :: MaxTimeRangeWidth
invalidMaxTimeRangeWidth = MaxTimeRangeWidth 0

invalidProposalTimings :: ProposalTimingConfig
invalidProposalTimings = ProposalTimingConfig (-1) (-1) (-1) (-1)

witnessRef :: TxOutRef
witnessRef = TxOutRef "b0353c22b0bd6c5296a8eef160ba25d90b5dc82a9bb8bdaa6823ffc19515d6ad" 0

governor :: Governor
governor =
  Shared.governor
    { gstOutRef = witnessRef
    }

scripts :: AgoraScripts
scripts = agoraScripts Shared.deterministicTracingConfing governor

govAssetClass :: AssetClass
govAssetClass = governorSTAssetClass scripts

govValidatorHash :: ValidatorHash
govValidatorHash = governorValidatorHash scripts

govSymbol :: CurrencySymbol
govSymbol = governorSTSymbol scripts

--------------------------------------------------------------------------------

mintGST :: forall b. CombinableBuilder b => Parameters -> b
mintGST ps = builder
  where
    gstAC =
      if getField @"mintStateTokenWithName" ps
        then AssetClass (govSymbol, "12345")
        else govAssetClass
    gstCount =
      if getField @"mintMoreThanOneStateToken" ps
        then 10
        else 1
    gst = Value.assetClassValue gstAC gstCount

    ---

    governorOutputDatum =
      let th =
            if getField @"datumThresholdsValid" ps
              then def
              else invalidProposalThresholds
          trw =
            if getField @"datumMaxTimeRangeWidthValid" ps
              then def
              else invalidMaxTimeRangeWidth
          ptc =
            if getField @"datumTimingConfigValid" ps
              then def
              else invalidProposalTimings
       in validGovernorOutputDatum
            { proposalThresholds = th
            , proposalTimings = ptc
            , createProposalTimeRangeMaxWidth = trw
            }

    governorValue = sortValue $ gst <> minAda

    ---

    witnessValue = minAda
    witnessPubKey = head pubKeyHashes

    ---

    witnessBuilder =
      if getField @"presentWitness" ps
        then
          mconcat
            [ input $
                mconcat
                  [ pubKey witnessPubKey
                  , withValue witnessValue
                  , withRef witnessRef
                  ]
            , output $
                mconcat
                  [ pubKey witnessPubKey
                  , withValue witnessValue
                  ]
            ]
        else mempty

    ---

    govBuilder =
      let datum =
            if getField @"withGovernorDatum" ps
              then withDatum governorOutputDatum
              else mempty
       in output $
            mconcat
              [ script govValidatorHash
              , withValue governorValue
              , datum
              ]
    --
    builder =
      mconcat
        [ txId "986b756ffb1c9839fc8d0b22a308ac91d5b5d0ebbfa683a47588c8a5cf70b5af"
        , signedWith (pubKeyHashes !! 1)
        , mint gst
        , govBuilder
        , witnessBuilder
        ]

--------------------------------------------------------------------------------

totallyValidParameters :: Parameters
totallyValidParameters =
  Parameters
    { datumThresholdsValid = True
    , datumMaxTimeRangeWidthValid = True
    , datumTimingConfigValid = True
    , withGovernorDatum = True
    , presentWitness = True
    , mintMoreThanOneStateToken = False
    , mintStateTokenWithName = False
    }

invalidDatumThresholdsParameters :: Parameters
invalidDatumThresholdsParameters =
  totallyValidParameters
    { datumThresholdsValid = False
    }

invalidDatumMaxTimeRangeWidthParameters :: Parameters
invalidDatumMaxTimeRangeWidthParameters =
  totallyValidParameters
    { datumMaxTimeRangeWidthValid = False
    }

invalidDatumTimingConfigParameters :: Parameters
invalidDatumTimingConfigParameters =
  totallyValidParameters
    { datumTimingConfigValid = False
    }

withoutGovernorDatumParameters :: Parameters
withoutGovernorDatumParameters =
  totallyValidParameters
    { withGovernorDatum = False
    }

witnessNotPresentedParameters :: Parameters
witnessNotPresentedParameters =
  totallyValidParameters
    { presentWitness = False
    }

mintMoreThanOneGSTParameters :: Parameters
mintMoreThanOneGSTParameters =
  totallyValidParameters
    { mintMoreThanOneStateToken = True
    }

mintGSTWithNoneEmptyNameParameters :: Parameters
mintGSTWithNoneEmptyNameParameters =
  totallyValidParameters
    { mintStateTokenWithName = True
    }

--------------------------------------------------------------------------------

{- | Create a test tree that runs the governor policy to test the initialization
      of the governor.
-}
mkTestCase :: String -> Parameters -> Bool -> SpecificationTree
mkTestCase name ps valid =
  testPolicy
    valid
    name
    (getField @"compiledGovernorPolicy" scripts)
    ()
    (mkMinting mintGST ps govSymbol)
