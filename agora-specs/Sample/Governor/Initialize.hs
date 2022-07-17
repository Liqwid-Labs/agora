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

import Agora.Governor (Governor (..), GovernorDatum (..))
import Agora.Governor.Scripts (
  governorPolicy,
  governorSTAssetClassFromGovernor,
  governorValidatorHash,
 )
import Agora.Proposal (ProposalId (..), ProposalThresholds (..))
import Agora.Proposal.Time (MaxTimeRangeWidth (MaxTimeRangeWidth), ProposalTimingConfig (ProposalTimingConfig))
import Data.Default (Default (..))
import Plutarch.Api.V1 (mintingPolicySymbol, mkMintingPolicy)
import Plutarch.Context (
  BaseBuilder,
  buildTxInfoUnsafe,
  input,
  mint,
  output,
  pubKey,
  script,
  signedWith,
  txId,
  withDatum,
  withOutRef,
  withValue,
 )
import PlutusLedgerApi.V1 (
  CurrencySymbol,
  MintingPolicy,
  ScriptContext (..),
  ScriptPurpose (Minting),
  TxInfo,
  TxOutRef (TxOutRef),
  ValidatorHash,
 )
import PlutusLedgerApi.V1.Value (AssetClass (..))
import PlutusLedgerApi.V1.Value qualified as Value
import Sample.Shared (
  minAda,
 )
import Sample.Shared qualified as Shared
import Test.Specification (SpecificationTree, testPolicy)
import Test.Util (pubKeyHashes, sortValue)

data Parameters = Parameters
  { datumThresholdsValid :: Bool
  , datumMaxTimeRangeWidthValid :: Bool
  , datumTimingConfigValid :: Bool
  , withGovernorDatum :: Bool
  , presentWitness :: Bool
  , mintMoreThanOneStateToken :: Bool
  , mintStateTokenWithName :: Bool
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

govAssetClass :: AssetClass
govAssetClass = governorSTAssetClassFromGovernor governor

govValidatorHash :: ValidatorHash
govValidatorHash = governorValidatorHash governor

govPolicy :: MintingPolicy
govPolicy = mkMintingPolicy (governorPolicy governor)

govSymbol :: CurrencySymbol
govSymbol = mintingPolicySymbol govPolicy

--------------------------------------------------------------------------------

mintGST :: Parameters -> TxInfo
mintGST ps = buildTxInfoUnsafe builder
  where
    gstAC =
      if ps.mintStateTokenWithName
        then AssetClass (govSymbol, "12345")
        else govAssetClass
    gstCount =
      if ps.mintMoreThanOneStateToken
        then 10
        else 1
    gst = Value.assetClassValue gstAC gstCount

    ---

    governorOutputDatum =
      let th = if ps.datumThresholdsValid then def else invalidProposalThresholds
          trw = if ps.datumMaxTimeRangeWidthValid then def else invalidMaxTimeRangeWidth
          ptc = if ps.datumTimingConfigValid then def else invalidProposalTimings
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

    witnessBuilder :: BaseBuilder
    witnessBuilder =
      if ps.presentWitness
        then
          mconcat
            [ input $
                pubKey witnessPubKey
                  . withValue witnessValue
                  . withOutRef witnessRef
            , output $
                pubKey witnessPubKey
                  . withValue witnessValue
            ]
        else mempty

    ---

    govBuilder :: BaseBuilder
    govBuilder =
      let datum =
            if ps.withGovernorDatum
              then withDatum governorOutputDatum
              else id
       in output $
            script govValidatorHash
              . withValue governorValue
              . datum
    --

    builder :: BaseBuilder
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

mkTestCase :: String -> Parameters -> Bool -> SpecificationTree
mkTestCase
  name
  ps
  valid = policyTest
    where
      txInfo = mintGST ps

      policyTest =
        testPolicy
          valid
          name
          (governorPolicy governor)
          ()
          (ScriptContext txInfo (Minting govSymbol))
