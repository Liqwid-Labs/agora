module Sample.Effect.GovernorMutation (
  mkEffectTxInfo,
  effectValidator,
  effectValidatorAddress,
  effectValidatorHash,
  atAssetClass,
  govRef,
  effectRef,
  invalidNewGovernorDatum,
  validNewGovernorDatum,
  mkEffectDatum,
) where

import Agora.Effect.GovernorMutation (
  MutateGovernorDatum (..),
  mutateGovernorValidator,
 )
import Agora.Governor (GovernorDatum (..))
import Agora.Proposal (ProposalId (..), ProposalThresholds (..))
import Agora.Utils (validatorHashToTokenName)
import Data.Default.Class (Default (def))
import Data.Tagged (Tagged (..))
import Plutarch.Api.V1 (mkValidator, validatorHash)
import PlutusLedgerApi.V1 (
  Address,
  Datum (..),
  ToData (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (TxOutRef),
  Validator,
  ValidatorHash (..),
 )
import PlutusLedgerApi.V1 qualified as Interval (always)
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (AssetClass, assetClass)
import PlutusLedgerApi.V1.Value qualified as Value (
  assetClassValue,
  singleton,
 )
import Sample.Shared (
  agoraScripts,
  authorityTokenSymbol,
  deterministicTracingConfing,
  govAssetClass,
  govValidatorAddress,
  minAda,
  signer,
 )
import Test.Util (datumPair, toDatumHash)

-- | The effect validator instance.
effectValidator :: Validator
effectValidator = mkValidator deterministicTracingConfing $ mutateGovernorValidator agoraScripts

-- | The hash of the validator instance.
effectValidatorHash :: ValidatorHash
effectValidatorHash = validatorHash effectValidator

-- | The address of the validator.
effectValidatorAddress :: Address
effectValidatorAddress = scriptHashAddress effectValidatorHash

-- | The assetclass of the authority token.
atAssetClass :: AssetClass
atAssetClass = assetClass authorityTokenSymbol tokenName
  where
    tokenName = validatorHashToTokenName effectValidatorHash

-- | The mock reference of the governor state UTXO.
govRef :: TxOutRef
govRef = TxOutRef "1475e1ee22330dfc55430980e5a6b100ec9d9249bb4b462256a79559" 1

-- | The mock reference of the effect UTXO.
effectRef :: TxOutRef
effectRef = TxOutRef "a302d327d8e5553d50b9d017475369753f723d7e999ac1b68da8ad52" 1

-- | The input effect datum in 'mkEffectTransaction'.
mkEffectDatum :: GovernorDatum -> MutateGovernorDatum
mkEffectDatum newGovDatum =
  MutateGovernorDatum
    { governorRef = govRef
    , newDatum = newGovDatum
    }

{- | Given the new governor state, create an effect to update the governor's state.

    Note that the transaction is valid only if the given new datum is valid.
-}
mkEffectTxInfo :: GovernorDatum -> TxInfo
mkEffectTxInfo newGovDatum =
  let gst = Value.assetClassValue govAssetClass 1
      at = Value.assetClassValue atAssetClass 1

      -- One authority token is burnt in the process.
      burnt = Value.assetClassValue atAssetClass (-1)

      --

      governorInputDatum' :: GovernorDatum
      governorInputDatum' =
        GovernorDatum
          { proposalThresholds = def
          , nextProposalId = ProposalId 0
          , proposalTimings = def
          , createProposalTimeRangeMaxWidth = def
          , maximumProposalsPerStake = 3
          }
      governorInputDatum :: Datum
      governorInputDatum = Datum $ toBuiltinData governorInputDatum'
      governorInput :: TxOut
      governorInput =
        TxOut
          { txOutAddress = govValidatorAddress
          , txOutValue = gst
          , txOutDatumHash = Just $ toDatumHash governorInputDatum
          }

      --

      -- The effect should update 'nextProposalId'
      effectInputDatum' :: MutateGovernorDatum
      effectInputDatum' = mkEffectDatum newGovDatum
      effectInputDatum :: Datum
      effectInputDatum = Datum $ toBuiltinData effectInputDatum'
      effectInput :: TxOut
      effectInput =
        TxOut
          { txOutAddress = effectValidatorAddress
          , txOutValue = at -- The effect carry an authotity token.
          , txOutDatumHash = Just $ toDatumHash effectInputDatum
          }

      --

      governorOutputDatum' :: GovernorDatum
      governorOutputDatum' = effectInputDatum'.newDatum
      governorOutputDatum :: Datum
      governorOutputDatum = Datum $ toBuiltinData governorOutputDatum'
      governorOutput :: TxOut
      governorOutput =
        TxOut
          { txOutAddress = govValidatorAddress
          , txOutValue = mconcat [gst, minAda]
          , txOutDatumHash = Just $ toDatumHash governorOutputDatum
          }
   in TxInfo
        { txInfoInputs =
            [ TxInInfo effectRef effectInput
            , TxInInfo govRef governorInput
            ]
        , txInfoOutputs = [governorOutput]
        , txInfoFee = Value.singleton "" "" 2
        , txInfoMint = burnt
        , txInfoDCert = []
        , txInfoWdrl = []
        , txInfoValidRange = Interval.always
        , txInfoSignatories = [signer]
        , txInfoData = datumPair <$> [governorInputDatum, governorOutputDatum, effectInputDatum]
        , txInfoId = "74c75505691e7baa981fa80e50b9b7e88dbe1eda67d4f062d89d203b"
        }

validNewGovernorDatum :: GovernorDatum
validNewGovernorDatum =
  GovernorDatum
    { proposalThresholds = def
    , nextProposalId = ProposalId 42
    , proposalTimings = def
    , createProposalTimeRangeMaxWidth = def
    , maximumProposalsPerStake = 3
    }

invalidNewGovernorDatum :: GovernorDatum
invalidNewGovernorDatum =
  GovernorDatum
    { proposalThresholds =
        def
          { vote = Tagged (-1)
          }
    , nextProposalId = ProposalId 42
    , proposalTimings = def
    , createProposalTimeRangeMaxWidth = def
    , maximumProposalsPerStake = 3
    }
