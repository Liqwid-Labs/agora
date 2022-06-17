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
import Data.Default.Class (Default (def))
import Data.Tagged (Tagged (..))
import Plutarch.Api.V1 (mkValidator, validatorHash)
import PlutusLedgerApi.V1 (
  Address,
  Datum (..),
  ToData (..),
  TokenName (..),
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
  authorityTokenSymbol,
  govAssetClass,
  govValidatorAddress,
  governor,
  minAda,
  signer,
 )
import Test.Util (datumPair, toDatumHash)

-- | The effect validator instance.
effectValidator :: Validator
effectValidator = mkValidator $ mutateGovernorValidator governor

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
    -- TODO: use 'validatorHashToTokenName'
    ValidatorHash bs = effectValidatorHash
    tokenName = TokenName bs

-- | The mock reference of the governor state UTXO.
govRef :: TxOutRef
govRef = TxOutRef "614481d2159bfb72350222d61fce17e548e0fc00e5a1f841ff1837c431346ce7" 1

-- | The mock reference of the effect UTXO.
effectRef :: TxOutRef
effectRef = TxOutRef "c31164dc11835de7eb6187f67d0e1a19c1dfc0786a456923eef5043189cdb578" 1

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
        , txInfoId = "4dae3806cc69615b721d52ed09b758f43f25a8f39b7934d6b28514caf71f5f7b"
        }

validNewGovernorDatum :: GovernorDatum
validNewGovernorDatum =
  GovernorDatum
    { proposalThresholds = def
    , nextProposalId = ProposalId 42
    , proposalTimings = def
    , createProposalTimeRangeMaxWidth = def
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
    }
