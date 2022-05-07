module Sample.Effect.GovernorMutation (
  validContext,
  effectValidator,
  effectValidatorAddress,
  effectValidatorHash,
  atAssetClass,
) where

import Agora.Effect.GovernorMutation (
  MutateGovernorDatum (..),
  mutateGovernorValidator,
 )
import Agora.Governor (GovernorDatum (..))
import Agora.Proposal (ProposalId (..))
import Plutarch.Api.V1 (mkValidator, validatorHash)
import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Api (
  Address,
  Datum (..),
  ScriptContext (..),
  ScriptPurpose (Spending),
  ToData (..),
  TokenName (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (TxOutRef),
  Validator,
  ValidatorHash (..),
 )
import Plutus.V1.Ledger.Api qualified as Interval
import Plutus.V1.Ledger.Value (AssetClass, assetClass)
import Plutus.V1.Ledger.Value qualified as Value
import Sample.Shared
import Test.Util (datumPair, toDatumHash)

effectValidator :: Validator
effectValidator = mkValidator $ mutateGovernorValidator governor

effectValidatorHash :: ValidatorHash
effectValidatorHash = validatorHash effectValidator

effectValidatorAddress :: Address
effectValidatorAddress = scriptHashAddress effectValidatorHash

atAssetClass :: AssetClass
atAssetClass = assetClass authorityTokenSymbol tokenName
  where
    -- TODO: use 'validatorHashToTokenName'
    ValidatorHash bs = effectValidatorHash
    tokenName = TokenName bs

validContext :: ScriptContext
validContext =
  let gst = Value.assetClassValue govAssetClass 1
      at = Value.assetClassValue atAssetClass 1

      burnt = Value.assetClassValue atAssetClass (-1)

      --

      governorInputRef :: TxOutRef
      governorInputRef = TxOutRef "614481d2159bfb72350222d61fce17e548e0fc00e5a1f841ff1837c431346ce7" 1

      --

      governorInputDatum' :: GovernorDatum
      governorInputDatum' =
        GovernorDatum
          { proposalThresholds = defaultProposalThresholds
          , nextProposalId = ProposalId 0
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
      effectInputDatum' =
        MutateGovernorDatum
          { governorRef = governorInputRef
          , newDatum =
              governorInputDatum'
                { nextProposalId = ProposalId 42
                }
          }
      effectInputDatum :: Datum
      effectInputDatum = Datum $ toBuiltinData effectInputDatum'
      effectInput :: TxOut
      effectInput =
        TxOut
          { txOutAddress = effectValidatorAddress
          , txOutValue = at
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
          { txOutAddress = effectValidatorAddress
          , txOutValue = withMinAda gst
          , txOutDatumHash = Just $ toDatumHash governorOutputDatum
          }

      --

      ownInputRef :: TxOutRef
      ownInputRef = TxOutRef "c31164dc11835de7eb6187f67d0e1a19c1dfc0786a456923eef5043189cdb578" 1
   in ScriptContext
        { scriptContextPurpose = Spending ownInputRef
        , scriptContextTxInfo =
            TxInfo
              { txInfoInputs =
                  [ TxInInfo ownInputRef effectInput
                  , TxInInfo governorInputRef governorInput
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
        }
