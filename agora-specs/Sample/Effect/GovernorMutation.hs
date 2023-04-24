module Sample.Effect.GovernorMutation (
  mkEffectTxInfo,
  effectValidator,
  effectValidatorAddress,
  effectScriptHash,
  atAssetClass,
  govRef,
  effectRef,
  invalidNewGovernorDatum,
  validNewGovernorDatum,
  mkEffectDatum,
) where

import Agora.Effect.GovernorMutation (
  MutateGovernorDatum (..),
 )
import Agora.Governor (GovernorDatum (..), GovernorRedeemer (MutateGovernor))
import Agora.Proposal (ProposalId (..), ProposalThresholds (..))
import Agora.SafeMoney (AuthorityTokenTag)
import Data.Default.Class (Default (def))
import Data.Map ((!))
import Data.Tagged (Tagged (..))
import Plutarch.Api.V2 (scriptHash)
import Plutarch.Extra.AssetClass (AssetClass (AssetClass), assetClassValue)
import Plutarch.Extra.ScriptContext (scriptHashToTokenName)
import Plutarch.Script (Script)
import PlutusLedgerApi.V1 qualified as Interval (always)
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value qualified as Value (
  singleton,
 )
import PlutusLedgerApi.V2 (
  Address,
  Datum (..),
  OutputDatum (OutputDatumHash),
  ScriptHash,
  ScriptPurpose (Spending),
  ToData (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (TxOutRef),
 )
import PlutusTx.AssocMap qualified as AssocMap
import Sample.Shared (
  agoraScripts,
  authorityTokenSymbol,
  governorAssetClass,
  governorValidatorAddress,
  minAda,
  mkRedeemer,
  signer,
 )
import Test.Util (datumPair, toDatumHash)

-- | The effect validator instance.
effectValidator :: Script
effectValidator = agoraScripts ! "agora:mutateGovernorValidator"

-- | The hash of the validator instance.
effectScriptHash :: ScriptHash
effectScriptHash = scriptHash effectValidator

-- | The address of the validator.
effectValidatorAddress :: Address
effectValidatorAddress = scriptHashAddress effectScriptHash

-- | The assetclass of the authority token.
atAssetClass :: Tagged AuthorityTokenTag AssetClass
atAssetClass = Tagged $ AssetClass authorityTokenSymbol tokenName
  where
    tokenName = scriptHashToTokenName effectScriptHash

-- | The mock reference of the governor state UTXO.
govRef :: TxOutRef
govRef =
  TxOutRef
    "d63fe09e6ac6e55dea82291149085d0a9b901df65087b83965188ee92fb25aef"
    1

-- | The mock reference of the effect UTXO.
effectRef :: TxOutRef
effectRef =
  TxOutRef
    "3ca6864670aae61a9f3e63064284cec00bd983d77cf4e1ab1e26bef34cafb0a9"
    1

-- | The input effect datum in 'mkEffectTransaction'.
mkEffectDatum :: GovernorDatum -> GovernorDatum -> MutateGovernorDatum
mkEffectDatum oldGovDatum newGovDatum =
  MutateGovernorDatum
    { oldDatum = oldGovDatum
    , newDatum = newGovDatum
    }

{- | Given the new governor state, create an effect to update the governor's state.

    Note that the transaction is valid only if the given new datum is valid.
-}
mkEffectTxInfo :: GovernorDatum -> TxInfo
mkEffectTxInfo newGovDatum =
  let gst = assetClassValue governorAssetClass 1
      at = assetClassValue atAssetClass 1

      -- One authority token is burnt in the process.
      burnt = assetClassValue atAssetClass (-1)

      --

      governorInputDatum' :: GovernorDatum
      governorInputDatum' =
        GovernorDatum
          { proposalThresholds = def
          , nextProposalId = ProposalId 0
          , proposalTimings = def
          , createProposalTimeRangeMaxWidth = def
          , maximumCreatedProposalsPerStake = 3
          }
      governorInputDatum :: Datum
      governorInputDatum = Datum $ toBuiltinData governorInputDatum'
      governorInput :: TxOut
      governorInput =
        TxOut
          { txOutAddress = governorValidatorAddress
          , txOutValue = gst
          , txOutDatum = OutputDatumHash $ toDatumHash governorInputDatum
          , txOutReferenceScript = Nothing
          }

      --

      -- The effect should update 'nextProposalId'
      effectInputDatum' :: MutateGovernorDatum
      effectInputDatum' = mkEffectDatum governorInputDatum' newGovDatum
      effectInputDatum :: Datum
      effectInputDatum = Datum $ toBuiltinData effectInputDatum'
      effectInput :: TxOut
      effectInput =
        TxOut
          { txOutAddress = effectValidatorAddress
          , txOutValue = at -- The effect carry an authotity token.
          , txOutDatum = OutputDatumHash $ toDatumHash effectInputDatum
          , txOutReferenceScript = Nothing
          }

      --

      governorOutputDatum' :: GovernorDatum
      governorOutputDatum' = effectInputDatum'.newDatum
      governorOutputDatum :: Datum
      governorOutputDatum = Datum $ toBuiltinData governorOutputDatum'
      governorOutput :: TxOut
      governorOutput =
        TxOut
          { txOutAddress = governorValidatorAddress
          , txOutValue = mconcat [gst, minAda]
          , txOutDatum = OutputDatumHash $ toDatumHash governorOutputDatum
          , txOutReferenceScript = Nothing
          }
   in TxInfo
        { txInfoInputs =
            [ TxInInfo effectRef effectInput
            , TxInInfo govRef governorInput
            ]
        , txInfoReferenceInputs = []
        , txInfoOutputs = [governorOutput]
        , txInfoFee = Value.singleton "" "" 2
        , txInfoMint = burnt
        , txInfoDCert = []
        , txInfoWdrl = AssocMap.empty
        , txInfoValidRange = Interval.always
        , txInfoSignatories = [signer]
        , txInfoData = AssocMap.fromList $ datumPair <$> [governorInputDatum, governorOutputDatum, effectInputDatum]
        , txInfoRedeemers =
            AssocMap.fromList
              [ (Spending effectRef, mkRedeemer ())
              , (Spending govRef, mkRedeemer MutateGovernor)
              ]
        , txInfoId = "74c75505691e7baa981fa80e50b9b7e88dbe1eda67d4f062d89d203b"
        }

validNewGovernorDatum :: GovernorDatum
validNewGovernorDatum =
  GovernorDatum
    { proposalThresholds = def
    , nextProposalId = ProposalId 42
    , proposalTimings = def
    , createProposalTimeRangeMaxWidth = def
    , maximumCreatedProposalsPerStake = 3
    }

invalidNewGovernorDatum :: GovernorDatum
invalidNewGovernorDatum =
  GovernorDatum
    { proposalThresholds =
        def
          { toVoting = Tagged (-1)
          }
    , nextProposalId = ProposalId 42
    , proposalTimings = def
    , createProposalTimeRangeMaxWidth = def
    , maximumCreatedProposalsPerStake = 3
    }
