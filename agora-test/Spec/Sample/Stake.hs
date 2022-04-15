{- |
Module     : Spec.Sample.Stake
Maintainer : emi@haskell.fyi
Description: Sample based testing for Stake utxos

This module tests primarily the happy path for Stake creation
-}
module Spec.Sample.Stake (
  stake,
  policy,
  policySymbol,
  validatorHashTN,
  signer,

  -- * Script contexts
  stakeCreation,
  stakeCreationWrongDatum,
  stakeCreationUnsigned,
  stakeDepositWithdraw,
  DepositWithdrawExample (..),
) where

--------------------------------------------------------------------------------
import Plutarch.Api.V1 (
  mintingPolicySymbol,
  mkMintingPolicy,
  mkValidator,
  validatorHash,
 )
import Plutus.V1.Ledger.Api (
  Address (Address),
  Credential (ScriptCredential),
  CurrencySymbol,
  Datum (Datum),
  DatumHash (DatumHash),
  MintingPolicy (..),
  PubKeyHash,
  ScriptContext (..),
  ScriptPurpose (..),
  ToData (toBuiltinData),
  TxInInfo (TxInInfo),
  TxInfo (..),
  TxOut (txOutAddress, txOutDatumHash, txOutValue),
  ValidatorHash (ValidatorHash),
 )
import Plutus.V1.Ledger.Contexts (TxOut (TxOut), TxOutRef (TxOutRef))
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Scripts (Validator)
import Plutus.V1.Ledger.Value (AssetClass (AssetClass), TokenName (TokenName))
import Plutus.V1.Ledger.Value qualified as Value

--------------------------------------------------------------------------------

import Agora.SafeMoney (GTTag)
import Agora.Stake
import Plutarch.SafeMoney
import Spec.Util (datumPair, toDatumHash)

--------------------------------------------------------------------------------

-- | 'Stake' parameters for 'LQ'.
stake :: Stake
stake =
  Stake
    { gtClassRef =
        Tagged
          ( AssetClass
              ( "da8c30857834c6ae7203935b89278c532b3995245295456f993e1d24"
              , "LQ"
              )
          )
    }

-- | 'Stake' policy instance.
policy :: MintingPolicy
policy = mkMintingPolicy (stakePolicy stake)

policySymbol :: CurrencySymbol
policySymbol = mintingPolicySymbol policy

-- | A sample 'PubKeyHash'.
signer :: PubKeyHash
signer = "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be7401214142019c"

-- | 'Stake' validator instance.
validator :: Validator
validator = mkValidator (stakeValidator stake)

-- | 'TokenName' that represents the hash of the 'Stake' validator.
validatorHashTN :: TokenName
validatorHashTN = let ValidatorHash vh = validatorHash validator in TokenName vh

-- | This script context should be a valid transaction.
stakeCreation :: ScriptContext
stakeCreation =
  let st = Value.singleton policySymbol validatorHashTN 1 -- Stake ST
      datum :: Datum
      datum = Datum (toBuiltinData $ StakeDatum 424242424242 signer [])
   in ScriptContext
        { scriptContextTxInfo =
            TxInfo
              { txInfoInputs = []
              , txInfoOutputs =
                  [ TxOut
                      { txOutAddress = Address (ScriptCredential $ validatorHash validator) Nothing
                      , txOutValue = st <> Value.singleton "da8c30857834c6ae7203935b89278c532b3995245295456f993e1d24" "LQ" 424242424242
                      , txOutDatumHash = Just (DatumHash "")
                      }
                  ]
              , txInfoFee = Value.singleton "" "" 2
              , txInfoMint = st
              , txInfoDCert = []
              , txInfoWdrl = []
              , txInfoValidRange = Interval.always
              , txInfoSignatories = [signer]
              , txInfoData = [("", datum)]
              , txInfoId = "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
              }
        , scriptContextPurpose = Minting policySymbol
        }

-- | This ScriptContext should fail because the datum has too much GT.
stakeCreationWrongDatum :: ScriptContext
stakeCreationWrongDatum =
  let datum :: Datum
      datum = Datum (toBuiltinData $ StakeDatum 4242424242424242 signer []) -- Too much GT
   in ScriptContext
        { scriptContextTxInfo = stakeCreation.scriptContextTxInfo {txInfoData = [("", datum)]}
        , scriptContextPurpose = Minting policySymbol
        }

-- | This ScriptContext should fail because the datum has too much GT.
stakeCreationUnsigned :: ScriptContext
stakeCreationUnsigned =
  ScriptContext
    { scriptContextTxInfo =
        stakeCreation.scriptContextTxInfo
          { txInfoSignatories = []
          }
    , scriptContextPurpose = Minting policySymbol
    }

--------------------------------------------------------------------------------

-- | Config for creating a ScriptContext that deposits or withdraws.
data DepositWithdrawExample = DepositWithdrawExample
  { startAmount :: Tagged GTTag Integer
  -- ^ The amount of GT stored before the transaction.
  , delta :: Tagged GTTag Integer
  -- ^ The amount of GT deposited or withdrawn from the Stake.
  }

-- | Create a ScriptContext that deposits or withdraws, given the config for it.
stakeDepositWithdraw :: DepositWithdrawExample -> ScriptContext
stakeDepositWithdraw config =
  let st = Value.singleton policySymbol validatorHashTN 1 -- Stake ST
      stakeBefore :: StakeDatum
      stakeBefore = StakeDatum config.startAmount signer []

      stakeAfter :: StakeDatum
      stakeAfter = stakeBefore {stakedAmount = stakeBefore.stakedAmount + config.delta}
   in ScriptContext
        { scriptContextTxInfo =
            TxInfo
              { txInfoInputs =
                  [ TxInInfo
                      (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 1)
                      TxOut
                        { txOutAddress = Address (ScriptCredential $ validatorHash validator) Nothing
                        , txOutValue =
                            st
                              <> Value.assetClassValue (untag stake.gtClassRef) (untag stakeBefore.stakedAmount)
                        , txOutDatumHash = Just (toDatumHash stakeAfter)
                        }
                  ]
              , txInfoOutputs =
                  [ TxOut
                      { txOutAddress = Address (ScriptCredential $ validatorHash validator) Nothing
                      , txOutValue =
                          st <> Value.assetClassValue (untag stake.gtClassRef) (untag stakeAfter.stakedAmount)
                      , txOutDatumHash = Just (toDatumHash stakeAfter)
                      }
                  ]
              , txInfoFee = Value.singleton "" "" 2
              , txInfoMint = st
              , txInfoDCert = []
              , txInfoWdrl = []
              , txInfoValidRange = Interval.always
              , txInfoSignatories = [signer]
              , txInfoData = [datumPair stakeAfter]
              , txInfoId = "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
              }
        , scriptContextPurpose = Spending (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 1)
        }
