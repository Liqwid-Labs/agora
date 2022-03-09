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
  stakeCreation,
  validatorHashTN,
) where

--------------------------------------------------------------------------------

import Plutarch.Api.V1 (
  mintingPolicySymbol,
  mkMintingPolicy,
  mkValidator,
  validatorHash,
 )
import Plutus.V1.Ledger.Ada (adaValueOf)
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
  TxInfo (..),
  TxOut (txOutAddress, txOutDatumHash, txOutValue),
  ValidatorHash (ValidatorHash),
 )
import Plutus.V1.Ledger.Contexts (TxOut (TxOut))
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Scripts (Validator)
import Plutus.V1.Ledger.Value (TokenName (TokenName))
import Plutus.V1.Ledger.Value qualified as Value

--------------------------------------------------------------------------------

import Agora.SafeMoney
import Agora.Stake

--------------------------------------------------------------------------------

stake :: Stake LQ
stake = Stake

policy :: MintingPolicy
policy = mkMintingPolicy (stakePolicy stake)

policySymbol :: CurrencySymbol
policySymbol = mintingPolicySymbol policy

signer :: PubKeyHash
signer = "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be7401214142019c"

validator :: Validator
validator = mkValidator (stakeValidator stake)

validatorHashTN :: TokenName
validatorHashTN = let ValidatorHash vh = validatorHash validator in TokenName vh

stakeCreation :: ScriptContext
stakeCreation =
  let st = Value.singleton policySymbol validatorHashTN 1 -- Stake ST
      datum :: Datum
      datum = Datum (toBuiltinData $ StakeDatum 424242424242 signer)
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
              , txInfoFee = adaValueOf 2
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
