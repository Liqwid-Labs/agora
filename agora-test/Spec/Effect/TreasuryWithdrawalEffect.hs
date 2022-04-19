{- |
Module     : Spec.Effect.TreasuryWithdrawalEffect
Maintainer : seungheon.ooh@gmail.com
Description: Sample based testing for Treasury Withdrawal Effect

This module tests the Treasury Withdrawal Effect.
-}
module Spec.Effect.TreasuryWithdrawalEffect (currSymbol, signer, validator, validatorHashTN, withdrawalEffect) where

import Plutarch.Api.V1
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Value qualified as Value

import Agora.Effect.TreasuryWithdrawal (treasuryWithdrawalValidator)

-- | A sample Currency Symbol
currSymbol :: CurrencySymbol
currSymbol = CurrencySymbol "Orange19721121"

-- | A sample 'PubKeyHash'.
signer :: PubKeyHash
signer = "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be7401214142019c"

-- | Effect validator instance.
validator :: Validator
validator = mkValidator $ treasuryWithdrawalValidator currSymbol

-- | 'TokenName' that represents the hash of the 'Stake' validator.
validatorHashTN :: TokenName
validatorHashTN = let ValidatorHash vh = validatorHash validator in TokenName vh

withdrawalEffect :: ScriptContext
withdrawalEffect =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs =
              [ TxInInfo
                  (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 1)
                  TxOut
                    { txOutAddress = Address (ScriptCredential $ validatorHash validator) Nothing
                    , txOutValue = Value.singleton currSymbol validatorHashTN 1
                    , txOutDatumHash = Nothing
                    }
              ]
          , txInfoOutputs = []
          , txInfoFee = Value.singleton "" "" 2
          , txInfoMint = mempty
          , txInfoDCert = []
          , txInfoWdrl = []
          , txInfoValidRange = Interval.always
          , txInfoSignatories = [signer]
          , txInfoData = []
          , txInfoId = "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
          }
    , scriptContextPurpose = Spending (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 1)
    }
