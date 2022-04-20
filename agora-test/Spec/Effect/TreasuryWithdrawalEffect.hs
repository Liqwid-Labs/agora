{- |
Module     : Spec.Effect.TreasuryWithdrawalEffect
Maintainer : seungheon.ooh@gmail.com
Description: Sample based testing for Treasury Withdrawal Effect

This module tests the Treasury Withdrawal Effect.
-}
module Spec.Effect.TreasuryWithdrawalEffect (currSymbol, signer, validator, validatorHashTN, scriptContext1) where

import Plutarch.Api.V1
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Value qualified as Value

import Data.ByteString.Hash

import Agora.Effect.TreasuryWithdrawal

-- receiverList :: TreasuryWithdrawalDatum
-- receiverList = TreasuryWithdrawalDatum [(mempty, mempty)]

-- | A sample Currency Symbol.
currSymbol :: CurrencySymbol
currSymbol = CurrencySymbol "Orangebottle19721121"

-- | A sample 'PubKeyHash'.
signer :: PubKeyHash
signer = "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be7401214142019c"

-- | List of users who the effect will pay to.
users :: [Credential]
users =
  PubKeyCredential . PubKeyHash . toBuiltin . sha2
    <$> [ "Hello world"
        , "Hello Agora"
        , "Hello Plutarch"
        ]

-- | Datum for Treasury Withdrawal Effect Validator.
_datum :: TreasuryWithdrawalDatum
_datum =
  TreasuryWithdrawalDatum
    [ (users !! 0, Value.singleton currSymbol validatorHashTN 1)
    , (users !! 1, Value.singleton currSymbol validatorHashTN 1)
    , (users !! 2, Value.singleton currSymbol validatorHashTN 1)
    ]

-- | Effect validator instance.
validator :: Validator
validator = mkValidator $ treasuryWithdrawalValidator currSymbol

-- | 'TokenName' that represents the hash of the 'Stake' validator.
validatorHashTN :: TokenName
validatorHashTN = let ValidatorHash vh = validatorHash validator in TokenName vh

scriptContext1 :: ScriptContext
scriptContext1 =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs =
              [ TxInInfo -- Initiator
                  (TxOutRef "Initiator" 1)
                  TxOut
                    { txOutAddress = Address (ScriptCredential $ validatorHash validator) Nothing
                    , txOutValue = Value.singleton "" "" 2000000
                    , txOutDatumHash = Nothing
                    }
              , TxInInfo -- Treasury 1
                  (TxOutRef "Treasury 1" 1)
                  TxOut
                    { txOutAddress = Address (ScriptCredential $ validatorHash validator) Nothing
                    , txOutValue = Value.singleton currSymbol validatorHashTN 10
                    , txOutDatumHash = Nothing
                    }
              , TxInInfo -- Treasury 2
                  (TxOutRef "Treasury 2" 1)
                  TxOut
                    { txOutAddress = Address (ScriptCredential $ validatorHash validator) Nothing
                    , txOutValue = Value.singleton currSymbol validatorHashTN 10
                    , txOutDatumHash = Nothing
                    }
              ]
          , txInfoOutputs =
              [ TxOut
                  { txOutAddress = Address (users !! 0) Nothing
                  , txOutValue = Value.singleton currSymbol validatorHashTN 1
                  , txOutDatumHash = Nothing
                  }
              , TxOut
                  { txOutAddress = Address (users !! 1) Nothing
                  , txOutValue = Value.singleton currSymbol validatorHashTN 1
                  , txOutDatumHash = Nothing
                  }
              , TxOut
                  { txOutAddress = Address (users !! 2) Nothing
                  , txOutValue = Value.singleton currSymbol validatorHashTN 1
                  , txOutDatumHash = Nothing
                  }
              ]
          , txInfoFee = Value.singleton "" "" 2
          , txInfoMint = mempty
          , txInfoDCert = []
          , txInfoWdrl = []
          , txInfoValidRange = Interval.always
          , txInfoSignatories = [signer]
          , txInfoData = []
          , txInfoId = "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
          }
    , scriptContextPurpose =
        Spending (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 1)
    }
