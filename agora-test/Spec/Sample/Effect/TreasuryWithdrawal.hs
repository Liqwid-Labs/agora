{- |
Module     : Spec.Sample.Effect.TreasuryWithdrawalEffect
Maintainer : seungheon.ooh@gmail.com
Description: Sample based testing for Treasury Withdrawal Effect

This module provides smaples for Treasury Withdrawal Effect tests.
-}
module Spec.Sample.Effect.TreasuryWithdrawal (datum, currSymbol, signer, validator, validatorHashTN, scriptContext1) where

import Plutarch.Api.V1
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Value qualified as Value

import Data.ByteString.Hash

import Agora.Effect.TreasuryWithdrawal

-- | A sample Currency Symbol.
currSymbol :: CurrencySymbol
currSymbol = CurrencySymbol "12312099"

-- | A sample 'PubKeyHash'.
signer :: PubKeyHash
signer = "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be7401214142019c"

-- | List of users who the effect will pay to.
users :: [Credential]
users =
  PubKeyCredential . PubKeyHash . toBuiltin . sha2
    <$> [ "Orange"
        , "Bottle"
        , "Hello"
        ]

-- | List of users who the effect will pay to.
treasuries :: [Credential]
treasuries =
  ScriptCredential . ValidatorHash . toBuiltin . sha2
    <$> [ "1234"
        , "qwer"
        , "asdf"
        ]

_aa :: [Credential]
_aa = treasuries

-- | Datum for Treasury Withdrawal Effect Validator.
datum :: TreasuryWithdrawalDatum
datum =
  TreasuryWithdrawalDatum
    [ (users !! 0, Value.singleton "1234ab" "LQ" 1)
    , (users !! 1, Value.singleton "1234ab" "LQ" 1)
    , (users !! 2, Value.singleton "1234ab" "LQ" 1)
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
                  (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 1)
                  TxOut
                    { txOutAddress = Address (ScriptCredential $ validatorHash validator) Nothing
                    , txOutValue = Value.singleton currSymbol validatorHashTN 1 -- Stake ST
                    , txOutDatumHash = Just (DatumHash "")
                    }
              , TxInInfo -- Treasury 1
                  (TxOutRef "0b121212121212121212121212121212121212121212121212121221" 2)
                  TxOut
                    { txOutAddress = Address (treasuries !! 0) Nothing
                    , txOutValue = Value.singleton "1234ab" "LQ" 10
                    , txOutDatumHash = Just (DatumHash "")
                    }
              , TxInInfo -- Treasury 2
                  (TxOutRef "0b121212121212121212a41212121212121212121212121212121221" 3)
                  TxOut
                    { txOutAddress = Address (treasuries !! 1) Nothing
                    , txOutValue = Value.singleton "1234ab" "LQ" 10
                    , txOutDatumHash = Just (DatumHash "")
                    }
              ]
          , txInfoOutputs =
              [ TxOut
                  { txOutAddress = Address (users !! 0) Nothing
                  , txOutValue = Value.singleton "1234ab" "LQ" 1
                  , txOutDatumHash = Nothing
                  }
              , TxOut
                  { txOutAddress = Address (users !! 1) Nothing
                  , txOutValue = Value.singleton "1234ab" "LQ" 1
                  , txOutDatumHash = Nothing
                  }
              , TxOut
                  { txOutAddress = Address (users !! 2) Nothing
                  , txOutValue = Value.singleton "1234ab" "LQ" 1
                  , txOutDatumHash = Nothing
                  }
              , -- Send left overs to treasuries
                TxOut
                  { txOutAddress = Address (treasuries !! 0) Nothing
                  , txOutValue = Value.singleton "1234ab" "LQ" 7
                  , txOutDatumHash = Nothing
                  }
              , TxOut
                  { txOutAddress = Address (treasuries !! 1) Nothing
                  , txOutValue = Value.singleton "1234ab" "LQ" 10
                  , txOutDatumHash = Nothing
                  }
              ]
          , txInfoFee = Value.singleton "" "" 2
          , txInfoMint = Value.singleton currSymbol validatorHashTN (-1)
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
