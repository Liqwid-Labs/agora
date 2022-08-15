{- |
Module     : Sample.Effect.TreasuryWithdrawalEffect
Maintainer : seungheon.ooh@gmail.com
Description: Sample based testing for Treasury Withdrawal Effect

This module provides samples for Treasury Withdrawal Effect tests.
-}
module Sample.Effect.TreasuryWithdrawal (
  inputTreasury,
  inputUser,
  inputGAT,
  inputCollateral,
  outputTreasury,
  outputUser,
  buildReceiversOutputFromDatum,
  currSymbol,
  users,
  treasuries,
  buildScriptContext,
) where

import Agora.Effect.TreasuryWithdrawal (
  TreasuryWithdrawalDatum (TreasuryWithdrawalDatum),
  treasuryWithdrawalValidator,
 )
import Plutarch.Api.V2 (mkValidator, validatorHash)
import PlutusLedgerApi.V1.Interval qualified as Interval (always)
import PlutusLedgerApi.V1.Value qualified as Value (singleton)
import PlutusLedgerApi.V2 (
  Address (Address),
  Credential (..),
  CurrencySymbol,
  DatumHash (DatumHash),
  OutputDatum (OutputDatumHash),
  PubKeyHash,
  Redeemer (Redeemer),
  ScriptContext (..),
  ScriptPurpose (Spending),
  TokenName (TokenName),
  TxInInfo (TxInInfo),
  TxInfo (..),
  TxOut (..),
  TxOutRef (TxOutRef),
  Validator,
  ValidatorHash (ValidatorHash),
  Value,
  toBuiltinData,
 )
import PlutusTx.AssocMap qualified as AssocMap
import Sample.Shared (deterministicTracingConfing)
import Test.Util (scriptCredentials, userCredentials)

-- | A sample Currency Symbol.
currSymbol :: CurrencySymbol
currSymbol = "9c04a69c7133e26061fe5a15adaf4f79cd51e47ef22a2e3c91a36f04"

-- | A sample 'PubKeyHash'.
signer :: PubKeyHash
signer = "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be7401214142019c"

-- | List of users who the effect will pay to.
users :: [Credential]
users = userCredentials

-- | List of users who the effect will pay to.
treasuries :: [Credential]
treasuries = scriptCredentials

inputGAT :: TxInInfo
inputGAT =
  TxInInfo
    (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 1)
    TxOut
      { txOutAddress = Address (ScriptCredential $ validatorHash validator) Nothing
      , txOutValue = Value.singleton currSymbol validatorHashTN 1 -- Stake ST
      , txOutDatum = OutputDatumHash (DatumHash "")
      , txOutReferenceScript = Nothing
      }

-- | Create an input given the index of the treasury and the 'Value' at this input.
inputTreasury :: Int -> Value -> TxInInfo
inputTreasury indx val =
  TxInInfo
    (TxOutRef "" 1)
    TxOut
      { txOutAddress = Address (treasuries !! indx) Nothing
      , txOutValue = val
      , txOutDatum = OutputDatumHash (DatumHash "")
      , txOutReferenceScript = Nothing
      }

-- | Create a input given the index of the user and the 'Value' at this input.
inputUser :: Int -> Value -> TxInInfo
inputUser indx val =
  TxInInfo
    (TxOutRef "" 1)
    TxOut
      { txOutAddress = Address (users !! indx) Nothing
      , txOutValue = val
      , txOutDatum = OutputDatumHash (DatumHash "")
      , txOutReferenceScript = Nothing
      }

-- | Create a input representing the collateral given by a user.
inputCollateral :: Int -> TxInInfo
inputCollateral indx =
  TxInInfo -- Initiator
    (TxOutRef "" 1)
    TxOut
      { txOutAddress = Address (users !! indx) Nothing
      , txOutValue = Value.singleton "" "" 2000000
      , txOutDatum = OutputDatumHash (DatumHash "")
      , txOutReferenceScript = Nothing
      }

-- | Create an output at the nth treasury with the given 'Value'.
outputTreasury :: Int -> Value -> TxOut
outputTreasury indx val =
  TxOut
    { txOutAddress = Address (treasuries !! indx) Nothing
    , txOutValue = val
    , txOutDatum = OutputDatumHash (DatumHash "")
    , txOutReferenceScript = Nothing
    }

-- | Create an output at the nth user with the given 'Value'.
outputUser :: Int -> Value -> TxOut
outputUser indx val =
  TxOut
    { txOutAddress = Address (users !! indx) Nothing
    , txOutValue = val
    , txOutDatum = OutputDatumHash (DatumHash "")
    , txOutReferenceScript = Nothing
    }

-- | Create a list of the outputs that are required as encoded in 'TreasuryWithdrawalDatum'.
buildReceiversOutputFromDatum :: TreasuryWithdrawalDatum -> [TxOut]
buildReceiversOutputFromDatum (TreasuryWithdrawalDatum xs _) = f <$> xs
  where
    f x =
      TxOut
        { txOutAddress = Address (fst x) Nothing
        , txOutValue = snd x
        , txOutDatum = OutputDatumHash (DatumHash "")
        , txOutReferenceScript = Nothing
        }

-- | Effect validator instance.
validator :: Validator
validator = mkValidator deterministicTracingConfing $ treasuryWithdrawalValidator currSymbol

-- | 'TokenName' that represents the hash of the 'Agora.Stake.Stake' validator.
validatorHashTN :: TokenName
validatorHashTN = let ValidatorHash vh = validatorHash validator in TokenName vh

buildScriptContext :: [TxInInfo] -> [TxOut] -> ScriptContext
buildScriptContext inputs outputs =
  let spending = Spending (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 1)
   in ScriptContext
        { scriptContextTxInfo =
            TxInfo
              { txInfoInputs = inputs
              , txInfoReferenceInputs = []
              , txInfoOutputs = outputs
              , txInfoFee = Value.singleton "" "" 2
              , txInfoMint = Value.singleton currSymbol validatorHashTN (-1)
              , txInfoDCert = []
              , txInfoWdrl = AssocMap.empty
              , txInfoValidRange = Interval.always
              , txInfoSignatories = [signer]
              , txInfoData = AssocMap.empty
              , txInfoRedeemers =
                  AssocMap.fromList
                    [ (spending, Redeemer $ toBuiltinData ())
                    ]
              , txInfoId = "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
              }
        , scriptContextPurpose = spending
        }
