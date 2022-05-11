{- |
Module     : Spec.Sample.Effect.TreasuryWithdrawalEffect
Maintainer : seungheon.ooh@gmail.com
Description: Sample based testing for Treasury Withdrawal Effect

This module provides smaples for Treasury Withdrawal Effect tests.
-}
module Spec.Sample.Effect.TreasuryWithdrawal (
  signer,
  validatorHashTN,
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

import Plutarch.Api.V1 (mkValidator, validatorHash)
import Plutus.V1.Ledger.Api (
  Address (Address),
  Credential (..),
  CurrencySymbol (CurrencySymbol),
  DatumHash (DatumHash),
  PubKeyHash (PubKeyHash),
  ScriptContext (..),
  ScriptPurpose (Spending),
  TokenName (TokenName),
  TxInInfo (TxInInfo),
  TxInfo (
    TxInfo,
    txInfoDCert,
    txInfoData,
    txInfoFee,
    txInfoId,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoSignatories,
    txInfoValidRange,
    txInfoWdrl
  ),
  TxOut (..),
  TxOutRef (TxOutRef),
  Validator,
  ValidatorHash (ValidatorHash),
  Value,
  toBuiltin,
 )
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Value qualified as Value

import Data.ByteString.Char8 qualified as C
import Data.ByteString.Hash (sha2)

import Agora.Effect.TreasuryWithdrawal (
  TreasuryWithdrawalDatum (TreasuryWithdrawalDatum),
  treasuryWithdrawalValidator,
 )

-- | A sample Currency Symbol.
currSymbol :: CurrencySymbol
currSymbol = CurrencySymbol "12312099"

-- | A sample 'PubKeyHash'.
signer :: PubKeyHash
signer = "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be7401214142019c"

-- | List of users who the effect will pay to.
users :: [Credential]
users = PubKeyCredential . PubKeyHash . toBuiltin . sha2 . C.pack . show <$> ([1 ..] :: [Integer])

-- | List of users who the effect will pay to.
treasuries :: [Credential]
treasuries = ScriptCredential . ValidatorHash . toBuiltin . sha2 . C.pack . show <$> ([1 ..] :: [Integer])

inputGAT :: TxInInfo
inputGAT =
  TxInInfo
    (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 1)
    TxOut
      { txOutAddress = Address (ScriptCredential $ validatorHash validator) Nothing
      , txOutValue = Value.singleton currSymbol validatorHashTN 1 -- Stake ST
      , txOutDatumHash = Just (DatumHash "")
      }

inputTreasury :: Int -> Value -> TxInInfo
inputTreasury indx val =
  TxInInfo
    (TxOutRef "" 1)
    TxOut
      { txOutAddress = Address (treasuries !! indx) Nothing
      , txOutValue = val
      , txOutDatumHash = Just (DatumHash "")
      }

inputUser :: Int -> Value -> TxInInfo
inputUser indx val =
  TxInInfo
    (TxOutRef "" 1)
    TxOut
      { txOutAddress = Address (users !! indx) Nothing
      , txOutValue = val
      , txOutDatumHash = Just (DatumHash "")
      }

inputCollateral :: Int -> TxInInfo
inputCollateral indx =
  TxInInfo -- Initiator
    (TxOutRef "" 1)
    TxOut
      { txOutAddress = Address (users !! indx) Nothing
      , txOutValue = Value.singleton "" "" 2000000
      , txOutDatumHash = Just (DatumHash "")
      }

outputTreasury :: Int -> Value -> TxOut
outputTreasury indx val =
  TxOut
    { txOutAddress = Address (treasuries !! indx) Nothing
    , txOutValue = val
    , txOutDatumHash = Nothing
    }

outputUser :: Int -> Value -> TxOut
outputUser indx val =
  TxOut
    { txOutAddress = Address (users !! indx) Nothing
    , txOutValue = val
    , txOutDatumHash = Nothing
    }

buildReceiversOutputFromDatum :: TreasuryWithdrawalDatum -> [TxOut]
buildReceiversOutputFromDatum (TreasuryWithdrawalDatum xs _) = f <$> xs
  where
    f x =
      TxOut
        { txOutAddress = Address (fst x) Nothing
        , txOutValue = snd x
        , txOutDatumHash = Nothing
        }

-- | Effect validator instance.
validator :: Validator
validator = mkValidator $ treasuryWithdrawalValidator currSymbol

-- | 'TokenName' that represents the hash of the 'Stake' validator.
validatorHashTN :: TokenName
validatorHashTN = let ValidatorHash vh = validatorHash validator in TokenName vh

buildScriptContext :: [TxInInfo] -> [TxOut] -> ScriptContext
buildScriptContext inputs outputs =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs = inputs
          , txInfoOutputs = outputs
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
