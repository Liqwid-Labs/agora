{-# LANGUAGE TemplateHaskell #-}

{- |
Module: Sample.Treasury
Description: Sample data for `Spec.Treasury`.
Maintainer: jack@mlabs.city

This module contains sample data, used in the tests written in
`Spec.Treasury`.
-}
module Sample.Treasury (
  gatCs,
  validCtx,
  treasuryRef,
  gatTn,
  walletIn,
  trCtxGATNameNotAddress,
) where

import Plutarch.Api.V1 (validatorHash)
import PlutusLedgerApi.V1 (
  BuiltinByteString,
  Credential (PubKeyCredential),
  PubKeyHash (PubKeyHash),
 )
import PlutusLedgerApi.V1.Address (Address (..))
import PlutusLedgerApi.V1.Contexts (
  ScriptContext (..),
  ScriptPurpose (Minting),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
 )
import PlutusLedgerApi.V1.Credential (Credential (ScriptCredential))
import PlutusLedgerApi.V1.Interval qualified as Interval
import PlutusLedgerApi.V1.Scripts (
  ValidatorHash (ValidatorHash),
 )
import PlutusLedgerApi.V1.Value qualified as Value
import Sample.Shared (
  gatCs,
  gatTn,
  minAda,
  mockTrEffect,
  signer,
  treasuryOut,
  wrongEffHash,
 )
import Test.Util (datumPair)

{- | A `ScriptContext` that should be compatible with treasury
     transactions.
-}
validCtx :: ScriptContext
validCtx =
  ScriptContext
    { scriptContextPurpose = Minting gatCs
    , scriptContextTxInfo =
        TxInfo
          { txInfoInputs =
              [ treasuryIn
              , effectIn
              ]
          , txInfoOutputs =
              [ treasuryOut
              ]
          , -- Ensure sufficient ADA for transaction costs.
            txInfoFee = Value.singleton "" "" 2 -- 2 ADA.
          , -- Burn the GAT.
            txInfoMint = Value.singleton gatCs gatTn (-1)
          , txInfoDCert = []
          , txInfoWdrl = []
          , txInfoValidRange = Interval.always
          , txInfoSignatories = [signer]
          , txInfoData =
              [ datumPair treasuryIn
              , datumPair treasuryOut
              , datumPair effectIn
              ]
          , txInfoId =
              "73475cb40a568e8da8a045ced110137e159f890ac4da883b6b17dc651b3a8049"
          }
    }
  where
    treasuryIn =
      TxInInfo
        { txInInfoOutRef = treasuryRef
        , txInInfoResolved = treasuryOut
        }
    effectIn =
      TxInInfo
        { txInInfoOutRef = effectRef
        , txInInfoResolved =
            TxOut
              { txOutAddress =
                  Address (ScriptCredential $ validatorHash mockTrEffect) Nothing
              , txOutValue =
                  mconcat
                    [ Value.singleton gatCs gatTn 1
                    , minAda
                    ]
              , txOutDatumHash = Nothing
              }
        }

-- | Reference to treasury output.
treasuryRef :: TxOutRef
treasuryRef =
  TxOutRef
    "73475cb40a568e8da8a045ced110137e159f890ac4da883b6b17dc651b3a8049"
    1

-- | Reference to dummy effect output.
effectRef :: TxOutRef
effectRef =
  TxOutRef
    "52b67b60260da3937510ad545c7f46f8d9915bd27e1082e76947fb309f913bd3"
    0

-- | Input representing a user wallet with a valid GAT.
walletIn :: TxInInfo
walletIn =
  TxInInfo
    { txInInfoOutRef =
        TxOutRef
          "cf4a8b33dd8e4493187e3339ecc3802d0cc000c947fb5559b7614153947d4e83"
          0
    , txInInfoResolved =
        TxOut
          { txOutDatumHash = Nothing
          , txOutValue = Value.singleton gatCs gatTn 1
          , txOutAddress =
              Address
                (PubKeyCredential $ PubKeyHash addressBs)
                Nothing
          }
    }

addressBs :: BuiltinByteString
(ValidatorHash addressBs) = validatorHash mockTrEffect

trCtxGATNameNotAddress :: ScriptContext
trCtxGATNameNotAddress =
  let txInfo = validCtx.scriptContextTxInfo
      inputs = txInfo.txInfoInputs
      effectIn = inputs !! 1
      invalidEff =
        effectIn
          { txInInfoResolved =
              effectIn.txInInfoResolved
                { txOutAddress = Address (ScriptCredential wrongEffHash) Nothing
                }
          }
   in validCtx
        { scriptContextTxInfo =
            txInfo
              { txInfoInputs =
                  [ head inputs
                  , invalidEff
                  ]
              }
        }
