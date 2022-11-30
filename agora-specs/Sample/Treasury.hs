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

import Plutarch.Context (
  SpendingBuilder,
  buildSpending',
  credential,
  input,
  mint,
  output,
  script,
  signedWith,
  txId,
  withRefTxId,
  withSpendingOutRefId,
  withValue,
 )
import PlutusLedgerApi.V1.Address (Address (..))
import PlutusLedgerApi.V1.Value qualified as Value (singleton)
import PlutusLedgerApi.V2 (
  Credential (PubKeyCredential),
  OutputDatum (NoOutputDatum),
  PubKeyHash (PubKeyHash),
  ScriptHash (ScriptHash),
 )
import PlutusLedgerApi.V2.Contexts (
  ScriptContext (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
 )
import Sample.Shared (
  gatCs,
  gatTn,
  minAda,
  mockTrEffectHash,
  signer,
  trCredential,
  wrongEffHash,
 )

baseCtxBuilder :: SpendingBuilder
baseCtxBuilder =
  let treasury =
        mconcat
          [ credential trCredential
          , withValue minAda
          , withRefTxId "73475cb40a568e8da8a045ced110137e159f890ac4da883b6b17dc651b3a8049"
          ]
   in mconcat
        [ txId "73475cb40a568e8da8a045ced110137e159f890ac4da883b6b17dc651b3a8049"
        , signedWith signer
        , mint (Value.singleton gatCs gatTn (-1))
        , input treasury
        , output treasury
        , withSpendingOutRefId "73475cb40a568e8da8a045ced110137e159f890ac4da883b6b17dc651b3a8049"
        ]

{- | A `ScriptContext` that should be compatible with treasury
     transactions.
-}
validCtx :: ScriptContext
validCtx =
  let builder :: SpendingBuilder
      builder =
        mconcat
          [ baseCtxBuilder
          , input $
              mconcat
                [ script mockTrEffectHash
                , withValue (Value.singleton gatCs gatTn 1 <> minAda)
                , withRefTxId "52b67b60260da3937510ad545c7f46f8d9915bd27e1082e76947fb309f913bd3"
                ]
          ]
   in buildSpending' builder

treasuryRef :: TxOutRef
treasuryRef =
  TxOutRef
    "73475cb40a568e8da8a045ced110137e159f890ac4da883b6b17dc651b3a8049"
    1

{- | Input representing a user wallet with a valid GAT.
 TODO: Resturcture this part of test.
-}
walletIn :: TxInInfo
walletIn =
  let (ScriptHash addressBs) = mockTrEffectHash
   in TxInInfo
        { txInInfoOutRef =
            TxOutRef
              "cf4a8b33dd8e4493187e3339ecc3802d0cc000c947fb5559b7614153947d4e83"
              0
        , txInInfoResolved =
            TxOut
              { txOutDatum = NoOutputDatum
              , txOutReferenceScript = Nothing
              , txOutValue = Value.singleton gatCs gatTn 1
              , txOutAddress =
                  Address
                    (PubKeyCredential $ PubKeyHash addressBs)
                    Nothing
              }
        }

trCtxGATNameNotAddress :: ScriptContext
trCtxGATNameNotAddress =
  let builder :: SpendingBuilder
      builder =
        mconcat
          [ baseCtxBuilder
          , input $
              mconcat
                [ script wrongEffHash
                , withValue (Value.singleton gatCs gatTn 1 <> minAda)
                , withRefTxId "52b67b60260da3937510ad545c7f46f8d9915bd27e1082e76947fb309f913bd3"
                ]
          ]
   in buildSpending' builder
