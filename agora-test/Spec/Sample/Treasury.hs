{-# LANGUAGE TemplateHaskell #-}

{- |
Module: Spec.Sample.Treasury
Description: Sample data for `Spec.Treasury`.
Maintainer: jack@mlabs.city

This module contains sample data, used in the tests written in
`Spec.Treasury`.
-}
module Spec.Sample.Treasury (
  BadTreasuryRedeemer (NukeTheSystem),
  gatCs,
  validCtx,
  treasuryRef,
  gatTn,
  walletIn,
  trCredential,
) where

import Agora.Effect.NoOp (noOpValidator)
import Agora.Treasury (
  treasuryValidator,
 )
import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))
import Plutarch.Api.V1 (mkValidator, validatorHash)
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PIsDataReprInstances (..),
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutus.V1.Ledger.Address (Address (..))
import Plutus.V1.Ledger.Api (
  BuiltinByteString,
  Credential (PubKeyCredential),
  PubKeyHash (PubKeyHash),
 )
import Plutus.V1.Ledger.Contexts (
  ScriptContext (..),
  ScriptPurpose (Minting),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
 )
import Plutus.V1.Ledger.Credential (Credential (ScriptCredential))
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Scripts (
  Validator,
  ValidatorHash (ValidatorHash),
 )
import Plutus.V1.Ledger.Value (
  CurrencySymbol,
  TokenName (TokenName),
 )
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import Spec.Sample.Shared (signer)
import Spec.Util (datumPair, toDatumHash)

{- | Arbitrary 'CurrencySymbol', representing the 'CurrencySymbol'
     of a valid governance authority token (GAT).
-}
gatCs :: CurrencySymbol
gatCs = "73475cb40a568e8da8a045ced110137e159f890ac4da883b6b17dc651b3a8049"

trValidator :: Validator
trValidator = mkValidator (treasuryValidator gatCs)

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
                  Address (ScriptCredential $ validatorHash mockEffect) Nothing
              , txOutValue = Value.singleton gatCs gatTn 1
              , txOutDatumHash = Just (toDatumHash ())
              }
        }
    treasuryOut :: TxOut =
      TxOut
        { txOutAddress = Address trCredential Nothing
        , txOutValue = Value.singleton "" "" 0
        , txOutDatumHash = Just (toDatumHash ())
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

-- | `ScriptCredential` used for the dummy treasury validator.
trCredential :: Credential
trCredential = ScriptCredential $ validatorHash trValidator

-- | Mock effect script, used for testing.
mockEffect :: Validator
mockEffect = mkValidator $ noOpValidator gatCs

-- | The hash of the mock effect script.
addressBs :: BuiltinByteString
(ValidatorHash addressBs) = validatorHash mockEffect

-- | `TokenName` for GAT generated from address of `mockEffect`.
gatTn :: TokenName
gatTn = TokenName addressBs

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

-- | Unsupported treasury redeemer.
data BadTreasuryRedeemer
  = -- | Unsupported treasury redeemer.
    NukeTheSystem Integer
  deriving stock (Eq, Show, GHC.Generic)

PlutusTx.makeIsDataIndexed
  ''BadTreasuryRedeemer
  [ ('NukeTheSystem, 0)
  ]

-- | Plutarch implementation of `BadTreasuryRedeemer`.
data PBadTreasuryRedeemer (s :: S)
  = PNukeTheSystem (Term s (PDataRecord '["_0" ':= PInteger]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PBadTreasuryRedeemer

instance PUnsafeLiftDecl PBadTreasuryRedeemer where
  type PLifted PBadTreasuryRedeemer = BadTreasuryRedeemer
deriving via
  ( DerivePConstantViaData
      BadTreasuryRedeemer
      PBadTreasuryRedeemer
  )
  instance
    (PConstantDecl BadTreasuryRedeemer)
