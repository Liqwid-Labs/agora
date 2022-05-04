{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wwarn #-}

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
) where

import Agora.Effect.NoOp (noOpValidator)
import Agora.Treasury (TreasuryRedeemer (SpendTreasuryGAT), treasuryValidator)
import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))
import Plutarch.Api.V1 (mkValidator, validatorHash)
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PIsDataReprInstances (..),
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutus.V1.Ledger.Address (Address (..))
import Plutus.V1.Ledger.Api (BuiltinByteString)
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
import Plutus.V1.Ledger.Scripts (Validator, ValidatorHash (ValidatorHash))
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName (TokenName))
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
        { txOutAddress =
            Address
              (ScriptCredential $ validatorHash trValidator)
              Nothing
        , txOutValue = Value.singleton "" "" 0
        , txOutDatumHash = Just (toDatumHash ())
        }

treasuryRef :: TxOutRef
treasuryRef = TxOutRef "73475cb40a568e8da8a045ced110137e159f890ac4da883b6b17dc651b3a8049" 1

effectRef :: TxOutRef
effectRef = TxOutRef "52b67b60260da3937510ad545c7f46f8d9915bd27e1082e76947fb309f913bd3" 0

mockEffect :: Validator
mockEffect = mkValidator $ noOpValidator gatCs

addressBs :: BuiltinByteString
(ValidatorHash addressBs) = validatorHash mockEffect

gatTn :: TokenName
gatTn = TokenName addressBs

------------------------------------------------------------------

-- Invalid treasury redeemer.

data BadTreasuryRedeemer = NukeTheSystem
  deriving stock (Eq, Show, GHC.Generic)

PlutusTx.makeIsDataIndexed
  ''BadTreasuryRedeemer
  [ ('NukeTheSystem, 0)
  ]

data PBadTreasuryRedeemer (s :: S)
  = PNukeTheSystem (Term s (PDataRecord '[]))
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

------------------------------------------------------------------
