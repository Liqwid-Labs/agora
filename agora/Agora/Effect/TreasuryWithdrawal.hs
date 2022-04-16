{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Agora.Effect.TreasuryWithdrawal
Maintainer : seungheon.ooh@gmail.com
Description: An Effect that withdraws treasury deposit

An Effect that withdraws treasury deposit
-}
module Agora.Effect.TreasuryWithdrawal (TreasuryWithdrawalDatum, PTreasuryWithdrawalDatum, treasuryWithdrawalValidator) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

import Agora.Effect (makeEffect)
import Agora.Utils (findTxOutByTxOutRef, passert)
import Plutarch (popaque)
import Plutarch.Api.V1 (
  PCredential,
  PTuple,
  PValidator,
  PValue,
  ptuple,
 )

import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
  PIsDataReprInstances (..),
 )
import Plutarch.Lift (PUnsafeLiftDecl (..))
import Plutarch.Monadic qualified as P
import Plutus.V1.Ledger.Credential (Credential)
import Plutus.V1.Ledger.Value (CurrencySymbol, Value)
import PlutusTx qualified

newtype TreasuryWithdrawalDatum = TreasuryWithdrawalDatum {receivers :: [(Credential, Value)]}
  deriving stock (Show, GHC.Generic)
  deriving anyclass (Generic)

PlutusTx.makeLift ''TreasuryWithdrawalDatum
PlutusTx.unstableMakeIsData ''TreasuryWithdrawalDatum

newtype PTreasuryWithdrawalDatum (s :: S)
  = PTreasuryWithdrawalDatum
      ( Term
          s
          ( PDataRecord
              '["receivers" ':= PBuiltinList (PAsData (PTuple PCredential PValue))]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PTreasuryWithdrawalDatum

instance PUnsafeLiftDecl PTreasuryWithdrawalDatum where
  type PLifted PTreasuryWithdrawalDatum = TreasuryWithdrawalDatum
deriving via
  (DerivePConstantViaData TreasuryWithdrawalDatum PTreasuryWithdrawalDatum)
  instance
    (PConstant TreasuryWithdrawalDatum)

treasuryWithdrawalValidator :: forall {s :: S}. CurrencySymbol -> Term s PValidator
treasuryWithdrawalValidator currSymbol = makeEffect currSymbol $
  \_cs (datum' :: Term _ PTreasuryWithdrawalDatum) txOutRef' txInfo' -> P.do
    receivers <- plet $ pfromData $ pfield @"receivers" # datum'
    txInfo <- pletFields @'["outputs"] txInfo'
    let outputValues =
          pmap
            # plam
              ( \out' -> P.do
                  out <- pletFields @'["address", "value"] $ pfromData out'
                  cred <- pletFields @'["credential"] $ pfromData out.address
                  pdata $ ptuple # cred.credential # out.value
              )
            #$ txInfo.outputs
        outputContentMatchesRecivers =
          pall # plam (\out -> pelem # out # outputValues)
            #$ receivers
        outputNumberMatchesReceivers = plength # receivers #== plength # (pfromData txInfo.outputs)
        outputIsNotPayingToEffect = P.do
          PJust txOut <- pmatch $ findTxOutByTxOutRef # txOutRef' # pfromData txInfo'
          input <- pletFields @'["address", "value"] $ txOut
          let notPayingToEffect =
                pnot #$ pany
                  # plam
                    ( \x ->
                        input.address #== pfield @"address" # pfromData x
                    )
                  # pfromData txInfo.outputs
          notPayingToEffect

    passert "Transaction output does not match receivers" outputContentMatchesRecivers
    passert "" outputNumberMatchesReceivers
    passert "" outputIsNotPayingToEffect

    popaque $ pconstant ()
