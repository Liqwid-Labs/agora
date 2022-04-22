{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Agora.Effect.TreasuryWithdrawal
Maintainer : seungheon.ooh@gmail.com
Description: An Effect that withdraws treasury deposit

An Effect that withdraws treasury deposit
-}
module Agora.Effect.TreasuryWithdrawal (
  TreasuryWithdrawalDatum (..),
  PTreasuryWithdrawalDatum (..),
  treasuryWithdrawalValidator,
) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

import Agora.Effect (makeEffect)
import Agora.Utils (findTxOutByTxOutRef, paddValue, passert)
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

data TreasuryWithdrawalDatum =
  TreasuryWithdrawalDatum
    { receivers :: [(Credential, Value)]
    , treasuries :: [Credential]
    }
  deriving stock (Show, GHC.Generic)
  deriving anyclass (Generic)

PlutusTx.makeLift ''TreasuryWithdrawalDatum
PlutusTx.unstableMakeIsData ''TreasuryWithdrawalDatum

newtype PTreasuryWithdrawalDatum (s :: S)
  = PTreasuryWithdrawalDatum
      ( Term
          s
          ( PDataRecord
              '[ "receivers" ':= PBuiltinList (PAsData (PTuple PCredential PValue))
               , "treasuries" ':= PBuiltinList (PAsData PCredential)
               ]
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

{- | Withdraws given list of values to specific target addresses.
     It can be evoked by burning GAT. The transaction should have correct
     outputs to the users and any left overs should be paid back to the treasury.

     The validator does not accept any Redeemer as all "parameters" are provided
     via encoded Datum.

     Note:
     It should check...
     1. Transaction outputs should contain all of what Datum specified
     2. Left over assests should be redirected back to Treasury
     It can be more flexiable over...
     - The number of outputs themselves
-}
treasuryWithdrawalValidator :: forall {s :: S}. CurrencySymbol -> Term s PValidator
treasuryWithdrawalValidator currSymbol = makeEffect currSymbol $
  \_cs (datum' :: Term _ PTreasuryWithdrawalDatum) txOutRef' txInfo' -> P.do
    datum <- pletFields @'["receivers", "treasuries"] datum'
    txInfo <- pletFields @'["outputs", "inputs"] txInfo'
    PJust txOut <- pmatch $ findTxOutByTxOutRef # txOutRef' # pfromData txInfo'
    effInput <- pletFields @'["address", "value"] $ txOut
    let outputValues =
          pmap
            # plam
              ( \(pfromData -> txOut') -> P.do
                  txOut <- pletFields @'["address", "value"] $ txOut'
                  let cred = pfield @"credential" # pfromData txOut.address
                  pdata $ ptuple # cred # txOut.value
              )
            # txInfo.outputs
        inputValues =
          pmap
            # plam
              ( \((pfield @"resolved" #) . pfromData -> txOut') -> P.do
                  txOut <- pletFields @'["address", "value"] $ txOut'
                  let cred = pfield @"credential" # pfromData txOut.address
                  pdata $ ptuple # cred # txOut.value
              )
            # txInfo.inputs
    treasuryInputValues <-
      plet $
        pfilter
          # plam (\((pfield @"_0" #) . pfromData -> cred) -> pelem # cred # datum.treasuries)
          # inputValues
    let treasuryOutputValues =
          pfilter
            # plam
              ( \((pfield @"_0" #) . pfromData -> cred) -> pelem # cred # datum.treasuries)
            # outputValues
        treasuryInputValuesSum =
          pfoldr
            # plam (\((pfield @"_1" #) . pfromData -> x) y -> paddValue # pfromData x # y)
            # pconstant (mempty :: Value)
            # treasuryInputValues
        treasuryOutputValuesSum =
          pfoldr
            # plam (\((pfield @"_1" #) . pfromData -> x) y -> paddValue # pfromData x # y)
            # pconstant (mempty :: Value)
            # treasuryOutputValues
        receiverValuesSum =
          pfoldr
            # plam (\((pfield @"_1" #) . pfromData -> x) y -> paddValue # pfromData x # y)
            # pconstant (mempty :: Value)
            # datum.receivers
        outputContentMatchesRecivers =
          pall # plam (\out -> pelem # out # outputValues)
            #$ datum.receivers
        excessShouldBePaidToInputs =
          pdata (paddValue # receiverValuesSum # treasuryOutputValuesSum) #== pdata treasuryInputValuesSum
        shouldNotPayToEffect =
          pnot #$ pany
            # plam
              ( \x ->
                  effInput.address #== pfield @"address" # pfromData x
              )
            # pfromData txInfo.outputs

    passert "Transaction output does not match receivers" outputContentMatchesRecivers
    passert "Transaction should not pay to effects" shouldNotPayToEffect
    passert "Remainders should be returned to the treasury" excessShouldBePaidToInputs
    popaque $ pconstant ()
