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
  PTxInInfo (PTxInInfo),
  PTxOut (PTxOut),
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
    receivers <- plet $ pfromData $ pfield @"receivers" # datum'
    txInfo <- pletFields @'["outputs", "inputs"] txInfo'
    PJust txOut <- pmatch $ findTxOutByTxOutRef # txOutRef' # pfromData txInfo'
    effInput <- pletFields @'["address", "value"] $ txOut
    let outputValues =
          pmap
            # plam
              ( \out' -> P.do
                  out <- pletFields @'["address", "value"] $ pfromData out'
                  cred <- pletFields @'["credential"] $ pfromData out.address
                  pdata $ ptuple # cred.credential # out.value
              )
            # txInfo.outputs
        outputContentMatchesRecivers =
          pall # plam (\out -> pelem # out # outputValues)
            #$ receivers
        sumValues =
          pfoldr
            # plam
              ( \((pfield @"_1" #) . pfromData -> x) y -> P.do
                  paddValue # pfromData x # y
              )
            # pconstant (mempty :: Value)
        inputCred =
          pmap
            # plam
              ( \inInfo -> P.do
                  PTxInInfo inInfo' <- pmatch $ pfromData inInfo
                  PTxOut out <- pmatch $ pfromData $ pfield @"resolved" # inInfo'
                  let addr = pfromData $ pfield @"address" # out
                  pfield @"credential" # addr
              )
            # pfromData txInfo.inputs
        totalTreasuryInputs =
          pfoldr
            # plam
              ( \x' y -> P.do
                  PTxInInfo x <- pmatch $ pfromData x'
                  PTxOut out <- pmatch $ pfromData $ pfield @"resolved" # x
                  -- only take ones from treasury
                  pif
                    (effInput.address #== pfield @"address" # out)
                    (paddValue # pfromData (pfield @"value" # out) # y)
                    y
              )
            # pconstant (mempty :: Value)
            # pfromData txInfo.inputs
        sumOutputsToTreasury =
          sumValues
            #$ pfilter
            # plam
              ( \((pfield @"_0" #) . pfromData -> addr) ->
                  pelem # addr # inputCred
                    #&& pnot # (addr #== pfield @"credential" # effInput.address)
              )
            # outputValues
        -- TODO: Probably need to check/exclude the effect input...
        excessShouldBePaidToInputs =
          pdata (paddValue # (sumValues # receivers) # sumOutputsToTreasury) #== pdata totalTreasuryInputs
        shouldNotPayToEffect =
          pnot #$ pany
            # plam
              ( \x ->
                  effInput.address #== pfield @"address" # pfromData x
              )
            # pfromData txInfo.outputs

    passert "Transaction output does not match receivers" outputContentMatchesRecivers
    passert "Transaction should not pay to effects" shouldNotPayToEffect
    passert "Remainders should be returned to the treasury" excessShouldBePaidToInputs -- We might not need this.
    popaque $ pconstant ()
