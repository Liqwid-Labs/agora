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

import Control.Applicative (Const)
import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

import Agora.Effect (makeEffect)
import Agora.Utils (findTxOutByTxOutRef, paddValue, passert)
import Plutarch (popaque)
import Plutarch.Api.V1 (
  PCredential (..),
  PTuple,
  PValidator,
  PValue,
  ptuple,
 )
import Plutarch.Internal (punsafeCoerce)

import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
  PIsDataReprInstances (..),
 )
import Plutarch.Lift (PUnsafeLiftDecl (..))
import Plutarch.Monadic qualified as P
import Plutarch.TryFrom (PTryFrom (..))
import Plutus.V1.Ledger.Credential (Credential)
import Plutus.V1.Ledger.Value (CurrencySymbol, Value)
import PlutusTx qualified
import Plutarch.Builtin 

data TreasuryWithdrawalDatum = TreasuryWithdrawalDatum
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

{-
TODO:
Try using unTermCont; use raw data, check over raw data, construct PTreasuryWithdrawalDataum with pdcons

More specific checks needed. Check what's inside of each keys
-}
instance PTryFrom PData PTreasuryWithdrawalDatum where
  type PTryFromExcess PData PTreasuryWithdrawalDatum = Const ()
  ptryFrom' tData = runTermCont $ do
    li <- tcont $ plet (ptrace "Converting to Constr List" $ psndBuiltin #$ pasConstr # tData)
    tcont $ \f -> pif (plength # li #== 2) (f ()) (ptraceError ("Given datum have " <> pshow (plength # li) <> " fields, PTreasuryWithdrawalDatum requires 2."))
    pure (punsafeCoerce tData, ())

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
    outputValues <-
      plet $
        pmap
          # plam
            ( \(pfromData -> txOut') -> P.do
                txOut <- pletFields @'["address", "value"] $ txOut'
                let cred = pfield @"credential" # pfromData txOut.address
                pdata $ ptuple # cred # txOut.value
            )
          # txInfo.outputs
    inputValues <-
      plet $
        pmap
          # plam
            ( \((pfield @"resolved" #) . pfromData -> txOut') -> P.do
                txOut <- pletFields @'["address", "value"] $ txOut'
                let cred = pfield @"credential" # pfromData txOut.address
                pdata $ ptuple # cred # txOut.value
            )
          # txInfo.inputs
    let ofTreasury =
          pfilter
            # plam (\((pfield @"_0" #) . pfromData -> cred) -> pelem # cred # datum.treasuries)
        sumValues =
          pfoldr
            # plam (\((pfield @"_1" #) . pfromData -> x) y -> paddValue # pfromData x # y)
            # pconstant (mempty :: Value)
        treasuryInputValuesSum = sumValues #$ ofTreasury # inputValues
        treasuryOutputValuesSum = sumValues #$ ofTreasury # outputValues
        receiverValuesSum = sumValues # datum.receivers
        isPubkey = plam $ \cred -> P.do
          pmatch cred $ \case
            PPubKeyCredential _ -> pcon PTrue
            PScriptCredential _ -> pcon PFalse

        -- Constraints
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
        inputsAreOnlyTreasuriesOrCollateral =
          pall
            # plam
              ( \((pfield @"_0" #) . pfromData -> cred) ->
                  cred #== pfield @"credential" # effInput.address
                    #|| pelem # cred # datum.treasuries
                    #|| isPubkey # pfromData cred
              )
            # inputValues

    passert "Transaction should not pay to effects" shouldNotPayToEffect
    passert "Transaction output does not match receivers" outputContentMatchesRecivers
    passert "Remainders should be returned to the treasury" excessShouldBePaidToInputs
    passert "Transaction should only have treasuries specified in the datum as input" inputsAreOnlyTreasuriesOrCollateral
    popaque $ pconstant ()
