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
import Agora.Utils (isPubKey)
import Plutarch.Api.V1 (
  AmountGuarantees (Positive),
  KeyGuarantees (Sorted),
  PCredential (..),
  PTuple,
  PValidator,
  PValue,
  ptuple,
 )
import Plutarch.Internal (punsafeCoerce)

import Plutarch.Api.V1.ScriptContext (pfindTxInByTxOutRef)
import "plutarch" Plutarch.Api.V1.Value (pnormalize)
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
  PIsDataReprInstances (..),
 )
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.TryFrom (PTryFrom (..))
import PlutusLedgerApi.V1.Credential (Credential)
import PlutusLedgerApi.V1.Value (CurrencySymbol, Value)
import PlutusTx qualified

{- | Datum that encodes behavior of Treasury Withdrawal effect.

Note: This Datum acts like a "predefined redeemer". Which is to say that
it encodes the properties a redeemer would, but is locked in-place until
spend.
-}
data TreasuryWithdrawalDatum = TreasuryWithdrawalDatum
  { receivers :: [(Credential, Value)]
  -- ^ AssocMap for Value sent to each receiver from the treasury.
  , treasuries :: [Credential]
  -- ^ What Credentials is spending from legal.
  }
  deriving stock (Show, GHC.Generic)
  deriving anyclass (Generic)

PlutusTx.makeLift ''TreasuryWithdrawalDatum
PlutusTx.makeIsDataIndexed ''TreasuryWithdrawalDatum [('TreasuryWithdrawalDatum, 0)]

-- | Haskell-level version of 'TreasuryWithdrawalDatum'.
newtype PTreasuryWithdrawalDatum (s :: S)
  = PTreasuryWithdrawalDatum
      ( Term
          s
          ( PDataRecord
              '[ "receivers" ':= PBuiltinList (PAsData (PTuple PCredential (PValue 'Sorted 'Positive)))
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
    (PConstantDecl TreasuryWithdrawalDatum)

instance PTryFrom PData (PAsData PTreasuryWithdrawalDatum) where
  type PTryFromExcess PData (PAsData PTreasuryWithdrawalDatum) = Const ()
  ptryFrom' opq cont =
    -- TODO: This should not use 'punsafeCoerce'.
    -- Blocked by 'PCredential', and 'PTuple'.
    cont (punsafeCoerce opq, ())

{- | Withdraws given list of values to specific target addresses.
     It can be evoked by burning GAT. The transaction should have correct
     outputs to the users and any left overs should be paid back to the treasury.

     The validator does not accept any Redeemer as all "parameters" are provided
     via encoded Datum.

     Note:
     It should check...
     1. Transaction outputs should contain all of what Datum specified
     2. Left over assets should be redirected back to Treasury
     It can be more flexiable over...
     - The number of outputs themselves
-}
treasuryWithdrawalValidator :: forall {s :: S}. CurrencySymbol -> Term s PValidator
treasuryWithdrawalValidator currSymbol = makeEffect currSymbol $
  \_cs (datum' :: Term _ PTreasuryWithdrawalDatum) txOutRef' txInfo' -> unTermCont $ do
    datum <- pletFieldsC @'["receivers", "treasuries"] datum'
    txInfo <- pletFieldsC @'["outputs", "inputs"] txInfo'
    PJust ((pfield @"resolved" #) -> txOut) <- pmatchC $ pfindTxInByTxOutRef # txOutRef' # pfromData txInfo.inputs
    effInput <- pletFieldsC @'["address", "value"] $ txOut
    outputValues <-
      pletC $
        pmap
          # plam
            ( \(pfromData -> txOut') -> unTermCont $ do
                txOut <- pletFieldsC @'["address", "value"] $ txOut'
                let cred = pfield @"credential" # pfromData txOut.address
                pure . pdata $ ptuple # cred # txOut.value
            )
          # txInfo.outputs
    inputValues <-
      pletC $
        pmap
          # plam
            ( \((pfield @"resolved" #) . pfromData -> txOut') -> unTermCont $ do
                txOut <- pletFieldsC @'["address", "value"] $ txOut'
                let cred = pfield @"credential" # pfromData txOut.address
                pure . pdata $ ptuple # cred # txOut.value
            )
          # txInfo.inputs
    let ofTreasury =
          pfilter
            # plam (\((pfield @"_0" #) . pfromData -> cred) -> pelem # cred # datum.treasuries)
        sumValues = phoistAcyclic $
          plam $ \v ->
            pnormalize
              #$ pfoldr
              # plam (\(pfromData . (pfield @"_1" #) -> x) y -> x <> y)
              # mempty
              # v
        treasuryInputValuesSum = sumValues #$ ofTreasury # inputValues
        treasuryOutputValuesSum = sumValues #$ ofTreasury # outputValues
        receiverValuesSum = sumValues # datum.receivers
        -- Constraints
        outputContentMatchesRecivers =
          pall # plam (\out -> pelem # out # outputValues)
            #$ datum.receivers
        excessShouldBePaidToInputs =
          treasuryOutputValuesSum <> receiverValuesSum #== treasuryInputValuesSum
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
                    #|| isPubKey # pfromData cred
              )
            # inputValues

    pguardC "Transaction should not pay to effects" shouldNotPayToEffect
    pguardC "Transaction output does not match receivers" outputContentMatchesRecivers
    pguardC "Remainders should be returned to the treasury" excessShouldBePaidToInputs
    pguardC "Transaction should only have treasuries specified in the datum as input" inputsAreOnlyTreasuriesOrCollateral
    pure . popaque $ pconstant ()
