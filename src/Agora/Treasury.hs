{-# OPTIONS_GHC -Wwarn #-}

{- |
Module: Agora.Treasury
Maintainer: jack@mlabs.city
Description: Treasury scripts.

Contains the datum, redeemer and validator for a template DAO
treasury.
-}
module Agora.Treasury (treasuryV) where

import GHC.Generics qualified as GHC
import Generics.SOP
import Plutarch.Api.V1.Contexts (
  PScriptContext,
  PTuple,
  PTxInfo (PTxInfo),
 )
import Plutarch.Api.V1.Maybe (PMaybeData (PDJust))
import Plutarch.Api.V1.Scripts (PDatum, PDatumHash)
import Plutarch.Api.V1.Tx (
  PTxInInfo,
  PTxOut,
 )
import Plutarch.Api.V1.Value (PCurrencySymbol, PValue)
import Plutarch.Builtin (pforgetData)
import Plutarch.DataRepr (
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Monadic qualified as P

{- | Validator ensuring that transactions consuming the treasury
     do so in a valid manner.
-}
treasuryV ::
  Term
    s
    ( PAsData PTreasuryDatum
        :--> PAsData PTreasuryRedeemer
        :--> PAsData PScriptContext
        :--> PUnit
    )
treasuryV = plam $ \d r ctx' -> P.do
  pmatch (pfromData r) $ \case
    -- Redeemer seeking to alter treasury parameters. Must ensure
    -- a valid GAT is burned in the transaction.
    PAlterTrParams _ ->
      ptraceError "Altering treasury parameters is not currently supported."
    -- Redeemer for all other treasury actions. Must ensure datum
    -- is unchanged and no value has been removed from the
    -- treasury.
    PRedeemTreasury _ -> P.do
      -- Amount of value treasury has before transaction.
      let valueTrIn = undefined

      -- Amount of value treasury has after transaction.
      let valueTrOut = undefined

      let vOutExceedsVIn = undefined

      pif
        (vOutExceedsVIn)
        (pconstant ())
        (ptraceError "Value has been illegally deducted from treasury.")

{- | Plutarch level type representing datum of the treasury.
     Contains:

       - @stateThread@ representing the asset class of the
         treasury's state thread token.
-}
newtype PTreasuryDatum (s :: S)
  = PTreasuryDatum
      ( Term
          s
          ( PDataRecord
              '[ "stateThread" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PTreasuryDatum

{- | Plutarch level type representing valid redeemers of the
     treasury.
-}
data PTreasuryRedeemer (s :: S)
  = -- | TODO: will allow the burning of GATs to alter Treasury params.
    PAlterTrParams (Term s (PDataRecord '[]))
  | -- | All other treasury actions. Value must not decrease.
    PRedeemTreasury (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PTreasuryRedeemer
