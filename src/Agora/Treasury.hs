{-# OPTIONS_GHC -Wwarn #-}

{- |
Module: Agora.Treasury
Maintainer: jack@mlabs.city
Description: Treasury scripts.

Contains the datum, redeemer and validator for a template DAO
treasury.
-}
module Agora.Treasury where

import GHC.Generics qualified as GHC
import Generics.SOP
import Plutarch.Api.V1.Contexts (PScriptContext)
import Plutarch.Api.V1.Value (PCurrencySymbol, PValue)
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
treasuryV = plam $ \_d r ctx' -> P.do
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  pmatch (pfromData r) $ \case
    -- Validation for receiving funds.
    PReceiveFunds _ -> pconstant ()
    -- Validation for witnessing transaction.
    PWitnessTreasury _ -> pconstant ()

{- | Plutarch level type representing datum of the treasury.
     Contains:

       - @reserves@ representing the current value kept in the
         treasury.
       - @stateThread@ representing the asset class of the
         treasury's state thread token.
-}
newtype PTreasuryDatum (s :: S)
  = PTreasuryDatum
      ( Term
          s
          ( PDataRecord
              '[ "reserves" ':= PValue
               , "stateThread" ':= PCurrencySymbol
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
  = -- | Receive funds and place them in the treasury.
    PReceiveFunds (Term s (PDataRecord '["_0" ':= PValue]))
  | -- | Serve as a witness for any transaction. Must remain unaltered.
    PWitnessTreasury (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PTreasuryRedeemer
