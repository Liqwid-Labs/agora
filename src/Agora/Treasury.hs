{- |
Module: Agora.Treasury
Maintainer: jack@mlabs.city
Description: Treasury scripts.

Contains the datum, redeemer and validator for a template DAO
treasury.
-}
module Agora.Treasury (module Agora.Treasury) where

import GHC.Generics qualified as GHC
import Generics.SOP
import Plutarch.Api.V1.Contexts (PScriptContext)
import Plutarch.Api.V1.Value (PCurrencySymbol)
import Plutarch.DataRepr (
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )

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
treasuryV = plam $ \_d r _ctx' -> P.do
  pmatch (pfromData r) $ \case
    -- Redeemer seeking to alter treasury parameters. Must ensure
    -- a valid GAT is burned in the transaction.
    PAlterTrParams _ ->
      -- TODO: Implement.
      ptraceError "Altering treasury parameters is not currently supported."

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
newtype PTreasuryRedeemer (s :: S)
  = -- | Alters treasury parameters (subject to the burning of a
    --   governance authority token).
    PAlterTrParams (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PTreasuryRedeemer
