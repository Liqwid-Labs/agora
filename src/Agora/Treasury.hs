{- |
Module: Agora.Treasury
Maintainer: jack@mlabs.city
Description: Treasury scripts.

Contains the datum, redeemer and validator for a template DAO
treasury.
-}
module Agora.Treasury (module Agora.Treasury) where

import Agora.Utils (passetClassValueOf)
import GHC.Generics qualified as GHC
import Generics.SOP
import Plutarch.Api.V1.Contexts (PScriptContext, PScriptPurpose (PMinting))
import Plutarch.Api.V1.Value (PCurrencySymbol, PValue)
import Plutarch.DataRepr (
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Monadic qualified as P
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName)

{- | Validator ensuring that transactions consuming the treasury
     do so in a valid manner.
-}
treasuryV ::
  forall {s :: S}.
  CurrencySymbol ->
  TokenName ->
  Term
    s
    ( PAsData PTreasuryDatum
        :--> PAsData PTreasuryRedeemer
        :--> PAsData PScriptContext
        :--> PUnit
    )
treasuryV cs tn = plam $ \_d r ctx' -> P.do
  -- plet required fields from script context.
  ctx <- pletFields @["txInfo", "purpose"] ctx'

  -- Ensure redeemer type is valid.
  PAlterTrParams _ <- pmatch $ pfromData r

  -- Ensure that script is for burning i.e. minting a negative amount.
  PMinting _ <- pmatch ctx.purpose

  -- Get the minted value from txInfo.
  txInfo' <- plet ctx.txInfo
  txInfo <- pletFields @'["mint"] txInfo'
  let mint :: Term s PValue
      mint = txInfo.mint
      gatAmountMinted :: Term s PInteger
      gatAmountMinted = passetClassValueOf # (pconstant cs) # (pconstant tn) # mint

  pif
    (gatAmountMinted #== -1) -- If the amount of GATS burned is not one, ...
    (ptraceError "GAT not burned.") -- ... then error.
    (pconstant ()) -- ... else success.

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
  = -- | Alters treasury parameters, subject to the burning of a
    --   governance authority token.
    PAlterTrParams (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PTreasuryRedeemer
