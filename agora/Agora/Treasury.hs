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
import Plutarch.Api.V1.Contexts (PScriptContext, PScriptPurpose (PMinting))
import Plutarch.Api.V1.Value (PCurrencySymbol, PValue)
import Plutarch.DataRepr (
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Monadic qualified as P
import Plutus.V1.Ledger.Value (CurrencySymbol)

--------------------------------------------------------------------------------

import Agora.AuthorityToken (singleAuthorityTokenBurned)
import Agora.Utils (passert)

{- | Validator ensuring that transactions consuming the treasury
     do so in a valid manner.
-}
treasuryV ::
  forall {s :: S}.
  CurrencySymbol ->
  Term
    s
    ( PAsData PTreasuryDatum
        :--> PAsData PTreasuryRedeemer
        :--> PAsData PScriptContext
        :--> PUnit
    )
treasuryV gatCs' = plam $ \_d r ctx' -> P.do
  -- plet required fields from script context.
  ctx <- pletFields @["txInfo", "purpose"] ctx'

  -- Ensure that script is for burning i.e. minting a negative amount.
  PMinting _ <- pmatch ctx.purpose

  -- Ensure redeemer type is valid.
  PAlterTreasuryParams _ <- pmatch $ pfromData r

  -- Get the minted value from txInfo.
  txInfo' <- plet ctx.txInfo
  txInfo <- pletFields @'["mint"] txInfo'
  let mint :: Term s PValue
      mint = txInfo.mint

  gatCs <- plet $ pconstant gatCs'

  passert "singleAuthorityTokenBurned" $ singleAuthorityTokenBurned gatCs txInfo' mint

  pconstant ()

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
    PAlterTreasuryParams (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PTreasuryRedeemer
