{- |
Module     : Agora.Effect
Maintainer : emi@haskell.fyi
Description: Helpers for constructing effects

Helpers for constructing effects.
-}
module Agora.Effect (makeEffect) where

import Agora.AuthorityToken (singleAuthorityTokenBurned)
import Agora.Utils (passert)
import Plutarch.Api.V1 (PScriptPurpose (PSpending), PTxInfo, PTxOutRef, PValidator, PValue)
import Plutarch.Internal (punsafeCoerce)
import Plutarch.Monadic qualified as P
import Plutus.V1.Ledger.Value (CurrencySymbol)

--------------------------------------------------------------------------------

{- | Helper "template" for creating effect validator.

     In some situations, it may be the case that we need more control over how
     an effect is implemented. In such situations, it's okay to not use this
     helper.
-}
makeEffect ::
  forall (datum :: PType) (s :: S).
  PIsData datum =>
  CurrencySymbol ->
  (Term s datum -> Term s PTxOutRef -> Term s (PAsData PTxInfo) -> Term s POpaque) ->
  Term s PValidator
makeEffect gatCs' f =
  plam $ \datum _redeemer ctx' -> P.do
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    txInfo' <- plet ctx.txInfo

    let datum' :: Term _ datum
        datum' = pfromData $ punsafeCoerce datum

    PSpending txOutRef <- pmatch $ pfromData ctx.purpose
    txOutRef' <- plet (pfield @"_0" # txOutRef)

    txInfo <- pletFields @'["mint"] txInfo'
    let mint :: Term s PValue
        mint = txInfo.mint

    gatCs <- plet $ pconstant gatCs'

    passert "singleAuthorityTokenBurned" $ singleAuthorityTokenBurned gatCs txInfo' mint

    f datum' txOutRef' txInfo'

--------------------------------------------------------------------------------
