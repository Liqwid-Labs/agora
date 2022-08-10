{- |
Module     : Agora.Effect
Maintainer : emi@haskell.fyi
Description: Helpers for constructing effects

Helpers for constructing effects.
-}
module Agora.Effect (makeEffect) where

import Agora.AuthorityToken (singleAuthorityTokenBurned)
import Plutarch.Api.V1 (PCurrencySymbol, PScriptPurpose (PSpending), PTxInfo, PTxOutRef, PValidator, PValue)
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC, ptryFromC)
import Plutarch.TryFrom ()
import PlutusLedgerApi.V1.Value (CurrencySymbol)

{- | Helper "template" for creating effect validator.

     In some situations, it may be the case that we need more control over how
     an effect is implemented. In such situations, it's okay to not use this
     helper.

     @since 0.1.0
-}
makeEffect ::
  forall (datum :: PType).
  (PTryFrom PData datum, PIsData datum) =>
  CurrencySymbol ->
  (forall (s :: S). Term s PCurrencySymbol -> Term s datum -> Term s PTxOutRef -> Term s (PAsData PTxInfo) -> Term s POpaque) ->
  ClosedTerm PValidator
makeEffect gatCs' f =
  plam $ \datum _redeemer ctx' -> unTermCont $ do
    ctx <- pletFieldsC @'["txInfo", "purpose"] ctx'

    -- convert input datum, PData, into desierable type
    -- the way this conversion is performed should be defined
    -- by PTryFrom for each datum in effect script.
    (datum', _) <- ptryFromC datum

    -- ensure purpose is Spending.
    PSpending txOutRef <- pmatchC $ pfromData ctx.purpose
    txOutRef' <- pletC (pfield @"_0" # txOutRef)

    -- fetch minted values to ensure single GAT is burned
    txInfo <- pletFieldsC @'["mint", "inputs"] ctx.txInfo
    let mint :: Term _ (PValue _ _)
        mint = txInfo.mint

    -- fetch script context
    gatCs <- pletC $ pconstant gatCs'

    pguardC "A single authority token has been burned" $ singleAuthorityTokenBurned gatCs txInfo.inputs mint

    -- run effect function
    pure $ f gatCs datum' txOutRef' ctx.txInfo
