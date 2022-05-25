{- |
Module     : Agora.Effect
Maintainer : emi@haskell.fyi
Description: Helpers for constructing effects

Helpers for constructing effects.
-}
module Agora.Effect (makeEffect) where

import Agora.AuthorityToken (singleAuthorityTokenBurned)
import Agora.Utils (tcassert, tclet, tcmatch, tctryFrom)
import Plutarch.Api.V1 (PCurrencySymbol, PScriptPurpose (PSpending), PTxInfo, PTxOutRef, PValidator, PValue)
import Plutarch.TryFrom (PTryFrom)
import Plutus.V1.Ledger.Value (CurrencySymbol)

--------------------------------------------------------------------------------

{- | Helper "template" for creating effect validator.

     In some situations, it may be the case that we need more control over how
     an effect is implemented. In such situations, it's okay to not use this
     helper.
-}
makeEffect ::
  forall (datum :: PType).
  (PIsData datum, PTryFrom PData (PAsData datum)) =>
  CurrencySymbol ->
  (forall (s :: S). Term s PCurrencySymbol -> Term s datum -> Term s PTxOutRef -> Term s (PAsData PTxInfo) -> Term s POpaque) ->
  ClosedTerm PValidator
makeEffect gatCs' f =
  plam $ \datum _redeemer ctx' -> unTermCont $ do
    ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
    txInfo' <- tclet ctx.txInfo

    -- convert input datum, PData, into desierable type
    -- the way this conversion is performed should be defined
    -- by PTryFrom for each datum in effect script.
    (pfromData -> datum', _) <- tctryFrom datum

    -- ensure purpose is Spending.
    PSpending txOutRef <- tcmatch $ pfromData ctx.purpose
    txOutRef' <- tclet (pfield @"_0" # txOutRef)

    -- fetch minted values to ensure single GAT is burned
    txInfo <- tcont $ pletFields @'["mint"] txInfo'
    let mint :: Term _ PValue
        mint = txInfo.mint

    -- fetch script context
    gatCs <- tclet $ pconstant gatCs'

    tcassert "A single authority token has been burned" $ singleAuthorityTokenBurned gatCs txInfo' mint

    -- run effect function
    pure $ f gatCs datum' txOutRef' txInfo'
