{- |
Module     : Agora.Effect
Maintainer : emi@haskell.fyi
Description: Helpers for constructing effects

Helpers for constructing effects.
-}
module Agora.Effect (makeEffect) where

import Agora.AuthorityToken (singleAuthorityTokenBurned)
import Plutarch.Api.V1 (
  PCurrencySymbol,
 )
import Plutarch.Api.V2 (
  PScriptPurpose (PSpending),
  PTxInfo,
  PTxOutRef,
  PValidator,
 )
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC, ptryFromC)
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
  ( forall (s :: S).
    Term s PCurrencySymbol ->
    Term s datum ->
    Term s PTxOutRef ->
    Term s (PAsData PTxInfo) ->
    Term s POpaque
  ) ->
  ClosedTerm PValidator
makeEffect gatCs' f =
  plam $ \datum _redeemer ctx' -> unTermCont $ do
    ctx <- pletFieldsC @'["txInfo", "purpose"] ctx'

    -- Convert input datum, PData, into desierable type
    -- the way this conversion is performed should be defined
    -- by PTryFrom for each datum in effect script.
    datum' <- fst <$> ptryFromC datum

    -- Ensure purpose is Spending. Why? The only way that this
    -- effect script can actually pass any validation onto other
    -- scripts is by preventing the spend of the GAT.
    --
    -- - In the case of GATs which don't get burned, that will
    --   allow reuse of the GAT.
    --
    -- - In the case of GATs which get _referenced_, this script
    --   won't be run at all, in which case. The auth check needs
    --   to be especially written with that in mind.
    PSpending txOutRef <- pmatchC $ pfromData (getField @"purpose" ctx)
    txOutRef' <- pletC (pfield @"_0" # txOutRef)

    txInfo <- pletFieldsC @'["mint", "inputs"] (getField @"txInfo" ctx)
    gatCs <- pletC $ pconstant gatCs'

    pguardC "A single authority token has been burned" $
      singleAuthorityTokenBurned gatCs (getField @"inputs" txInfo) (getField @"mint" txInfo)

    -- run effect function
    pure $ f gatCs datum' txOutRef' (getField @"txInfo" ctx)
