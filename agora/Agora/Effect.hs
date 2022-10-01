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

{- | Helper "template" for creating effect validator.

     In some situations, it may be the case that we need more control over how
     an effect is implemented. In such situations, it's okay to not use this
     helper.

     @since 0.1.0
-}
makeEffect ::
  forall (datum :: PType) (s :: S).
  (PTryFrom PData datum, PIsData datum) =>
  ( Term s PCurrencySymbol ->
    Term s datum ->
    Term s PTxOutRef ->
    Term s (PAsData PTxInfo) ->
    Term s POpaque
  ) ->
  Term s PCurrencySymbol ->
  Term s PValidator
makeEffect f =
  \atSymbol -> plam $ \datum _redeemer ctx' -> unTermCont $ do
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
    PSpending txOutRef <- pmatchC $ pfromData ctx.purpose
    txOutRef' <- pletC (pfield @"_0" # txOutRef)

    txInfo <- pletFieldsC @'["mint", "inputs"] ctx.txInfo

    pguardC "A single authority token has been burned" $
      singleAuthorityTokenBurned atSymbol txInfo.inputs txInfo.mint

    -- run effect function
    pure $ f atSymbol datum' txOutRef' ctx.txInfo
