{- |
Module     : Agora.Effect
Maintainer : emi@haskell.fyi
Description: Helpers for constructing effects

Helpers for constructing effects.
-}
module Agora.Effect (makeEffect) where

import Plutarch.Api.V1 (PScriptPurpose (PSpending), PTxInfo, PTxOutRef, PValidator)
import Plutarch.Internal (punsafeCoerce)
import Plutarch.Monadic qualified as P

--------------------------------------------------------------------------------

-- | Helper 'template' for creating effect validator.
makeEffect ::
  forall (datum :: PType) (s :: S).
  PIsData datum =>
  (Term s datum -> Term s PTxOutRef -> Term s PTxInfo -> Term s POpaque) ->
  Term s PValidator
makeEffect f =
  plam $ \datum _redeemer ctx' -> P.do
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    txInfo' <- plet ctx.txInfo

    let datum' :: Term _ datum
        datum' = pfromData $ punsafeCoerce datum

    PSpending txOutRef <- pmatch $ pfromData ctx.purpose
    txOutRef' <- plet (pfield @"_0" # txOutRef)

    -- TODO: Here, check that a *single* GAT is burned.

    f datum' txOutRef' txInfo'

--------------------------------------------------------------------------------
