{-# LANGUAGE TemplateHaskell #-}

{- |
Module: Agora.Treasury
Maintainer: jack@mlabs.city
Description: Treasury scripts.

Contains the datum, redeemer and validator for a template DAO
treasury.
-}
module Agora.Treasury (
  treasuryValidator,
) where

import Agora.AuthorityToken (singleAuthorityTokenBurned)
import Plutarch.Api.V1.Value (PValue)
import Plutarch.Api.V2 (PScriptPurpose (PSpending), PValidator)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC)
import Plutarch.TryFrom ()
import PlutusLedgerApi.V1.Value (CurrencySymbol)

{- | Validator ensuring that transactions consuming the treasury
     do so in a valid manner.

     @since 0.1.0
-}
treasuryValidator ::
  -- | Governance Authority Token that can unlock this validator.
  CurrencySymbol ->
  ClosedTerm PValidator
treasuryValidator gatCs' = plam $ \_ _ ctx' -> unTermCont $ do
  -- plet required fields from script context.
  ctx <- pletFieldsC @["txInfo", "purpose"] ctx'

  -- Ensure that script is for spending.
  PSpending _ <- pmatchC (getField @"purpose" ctx)

  -- Get the minted value from txInfo.
  txInfo <- pletFieldsC @'["mint", "inputs"] (getField @"txInfo" ctx)
  let mint :: Term _ (PValue _ _)
      mint = getField @"mint" txInfo

  gatCs <- pletC $ pconstant gatCs'

  pguardC "A single authority token has been burned" $
    singleAuthorityTokenBurned gatCs (getField @"inputs" txInfo) mint

  pure . popaque $ pconstant ()
