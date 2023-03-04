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
import Agora.SafeMoney (AuthorityTokenTag)
import Plutarch.Api.V1.Value (PCurrencySymbol, PValue)
import Plutarch.Api.V2 (PScriptPurpose (PSpending), PValidator)
import Plutarch.Extra.Tagged (PTagged)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pguardC, pletFieldsC, pmatchC)

{- | Validator ensuring that transactions consuming the treasury
     do so in a valid manner.

     == Arguments

     Following arguments should be provided(in this order):
     1. authority token symbol

     @since 1.0.0
-}
treasuryValidator ::
  ClosedTerm (PAsData (PTagged AuthorityTokenTag PCurrencySymbol) :--> PValidator)
treasuryValidator = plam $ \atSymbol _ _ ctx' -> unTermCont $ do
  -- plet required fields from script context.
  ctx <- pletFieldsC @["txInfo", "purpose"] ctx'

  -- Ensure that script is for spending.
  PSpending _ <- pmatchC ctx.purpose

  -- Get the minted value from txInfo.
  txInfo <- pletFieldsC @'["mint", "inputs"] ctx.txInfo
  let mint :: Term _ (PValue _ _)
      mint = txInfo.mint

  pguardC "A single authority token has been burned" $
    singleAuthorityTokenBurned (pfromData atSymbol) txInfo.inputs mint

  pure . popaque $ pconstant ()
