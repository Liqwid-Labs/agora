{-# LANGUAGE TemplateHaskell #-}

{- |
Module: Agora.Treasury
Maintainer: jack@mlabs.city
Description: Treasury scripts.

Contains the datum, redeemer and validator for a template DAO
treasury.
-}
module Agora.Treasury (module Agora.Treasury) where

import Agora.AuthorityToken (singleAuthorityTokenBurned)
import Agora.Utils (tcassert, tclet, tcmatch, tctryFrom)
import GHC.Generics qualified as GHC
import Generics.SOP
import Plutarch.Api.V1 (PValidator)
import Plutarch.Api.V1.Contexts (PScriptPurpose (PMinting))
import "plutarch" Plutarch.Api.V1.Value (PValue)
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Lift (PConstantDecl (..), PLifted (..), PUnsafeLiftDecl)
import Plutarch.TryFrom ()
import PlutusLedgerApi.V1.Value (CurrencySymbol)
import PlutusTx qualified

--------------------------------------------------------------------------------

-- | Redeemer for Treasury actions.
data TreasuryRedeemer
  = -- | Allow transaction to pass by delegating to GAT burn.
    SpendTreasuryGAT
  deriving stock (Eq, Show, GHC.Generic)

PlutusTx.makeIsDataIndexed
  ''TreasuryRedeemer
  [ ('SpendTreasuryGAT, 0)
  ]

--------------------------------------------------------------------------------

{- | Plutarch level type representing valid redeemers of the
     treasury.
-}
newtype PTreasuryRedeemer (s :: S)
  = -- | Alters treasury parameters, subject to the burning of a
    --   governance authority token.
    PSpendTreasuryGAT (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PTreasuryRedeemer

deriving via
  PAsData (PIsDataReprInstances PTreasuryRedeemer)
  instance
    PTryFrom PData (PAsData PTreasuryRedeemer)

instance PUnsafeLiftDecl PTreasuryRedeemer where
  type PLifted PTreasuryRedeemer = TreasuryRedeemer
deriving via
  (DerivePConstantViaData TreasuryRedeemer PTreasuryRedeemer)
  instance
    (PConstantDecl TreasuryRedeemer)

--------------------------------------------------------------------------------

{- | Validator ensuring that transactions consuming the treasury
     do so in a valid manner.
-}
treasuryValidator ::
  -- | Governance Authority Token that can unlock this validator.
  CurrencySymbol ->
  ClosedTerm PValidator
treasuryValidator gatCs' = plam $ \_datum redeemer ctx' -> unTermCont $ do
  (treasuryRedeemer, _) <- tctryFrom redeemer

  -- plet required fields from script context.
  ctx <- tcont $ pletFields @["txInfo", "purpose"] ctx'

  -- Ensure that script is for burning i.e. minting a negative amount.
  PMinting _ <- tcmatch ctx.purpose

  -- Ensure redeemer type is valid.
  PSpendTreasuryGAT _ <- tcmatch $ pfromData treasuryRedeemer

  -- Get the minted value from txInfo.
  txInfo' <- tclet ctx.txInfo
  txInfo <- tcont $ pletFields @'["mint"] txInfo'
  let mint :: Term _ (PValue _ _)
      mint = txInfo.mint

  gatCs <- tclet $ pconstant gatCs'

  tcassert "A single authority token has been burned" $
    singleAuthorityTokenBurned gatCs txInfo' mint

  pure . popaque $ pconstant ()
