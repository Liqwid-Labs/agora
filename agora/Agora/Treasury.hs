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
import GHC.Generics qualified as GHC
import Generics.SOP
import Plutarch.Api.V1 (PValidator)
import Plutarch.Api.V1.Contexts (PScriptPurpose (PMinting))
import "plutarch" Plutarch.Api.V1.Value (PValue)
import Plutarch.Extra.IsData (DerivePConstantViaEnum (..), EnumIsData (..), pmatchEnumFromData)
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC)
import Plutarch.Lift (PConstantDecl (..), PLifted (..), PUnsafeLiftDecl)
import Plutarch.TryFrom ()
import PlutusLedgerApi.V1.Value (CurrencySymbol)
import PlutusTx qualified

--------------------------------------------------------------------------------

{- | Redeemer for Treasury actions.
 Note that this redeemer is encoded as an 'Integer' on-chain.
-}
data TreasuryRedeemer
  = -- | Allow transaction to pass by delegating to GAT burn.
    SpendTreasuryGAT
  deriving stock (Eq, Show, GHC.Generic, Enum, Bounded)
  deriving (PlutusTx.ToData, PlutusTx.FromData) via (EnumIsData TreasuryRedeemer)

--------------------------------------------------------------------------------

{- | Plutarch level type representing valid redeemers of the
     treasury.
-}
newtype PTreasuryRedeemer (s :: S)
  = PTreasuryRedeemer (Term s PInteger)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving (PlutusType, PIsData) via (DerivePNewtype PTreasuryRedeemer PInteger)

instance PUnsafeLiftDecl PTreasuryRedeemer where
  type PLifted PTreasuryRedeemer = TreasuryRedeemer
deriving via
  (DerivePConstantViaEnum TreasuryRedeemer PTreasuryRedeemer)
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
  -- plet required fields from script context.
  ctx <- pletFieldsC @["txInfo", "purpose"] ctx'

  -- Ensure that script is for burning i.e. minting a negative amount.
  PMinting _ <- pmatchC ctx.purpose

  -- Ensure redeemer type is valid.
  let isRedeemerValid =
        pmatchEnumFromData redeemer $ \case
          Just SpendTreasuryGAT -> pconstant True
          _ -> pconstant False

  pguardC "Redeemer is valid" isRedeemerValid

  -- Get the minted value from txInfo.
  txInfo' <- pletC ctx.txInfo
  txInfo <- pletFieldsC @'["mint"] txInfo'
  let mint :: Term _ (PValue _ _)
      mint = txInfo.mint

  gatCs <- pletC $ pconstant gatCs'

  pguardC "A single authority token has been burned" $
    singleAuthorityTokenBurned gatCs txInfo' mint

  pure . popaque $ pconstant ()
