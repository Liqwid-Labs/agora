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
import Generics.SOP (Generic, I (I))
import Plutarch.Api.V1 (PValidator)
import Plutarch.Api.V1.Contexts (PScriptPurpose (PMinting))
import "plutarch" Plutarch.Api.V1.Value (PValue)
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Extra.TermCont (pguardC, pletC, pmatchC, ptryFromC)
import Plutarch.Lift (PConstantDecl (..), PLifted (..), PUnsafeLiftDecl)
import Plutarch.TryFrom ()
import PlutusLedgerApi.V1.Value (CurrencySymbol)
import PlutusTx qualified

{- | Redeemer for Treasury actions.

     @since 0.1.0
-}
data TreasuryRedeemer
  = -- | Allow transaction to pass by delegating to GAT burn.
    SpendTreasuryGAT
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      GHC.Generic
    )

-- | @since 0.1.0
PlutusTx.makeIsDataIndexed
  ''TreasuryRedeemer
  [ ('SpendTreasuryGAT, 0)
  ]

--------------------------------------------------------------------------------

{- | Plutarch level type representing valid redeemers of the
     treasury.

     @since 0.1.0
-}
newtype PTreasuryRedeemer (s :: S)
  = -- | Alters treasury parameters, subject to the burning of a
    --   governance authority token.
    PSpendTreasuryGAT (Term s (PDataRecord '[]))
  deriving stock
    ( -- | @since 0.1.0
      GHC.Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      Generic
    , -- | @since 0.1.0
      PIsDataRepr
    )
  deriving
    ( -- | @since 0.1.0
      PlutusType
    , -- | @since 0.1.0
      PIsData
    )
    via PIsDataReprInstances PTreasuryRedeemer

-- | @since 0.1.0
deriving via
  PAsData (PIsDataReprInstances PTreasuryRedeemer)
  instance
    PTryFrom PData (PAsData PTreasuryRedeemer)

-- | @since 0.1.0
instance PUnsafeLiftDecl PTreasuryRedeemer where
  type PLifted PTreasuryRedeemer = TreasuryRedeemer

-- | @since 0.1.0
deriving via
  (DerivePConstantViaData TreasuryRedeemer PTreasuryRedeemer)
  instance
    (PConstantDecl TreasuryRedeemer)

--------------------------------------------------------------------------------

{- | Validator ensuring that transactions consuming the treasury
     do so in a valid manner.

     @since 0.1.0
-}
treasuryValidator ::
  -- | Governance Authority Token that can unlock this validator.
  CurrencySymbol ->
  ClosedTerm PValidator
treasuryValidator gatCs' = plam $ \_datum redeemer ctx' -> unTermCont $ do
  (treasuryRedeemer, _) <- ptryFromC redeemer

  -- plet required fields from script context.
  ctx <- tcont $ pletFields @["txInfo", "purpose"] ctx'

  -- Ensure that script is for burning i.e. minting a negative amount.
  PMinting _ <- pmatchC ctx.purpose

  -- Ensure redeemer type is valid.
  PSpendTreasuryGAT _ <- pmatchC $ pfromData treasuryRedeemer

  -- Get the minted value from txInfo.
  txInfo' <- pletC ctx.txInfo
  txInfo <- tcont $ pletFields @'["mint"] txInfo'
  let mint :: Term _ (PValue _ _)
      mint = txInfo.mint

  gatCs <- pletC $ pconstant gatCs'

  pguardC "A single authority token has been burned" $
    singleAuthorityTokenBurned gatCs txInfo' mint

  pure . popaque $ pconstant ()
