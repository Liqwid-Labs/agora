{- |
Module     : Agora.Effect.NoOp
Maintainer : seungheon.ooh@gmail.com
Description: Dummy dumb dumb effect.

A dumb effect that only burns its GAT.
-}
module Agora.Effect.NoOp (noOpValidator, PNoOp) where

import Control.Applicative (Const)

import Agora.Effect (makeEffect)
import Plutarch.Api.V1 (PValidator)
import Plutarch.TryFrom (PTryFrom (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol)

{- | Dummy datum for NoOp effect.

     @since 0.1.0
-}
newtype PNoOp (s :: S) = PNoOp (Term s PUnit)
  deriving
    ( -- | @since 0.1.0
      PlutusType
    , -- | @since 0.1.0
      PIsData
    )
    via (DerivePNewtype PNoOp PUnit)

-- | @since 0.1.0
instance PTryFrom PData (PAsData PNoOp) where
  type PTryFromExcess PData (PAsData PNoOp) = Const ()
  ptryFrom' _ cont =
    -- JUSTIFICATION:
    -- We don't care anything about data.
    -- It should always be reduced to Unit.
    cont (pdata $ pcon $ PNoOp (pconstant ()), ())

{- | Dummy effect which can only burn its GAT.

     @since 0.1.0
-}
noOpValidator :: CurrencySymbol -> ClosedTerm PValidator
noOpValidator curr = makeEffect curr $
  \_ (_datum :: Term s PNoOp) _ _ -> popaque (pconstant ())
