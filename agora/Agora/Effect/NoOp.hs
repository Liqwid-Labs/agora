{- |
Module     : Agora.Effect.NoOp
Maintainer : seungheon.ooh@gmail.com
Description: Dummy dumb dumb effect.

A dumb effect that only burns its GAT.
-}
module Agora.Effect.NoOp (noOpValidator, PNoOp) where

import Control.Applicative (Const)

import Agora.Effect (makeEffect)
import Plutarch (popaque)
import Plutarch.Api.V1 (PValidator)
import Plutarch.TryFrom (PTryFrom (..))
import Plutus.V1.Ledger.Value (CurrencySymbol)

newtype PNoOp (s :: S) = PNoOp (Term s PUnit)
  deriving (PlutusType, PIsData) via (DerivePNewtype PNoOp PUnit)

instance PTryFrom PData PNoOp where
  type PTryFromExcess PData PNoOp = Const ()
  ptryFrom' _ cont =
    -- JUSTIFICATION:
    -- We don't care anything about data.
    -- It should always be reduced to Unit.
    cont (pcon $ PNoOp (pconstant ()), ())

-- | Dummy effect which can only burn its GAT.
noOpValidator :: CurrencySymbol -> ClosedTerm PValidator
noOpValidator curr = makeEffect curr $
  \_ (_datum :: Term s PNoOp) _ _ -> P.do
    popaque (pconstant ())
