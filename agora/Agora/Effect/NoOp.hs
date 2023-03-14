{- |
Module     : Agora.Effect.NoOp
Maintainer : seungheon.ooh@gmail.com
Description: Dummy dumb dumb effect.

A dumb effect that only burns its GAT.
-}
module Agora.Effect.NoOp (noOpValidator, PNoOp) where

import Agora.Effect (makeEffect)
import Agora.SafeMoney (AuthorityTokenTag)
import Plutarch.Api.V1 (PCurrencySymbol)
import Plutarch.Api.V2 (PValidator)
import Plutarch.Extra.Tagged (PTagged)

{- | Dummy datum for NoOp effect.

     @since 0.1.0
-}
newtype PNoOp (s :: S) = PNoOp (Term s PUnit)
  deriving stock
    ( -- | @since 0.2.0
      Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      PlutusType
    , -- | @since 0.1.0
      PIsData
    )

-- | @since 0.2.0
instance DerivePlutusType PNoOp where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 0.2.0
instance PTryFrom PData (PAsData PNoOp)

{- | Dummy effect which can only burn its GAT.

     @since 1.0.0
-}
noOpValidator :: ClosedTerm (PAsData (PTagged AuthorityTokenTag PCurrencySymbol) :--> PValidator)
noOpValidator = plam $
  makeEffect $
    \_ (_datum :: Term s (PAsData PNoOp)) _ _ -> popaque (pconstant ())
