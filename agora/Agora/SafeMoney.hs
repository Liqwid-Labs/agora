{- |
Module     : Agora.SafeMoney
Maintainer : emi@haskell.fyi
Description: Phantom-type protected types for handling money in Plutus.

Phantom-type protected types for handling money in Plutus.
-}
module Agora.SafeMoney (
  -- * Types
  PDiscrete,

  -- * Tags and refs
  AssetClassRef (..),
  ADATag,
  GTTag,
  adaRef,

  -- * Utility functions
  paddDiscrete,
  pgeqDiscrete,
  pzeroDiscrete,

  -- * Conversions
  pdiscreteValue,
  pvalueDiscrete,
) where

import Prelude

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Api (BuiltinData (..), Data (..))
import Plutus.V1.Ledger.Value (AssetClass (AssetClass))
import PlutusTx.IsData.Class (FromData (..), ToData (..))

import Plutarch.Api.V1 (PValue)
import Plutarch.Builtin ()
import Plutarch.Internal ()
import Plutarch.Monadic qualified as P

--------------------------------------------------------------------------------

import Agora.Utils (
  passetClassValueOf',
  psingletonValue,
 )

--------------------------------------------------------------------------------
-- Example tags

-- | Governance token
data GTTag

-- | ADA
data ADATag

--------------------------------------------------------------------------------

-- | A tagged AssetClass. Use to resolve a reference inside of a PDiscrete
newtype AssetClassRef (tag :: Type) = AssetClassRef {getAssetClass :: AssetClass}

-- | Resolves ada tags.
adaRef :: AssetClassRef ADATag
adaRef = AssetClassRef (AssetClass ("", ""))

{- | Represents a single asset in a 'Value' related to a particular 'AssetClass'
     through 'AssetClassRef'.
-}
newtype Discrete (tag :: Type)
  = Discrete Integer
  deriving stock (Show, Eq)

{- We have to manually write these instances because the `tag` will confuse
   `makeIsDataIndexed`.
-}
instance forall tag. FromData (Discrete tag) where
  fromBuiltinData (BuiltinData (I x)) = Just (Discrete x)
  fromBuiltinData _ = Nothing

instance forall tag. ToData (Discrete tag) where
  toBuiltinData (Discrete x) = BuiltinData (I x)

{- | Represents a single asset in a 'PValue' related to a particular 'AssetClass'
     through 'AssetClassRef'.
-}
newtype PDiscrete (tag :: Type) (s :: S)
  = PDiscrete (Term s PInteger)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype (PDiscrete tag) PInteger)

-- | Check if one 'PDiscrete' is greater than another.
pgeqDiscrete :: forall (tag :: Type) (s :: S). Term s (PDiscrete tag :--> PDiscrete tag :--> PBool)
pgeqDiscrete = phoistAcyclic $
  plam $ \x y -> P.do
    PDiscrete x' <- pmatch x
    PDiscrete y' <- pmatch y
    y' #<= x'

-- | Returns a zero-value 'PDiscrete' unit for any tag.
pzeroDiscrete :: forall (tag :: Type) (s :: S). Term s (PDiscrete tag)
pzeroDiscrete = phoistAcyclic $ pcon (PDiscrete 0)

-- | Add two 'PDiscrete' values of the same tag.
paddDiscrete :: forall (tag :: Type) (s :: S). Term s (PDiscrete tag :--> PDiscrete tag :--> PDiscrete tag)
paddDiscrete = phoistAcyclic $
  -- In the future, this should use plutarch-numeric
  plam $ \x y -> P.do
    PDiscrete x' <- pmatch x
    PDiscrete y' <- pmatch y
    pcon (PDiscrete $ x' + y')

--------------------------------------------------------------------------------

-- | Downcast a `PValue` to a `PDiscrete` unit.
pvalueDiscrete ::
  forall (tag :: Type) (s :: S).
  AssetClassRef tag ->
  Term s (PValue :--> PDiscrete tag)
pvalueDiscrete (AssetClassRef ac) = phoistAcyclic $
  plam $ \f ->
    pcon . PDiscrete $ passetClassValueOf' ac # f

{- | Get a `PValue` from a `PDiscrete`.
     __NOTE__: `pdiscreteValue` after `pvalueDiscrete` is not a round-trip.
     It filters for a particular tag.
-}
pdiscreteValue ::
  forall (tag :: Type) (s :: S).
  AssetClassRef tag ->
  Term s (PDiscrete tag :--> PValue)
pdiscreteValue (AssetClassRef (AssetClass (cs, tn))) = phoistAcyclic $
  plam $ \f -> pmatch f $ \case
    PDiscrete p ->
      psingletonValue
        # pconstant cs
        # pconstant tn
        # p
