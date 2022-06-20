{- |
Module     : Test.Util
Maintainer : emi@haskell.fyi
Description: Utility functions for testing Plutarch scripts with ScriptContext
-}
module Test.Util (
  -- * Plutus-land utils
  datumHash,
  toDatum,
  toDatumHash,
  datumPair,
  closedBoundedInterval,
  updateMap,
  sortMap,
  sortValue,
) where

--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as ByteString.Lazy

--------------------------------------------------------------------------------

import Data.Bifunctor (second)
import Data.List (sortOn)
import Plutarch.Crypto (pblake2b_256)
import PlutusLedgerApi.V1.Interval qualified as PlutusTx
import PlutusLedgerApi.V1.Scripts (Datum (Datum), DatumHash (DatumHash))
import PlutusLedgerApi.V1.Value (Value (..))
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as PlutusTx
import PlutusTx.IsData qualified as PlutusTx
import PlutusTx.Ord qualified as PlutusTx

--------------------------------------------------------------------------------

{- | Create a pair from data for use in 'PlutusLedgerApi.V1.Contexts.txInfoData'.

   Example:
   @
     myTxInfo { 'PlutusLedgerApi.V1.Contexts.txInfoData' = ['datumPair' myDatum] }
   @
-}
datumPair :: PlutusTx.ToData a => a -> (DatumHash, Datum)
datumPair = (,) <$> toDatumHash <*> toDatum

-- | Calculate the blake2b-256 hash of a Datum.
datumHash :: Datum -> DatumHash
datumHash (Datum data') = toDatumHash data'

-- | Convenience function to create a Datum from any type that implements ToData.
toDatum :: PlutusTx.ToData a => a -> Datum
toDatum = Datum . PlutusTx.toBuiltinData

{- | Calculate the blake2b-256 hash of any type that implements ToData

     Shamelessly go through plutus.
-}
toDatumHash :: PlutusTx.ToData a => a -> DatumHash
toDatumHash datum =
  DatumHash $
    PlutusTx.toBuiltin $
      plift $
        pblake2b_256
          # pconstant (ByteString.Lazy.toStrict $ serialise $ PlutusTx.toData datum)

--------------------------------------------------------------------------------

-- | Create a closed bounded `Interval`.
closedBoundedInterval :: PlutusTx.Ord a => a -> a -> PlutusTx.Interval a
closedBoundedInterval from to = PlutusTx.intersection (PlutusTx.from from) (PlutusTx.to to)

--------------------------------------------------------------------------------

{- | / O(n) /. The expression @'updateMap' f k v@ will update the value @x@ at key @k@.
    If @f x@ is Nothing, the key-value pair will be deleted from the map, otherwise the
     value will be updated.
-}
updateMap :: Eq k => (v -> Maybe v) -> k -> AssocMap.Map k v -> AssocMap.Map k v
updateMap f k =
  AssocMap.mapMaybeWithKey
    ( \k' v ->
        if k' == k
          then f v
          else Just v
    )

--------------------------------------------------------------------------------

sortMap :: forall k v. Ord k => AssocMap.Map k v -> AssocMap.Map k v
sortMap =
  AssocMap.fromList
    . sortOn fst
    . AssocMap.toList

sortValue :: Value -> Value
sortValue =
  Value
    . sortMap
    . AssocMap.fromList
    . fmap (second sortMap)
    . AssocMap.toList
    . getValue
