{-# OPTIONS_GHC -Wno-orphans #-}

module Agora.Plutarch.Orphans () where

import Plutarch.Lift (PConstantDecl (..), PUnsafeLiftDecl (PLifted))

import Data.Bifunctor (Bifunctor (bimap))
import Data.Map.Strict qualified as StrictMap
import Data.Tagged (Tagged (Tagged))
import Data.Traversable (for)
import Plutarch.Api.V1 (KeyGuarantees (Sorted), PMap)
import Plutarch.Extra.Tagged (PTagged)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import Ply (PlyArg)
import Ply.Plutarch.Class (PlyArgOf)

-- | @since 1.0.0
instance
  ( PConstantData k
  , PConstantData v
  , Ord k
  ) =>
  PConstantDecl (StrictMap.Map k v)
  where
  type
    PConstantRepr (StrictMap.Map k v) =
      [(PlutusTx.Data, PlutusTx.Data)]
  type
    PConstanted (StrictMap.Map k v) =
      PMap 'Sorted (PConstanted k) (PConstanted v)
  pconstantToRepr m =
    bimap
      PlutusTx.toData
      PlutusTx.toData
      <$> StrictMap.toList m
  pconstantFromRepr m = fmap StrictMap.fromList $
    for m $ \(x, y) -> do
      x' <- PlutusTx.fromData x
      y' <- PlutusTx.fromData y
      Just (x', y')

-- | @since 1.0.0
instance
  ( PLiftData k
  , PLiftData v
  , Ord (PLifted k)
  ) =>
  PUnsafeLiftDecl (PMap 'Sorted k v)
  where
  type PLifted (PMap 'Sorted k v) = StrictMap.Map (PLifted k) (PLifted v)

-- | @since 1.0.0
instance
  (PlutusTx.ToData k, PlutusTx.ToData v) =>
  PlutusTx.ToData (StrictMap.Map k v)
  where
  toBuiltinData = PlutusTx.toBuiltinData . toAssocMap
    where
      toAssocMap :: StrictMap.Map k v -> AssocMap.Map k v
      toAssocMap = AssocMap.fromList . StrictMap.toAscList

-- | @since 1.0.0
instance
  (PlutusTx.FromData k, PlutusTx.FromData v, Ord k) =>
  PlutusTx.FromData (StrictMap.Map k v)
  where
  fromBuiltinData d = PlutusTx.fromBuiltinData d >>= toStrictMap
    where
      toStrictMap :: AssocMap.Map k v -> Maybe (StrictMap.Map k v)
      toStrictMap m =
        let l = AssocMap.toList m
         in if isSorted $ fmap fst l
              then Just $ StrictMap.fromAscList l
              else Nothing

      isSorted :: forall a. Ord a => [a] -> Bool
      isSorted [] = True
      isSorted [_] = True
      isSorted (x : y : xs) = x < y && isSorted (y : xs)

-- | @since 1.0.0
type instance PlyArgOf (PTagged tag a) = Tagged tag (PlyArgOf a)

-- | @since 1.0.0
deriving newtype instance PlyArg a => PlyArg (Tagged tag a)
