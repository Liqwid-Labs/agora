{-# OPTIONS_GHC -Wno-orphans #-}

module Agora.Plutarch.Orphans () where

import Plutarch.Api.V1 (PDatumHash)
import Plutarch.Builtin (PIsData (..))

-- TODO: add checks
instance PTryFrom PData (PAsData PDatumHash)

instance PTryFrom PData (PAsData PUnit)

instance (PIsData a) => PIsData (PAsData a) where
  pfromDataImpl = pfromData
  pdataImpl = pdataImpl . pfromData
