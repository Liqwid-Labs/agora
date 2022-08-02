{-# OPTIONS_GHC -Wno-orphans #-}

module Agora.Plutarch.Orphans () where

import Plutarch.Api.V1 (PDatumHash)

instance PTryFrom PData (PAsData PDatumHash)
