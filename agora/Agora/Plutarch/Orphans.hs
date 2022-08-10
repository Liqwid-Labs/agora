{-# OPTIONS_GHC -Wno-orphans #-}

{- FIXME: All of the following instances and
   types ought to belong in either plutarch or
   plutarch-extra.
-}

module Agora.Plutarch.Orphans () where

import Plutarch.Api.V1 (PDatumHash (..))
import Plutarch.Builtin (PIsData (..))
import Plutarch.Extra.TermCont (ptryFromC)
import Plutarch.TryFrom (PTryFrom (..))
import Plutarch.Unsafe (punsafeCoerce)

newtype Flip f a b = Flip (f b a) deriving stock (Generic)

-- | @since 0.1.0
instance PTryFrom PData (PAsData PDatumHash) where
  type PTryFromExcess PData (PAsData PDatumHash) = Flip Term PDatumHash
  ptryFrom' opq = runTermCont $ do
    (pfromData -> unwrapped, _) <- ptryFromC @(PAsData PByteString) opq

    tcont $ \f ->
      pif
        -- Blake2b_256 hash: 256 bits/32 bytes.
        (plengthBS # unwrapped #== 32)
        (f ())
        (ptraceError "ptryFrom(PDatumHash): must be 32 bytes long")

    pure (punsafeCoerce opq, pcon $ PDatumHash unwrapped)

-- | @since 0.2.0
instance PTryFrom PData (PAsData PUnit)

-- | @since 0.2.0
instance (PIsData a) => PIsData (PAsData a) where
  pfromDataImpl = punsafeCoerce
  pdataImpl = pdataImpl . pfromData
