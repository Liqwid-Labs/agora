{-# OPTIONS_GHC -Wno-orphans #-}

{- FIXME: All of the following instances and
   types ought to belong in either plutarch or
   plutarch-extra.

   A number of these have been "stolen" from Mango's
   PR: https://github.com/Plutonomicon/plutarch/pull/438/
-}

module Agora.Plutarch.Orphans () where

import Control.Arrow (first)
import Plutarch.Api.V1 (PAddress, PCredential, PCurrencySymbol, PDatumHash, PMap, PMaybeData, PPOSIXTime, PPubKeyHash, PStakingCredential, PTokenName, PTxId, PTxOutRef, PValidatorHash, PValue)
import Plutarch.Builtin (PBuiltinMap)
import Plutarch.DataRepr (PIsDataReprInstances (..))
import Plutarch.Numeric.Additive (AdditiveSemigroup ((+)))
import Plutarch.Reducible (Reduce, Reducible)
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce)
import Prelude hiding ((+))

instance Reducible (f x y) => Reducible (Flip f y x) where
  type Reduce (Flip f y x) = Reduce (f x y)

newtype Flip f a b = Flip (f b a)

-- | @since 0.1.0
instance PTryFrom PData (PAsData b) => PTryFrom PData (PAsData (DerivePNewtype c b)) where
  type
    PTryFromExcess PData (PAsData (DerivePNewtype c b)) =
      PTryFromExcess PData (PAsData b)
  ptryFrom' d k =
    ptryFrom' @_ @(PAsData b) d $ k . first punsafeCoerce

-- | @since 0.1.0
instance PTryFrom PData (PAsData PPubKeyHash) where
  type PTryFromExcess PData (PAsData PPubKeyHash) = Flip Term PPubKeyHash
  ptryFrom' opq = runTermCont $ do
    (wrapped :: Term _ (PAsData PByteString), unwrapped :: Term _ PByteString) <-
      tcont $ ptryFrom @(PAsData PByteString) opq
    tcont $ \f -> pif (plengthBS # unwrapped #== 28) (f ()) (ptraceError "a PubKeyHash should be 28 bytes long")
    pure (punsafeCoerce wrapped, punsafeCoerce unwrapped)

-- | @since 0.1.0
instance AdditiveSemigroup (Term s PPOSIXTime) where
  (punsafeCoerce @_ @_ @PInteger -> x) + (punsafeCoerce @_ @_ @PInteger -> y) = punsafeCoerce $ x + y

-- | @since 0.1.0
deriving via
  PAsData (DerivePNewtype PPOSIXTime PInteger)
  instance
    PTryFrom PData (PAsData PPOSIXTime)

-- | @since 0.1.0
deriving via
  PAsData (PIsDataReprInstances PTxId)
  instance
    PTryFrom PData (PAsData PTxId)

-- | @since 0.1.0
deriving via
  PAsData (PIsDataReprInstances PTxOutRef)
  instance
    PTryFrom PData (PAsData PTxOutRef)

-- | @since 0.1.0
deriving via
  PAsData (DerivePNewtype (PMap g k v) (PBuiltinMap k v))
  instance
    ( PTryFrom PData (PAsData k)
    , PTryFrom PData (PAsData v)
    ) =>
    PTryFrom PData (PAsData (PMap g k v))

-- | @since 0.1.0
instance PTryFrom PData (PAsData PValidatorHash) where
  type PTryFromExcess PData (PAsData PValidatorHash) = Flip Term PValidatorHash
  ptryFrom' opq = runTermCont $ do
    (wrapped :: Term _ (PAsData PByteString), unwrapped :: Term _ PByteString) <-
      tcont $ ptryFrom @(PAsData PByteString) opq
    tcont $ \f -> pif (plengthBS # unwrapped #== 28) (f ()) (ptraceError "a ValidatorHash should be 28 bytes long")
    pure (punsafeCoerce wrapped, punsafeCoerce unwrapped)

-- | @since 0.1.0
instance PTryFrom PData (PAsData PDatumHash) where
  type PTryFromExcess PData (PAsData PDatumHash) = Flip Term PDatumHash
  ptryFrom' opq = runTermCont $ do
    (wrapped :: Term _ (PAsData PByteString), unwrapped :: Term _ PByteString) <-
      tcont $ ptryFrom @(PAsData PByteString) opq
    tcont $ \f -> pif (plengthBS # unwrapped #== 32) (f ()) (ptraceError "a DatumHash should be 32 bytes long")
    pure (punsafeCoerce wrapped, punsafeCoerce unwrapped)

-- | @since 0.1.0
deriving via
  PAsData (DerivePNewtype PCurrencySymbol PByteString)
  instance
    PTryFrom PData (PAsData PCurrencySymbol)

-- | @since 0.1.0
deriving via
  PAsData (DerivePNewtype PTokenName PByteString)
  instance
    PTryFrom PData (PAsData PTokenName)

-- | @since 0.1.0
deriving via
  PAsData (DerivePNewtype (PValue k v) (PMap k PCurrencySymbol (PMap k PTokenName PInteger)))
  instance
    PTryFrom PData (PAsData (PValue k v))

-- | @since 0.1.0
deriving via
  PAsData (PIsDataReprInstances (PMaybeData a))
  instance
    PTryFrom PData (PAsData a) => PTryFrom PData (PAsData (PMaybeData a))

-- | @since 0.1.0
deriving via
  PAsData (PIsDataReprInstances PAddress)
  instance
    PTryFrom PData (PAsData PAddress)

-- | @since 0.1.0
deriving via
  PAsData (PIsDataReprInstances PCredential)
  instance
    PTryFrom PData (PAsData PCredential)

-- | @since 0.1.0
deriving via
  PAsData (PIsDataReprInstances PStakingCredential)
  instance
    PTryFrom PData (PAsData PStakingCredential)
