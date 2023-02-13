{-# LANGUAGE QuantifiedConstraints #-}

{- |
Module     : Agora.Utils
Maintainer : emi@haskell.fyi
Description: Plutarch utility functions that should be upstreamed or don't belong anywhere else.

Plutarch utility functions that should be upstreamed or don't belong anywhere else.
-}
module Agora.Utils (
  scriptHashToAddress,
  pstringIntercalate,
  punwords,
  pisNothing,
  pisDNothing,
  ptoScottEncodingT,
  ptaggedSymbolValueOf,
  ptag,
  puntag,
  phashDatum,
  phashDatum',
) where

import Plutarch.Api.V1.Scripts (PDatumHash (PDatumHash), PDatum)
import Plutarch.Api.V2 (
  AmountGuarantees,
  KeyGuarantees,
  PCurrencySymbol,
  PMaybeData (PDNothing),
  PValue,
 )
import Plutarch.Builtin (pforgetData, pserialiseData)
import Plutarch.Crypto (pblake2b_256)
import Plutarch.Extra.AssetClass (PAssetClass, PAssetClassData, ptoScottEncoding)
import Plutarch.Extra.Tagged (PTagged)
import Plutarch.Extra.Value (psymbolValueOf)
import Plutarch.Unsafe (punsafeDowncast)
import PlutusLedgerApi.V2 (
  Address (Address),
  Credential (ScriptCredential),
  ScriptHash,
 )

{- | Create an 'Address' from a given 'ScriptHash' with no 'PlutusLedgerApi.V1.Credential.StakingCredential'.

     @since 1.0.0
-}
scriptHashToAddress :: ScriptHash -> Address
scriptHashToAddress vh = Address (ScriptCredential vh) Nothing

-- | @since 1.0.0
pstringIntercalate ::
  forall (s :: S).
  Term s PString ->
  [Term s PString] ->
  Term s PString
pstringIntercalate _ [x] = x
pstringIntercalate i (x : xs) = x <> i <> pstringIntercalate i xs
pstringIntercalate _ _ = ""

-- | @since 1.0.0
punwords ::
  forall (s :: S).
  [Term s PString] ->
  Term s PString
punwords = pstringIntercalate " "

-- | @since 1.0.0
pisNothing ::
  forall (a :: PType) (s :: S).
  Term s (PMaybe a :--> PBool)
pisNothing = phoistAcyclic $
  plam $
    flip pmatch $ \case
      PNothing -> pconstant True
      _ -> pconstant False

-- | @since 1.0.0
pisDNothing ::
  forall (a :: PType) (s :: S).
  Term s (PMaybeData a :--> PBool)
pisDNothing = phoistAcyclic $
  plam $
    flip pmatch $ \case
      PDNothing _ -> pconstant True
      _ -> pconstant False

-- | @since 1.0.0
ptoScottEncodingT ::
  forall {k :: Type} (unit :: k) (s :: S).
  Term s (PTagged unit PAssetClassData :--> PTagged unit PAssetClass)
ptoScottEncodingT = phoistAcyclic $
  plam $ \d ->
    punsafeDowncast $ ptoScottEncoding #$ pto d

{- | Get the sum of all values belonging to a particular tagged 'CurrencySymbol'.

     @since 1.0.0
-}
ptaggedSymbolValueOf ::
  forall
    {k :: Type}
    (unit :: k)
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PTagged unit PCurrencySymbol :--> (PValue keys amounts :--> PInteger))
ptaggedSymbolValueOf = phoistAcyclic $ plam $ \tcs -> psymbolValueOf # pto tcs

-- | @since 1.0.0
ptag ::
  forall {k :: Type} (tag :: k) (a :: PType) (s :: S).
  Term s a ->
  Term s (PTagged tag a)
ptag = punsafeDowncast

-- | @since 1.0.0
puntag ::
  forall {k :: Type} (tag :: k) (a :: PType) (s :: S).
  Term s (PTagged tag a) ->
  Term s a
puntag = pto

{- | Hash the given datum using the correct algorithm(blake2b_256).

     Note: check the discussion here: https://github.com/input-output-hk/cardano-ledger/issues/2941.

     @since 1.0.0
-}
phashDatum ::
  forall (a :: PType) (s :: S).
  PIsData a =>
  Term s (a :--> PDatumHash)
phashDatum =
  phoistAcyclic $
    plam $
      pcon
        . PDatumHash
        . (pblake2b_256 #)
        . (pserialiseData #)
        . pforgetData
        . pdata

phashDatum' ::
  forall (s :: S).
  Term s (PDatum :--> PDatumHash)
phashDatum' =
  phoistAcyclic $
    plam $
      pcon
        . PDatumHash
        . (pblake2b_256 #)
        . (pserialiseData #)
        . pto