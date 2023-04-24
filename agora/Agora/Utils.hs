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
  puncurryTuple,
  psubtractSortedValue,
  pfindInputWithStateThreadToken,
  pfindOutputWithStateThreadToken,
  pisSubValueOf,
) where

import Plutarch.Api.V1 (AmountGuarantees (Positive), KeyGuarantees (Sorted))
import Plutarch.Api.V1.AssocMap (punionWith)
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V1.Scripts (PDatumHash (PDatumHash))
import Plutarch.Api.V2 (
  AmountGuarantees (NoGuarantees),
  PCurrencySymbol,
  PMaybeData (PDNothing),
  PTuple,
  PTxInInfo,
  PTxOut,
  PValue,
 )
import Plutarch.Builtin (pforgetData, pserialiseData)
import Plutarch.Crypto (pblake2b_256)
import Plutarch.DataRepr (punDataSum)
import Plutarch.Extra.AssetClass (PAssetClass, PAssetClassData, ptoScottEncoding)
import Plutarch.Extra.Field (pletAll)
import Plutarch.Extra.Functor (PFunctor (pfmap))
import Plutarch.Extra.Tagged (PTagged)
import Plutarch.Extra.Value (psymbolValueOf)
import Plutarch.Num (PNum (pnegate, (#+)))
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)
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

puncurryTuple ::
  forall (c :: PType) (a :: PType) (b :: PType) (s :: S).
  (PIsData a, PIsData b) =>
  Term s ((a :--> b :--> c) :--> PTuple a b :--> c)
puncurryTuple = phoistAcyclic $
  plam $
    \f ((punDataSum #) -> r) ->
      pletAll r $ \rF -> f # rF._0 # rF._1

psubtractSortedValue ::
  forall (ag :: AmountGuarantees) (s :: S).
  Term
    s
    ( PValue 'Sorted ag
        :--> PValue 'Sorted ag
        :--> PValue 'Sorted 'NoGuarantees
    )
psubtractSortedValue = phoistAcyclic $ plam $ \a b ->
  punsafeCoerce $
    punionWith
      # (punionWith # plam (#+))
      # pto a
      #$ pfmap
      # (pfmap # pnegate)
      # pto b

{- | Find an input containing exactly one token with the given currency symbol

     @since 1.0.0
-}
pfindInputWithStateThreadToken ::
  forall tag.
  ClosedTerm
    ( PTagged tag PCurrencySymbol
        :--> PBuiltinList PTxInInfo
        :--> PMaybe PTxInInfo
    )
pfindInputWithStateThreadToken = plam $ \tokenSymbol inputs ->
  pfind
    # ( plam $ \input ->
          ptaggedSymbolValueOf
            # tokenSymbol
            # (pfield @"value" # (pfield @"resolved" # input))
            #== 1
      )
    # inputs

{- | Find an output containing exactly one token with the given currency symbol,

     @since 1.0.0
-}
pfindOutputWithStateThreadToken ::
  forall tag.
  ClosedTerm
    ( PTagged tag PCurrencySymbol
        :--> PBuiltinList PTxOut
        :--> PMaybe PTxOut
    )
pfindOutputWithStateThreadToken = plam $ \tokenSymbol outputs ->
  pfind
    # ( plam $ \output ->
          ( ptaggedSymbolValueOf
              # tokenSymbol
              # (pfield @"value" # output)
              #== 1
          )
      )
    # outputs

pisNonNegativeValue ::
  forall (kg :: KeyGuarantees) (am :: AmountGuarantees) (s :: S).
  Term s (PValue kg am :--> PBool)
pisNonNegativeValue =
  phoistAcyclic $
    plam $
      (AssocMap.pall # (AssocMap.pall # plam (0 #<=)) #)
        . pto

pisSubValueOf ::
  forall (s :: S).
  Term
    s
    ( PValue 'Sorted 'Positive
        :--> PValue 'Sorted 'Positive
        :--> PBool
    )
pisSubValueOf = phoistAcyclic $ plam $ \vl vr ->
  pisNonNegativeValue
    #$ psubtractSortedValue
    # vl
    # vr
