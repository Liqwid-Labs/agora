{-# LANGUAGE QuantifiedConstraints #-}

{- |
Module     : Agora.Utils
Maintainer : emi@haskell.fyi
Description: Plutarch utility functions that should be upstreamed or don't belong anywhere else.

Plutarch utility functions that should be upstreamed or don't belong anywhere else.
-}
module Agora.Utils (
  scriptHashFromAddress,
  findOutputsToAddress,
  findTxOutDatum,
  validatorHashToTokenName,
  pvalidatorHashToTokenName,
  getMintingPolicySymbol,
  hasOnlyOneTokenOfCurrencySymbol,
  mustFindDatum',
  mustBePJust,
  mustBePDJust,
  validatorHashToAddress,
  isScriptAddress,
  isPubKey,
  pltAsData,
  pon,
  withBuiltinPairAsData,
) where

import Plutarch.Api.V1 (
  AmountGuarantees,
  KeyGuarantees,
  PAddress,
  PCredential (PScriptCredential),
  PCurrencySymbol,
  PDatum,
  PDatumHash,
  PMaybeData (PDJust),
  PMintingPolicy,
  PTokenName (PTokenName),
  PTuple,
  PTxOut,
  PValidatorHash,
  PValue,
  mintingPolicySymbol,
  mkMintingPolicy,
 )
import Plutarch.Api.V1.ScriptContext (pfindDatum)
import "liqwid-plutarch-extra" Plutarch.Api.V1.Value (psymbolValueOf)
import Plutarch.Builtin (pforgetData)
import Plutarch.Extra.List (plookupTuple)
import Plutarch.Extra.TermCont (pletC, pmatchC, ptryFromC)
import PlutusLedgerApi.V1 (
  Address (..),
  Credential (..),
  CurrencySymbol,
  TokenName (..),
  ValidatorHash (..),
 )

{- Functions which should (probably) not be upstreamed
   All of these functions are quite inefficient.
-}

{- | Get script hash from an Address.

     @since 0.1.0
-}
scriptHashFromAddress :: Term s (PAddress :--> PMaybe PValidatorHash)
scriptHashFromAddress = phoistAcyclic $
  plam $ \addr ->
    pmatch (pfromData $ pfield @"credential" # addr) $ \case
      PScriptCredential ((pfield @"_0" #) -> h) -> pcon $ PJust h
      _ -> pcon PNothing

{- | Return true if the given address is a script address.

     @since 0.1.0
-}
isScriptAddress :: Term s (PAddress :--> PBool)
isScriptAddress = phoistAcyclic $
  plam $ \addr -> pnot #$ isPubKey #$ pfromData $ pfield @"credential" # addr

{- | Return true if the given credential is a pub-key-hash.

     @since 0.1.0
-}
isPubKey :: Term s (PCredential :--> PBool)
isPubKey = phoistAcyclic $
  plam $ \cred ->
    pmatch cred $ \case
      PScriptCredential _ -> pconstant False
      _ -> pconstant True

{- | Find all TxOuts sent to an Address

     @since 0.1.0
-}
findOutputsToAddress :: Term s (PBuiltinList (PAsData PTxOut) :--> PAddress :--> PBuiltinList (PAsData PTxOut))
findOutputsToAddress = phoistAcyclic $
  plam $ \outputs address' -> unTermCont $ do
    address <- pletC $ pdata address'
    pure $
      pfilter # plam (\(pfromData -> txOut) -> pfield @"address" # txOut #== address)
        # outputs

{- | Find the data corresponding to a TxOut, if there is one

     @since 0.1.0
-}
findTxOutDatum :: Term s (PBuiltinList (PAsData (PTuple PDatumHash PDatum)) :--> PTxOut :--> PMaybe PDatum)
findTxOutDatum = phoistAcyclic $
  plam $ \datums out -> unTermCont $ do
    datumHash' <- pmatchC $ pfromData $ pfield @"datumHash" # out
    pure $ case datumHash' of
      PDJust ((pfield @"_0" #) -> datumHash) -> pfindDatum # datumHash # datums
      _ -> pcon PNothing

{- | Safely convert a 'PValidatorHash' into a 'PTokenName'. This can be useful for tagging
     tokens for extra safety.

     @since 0.1.0
-}
validatorHashToTokenName :: ValidatorHash -> TokenName
validatorHashToTokenName (ValidatorHash hash) = TokenName hash

{- | Plutarch level 'validatorHashToTokenName'.

     @since 0.1.0
-}
pvalidatorHashToTokenName :: forall (s :: S). Term s PValidatorHash -> Term s PTokenName
pvalidatorHashToTokenName vh = pcon (PTokenName (pto vh))

{- | Get the CurrencySymbol of a PMintingPolicy.

     @since 0.1.0
-}
getMintingPolicySymbol :: ClosedTerm PMintingPolicy -> CurrencySymbol
getMintingPolicySymbol v = mintingPolicySymbol $ mkMintingPolicy v

{- | The entire value only contains one token of the given currency symbol.

     @since 0.1.0
-}
hasOnlyOneTokenOfCurrencySymbol ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term s (PCurrencySymbol :--> PValue keys amounts :--> PBool)
hasOnlyOneTokenOfCurrencySymbol = phoistAcyclic $
  plam $ \cs vs -> P.do
    psymbolValueOf # cs # vs #== 1
      #&& (plength #$ pto $ pto $ pto vs) #== 1

{- | Find datum given a maybe datum hash

     @since 0.1.0
-}
mustFindDatum' ::
  forall (datum :: PType).
  (PIsData datum, PTryFrom PData (PAsData datum)) =>
  forall s.
  Term
    s
    ( PMaybeData PDatumHash
        :--> PBuiltinList (PAsData (PTuple PDatumHash PDatum))
        :--> datum
    )
mustFindDatum' = phoistAcyclic $
  plam $ \mdh datums -> unTermCont $ do
    let dh = mustBePDJust # "Given TxOut dones't have a datum" # mdh
        dt = mustBePJust # "Datum not found in the transaction" #$ plookupTuple # dh # datums
    (d, _) <- ptryFromC $ pforgetData $ pdata dt
    pure $ pfromData d

{- | Extract the value stored in a PMaybe container.
     If there's no value, throw an error with the given message.

     @since 0.1.0
-}
mustBePJust :: forall a s. Term s (PString :--> PMaybe a :--> a)
mustBePJust = phoistAcyclic $
  plam $ \emsg mv' -> pmatch mv' $ \case
    PJust v -> v
    _ -> ptraceError emsg

{- | Extract the value stored in a PMaybeData container.
     If there's no value, throw an error with the given message.

     @since 0.1.0
-}
mustBePDJust :: forall a s. (PIsData a) => Term s (PString :--> PMaybeData a :--> a)
mustBePDJust = phoistAcyclic $
  plam $ \emsg mv' -> pmatch mv' $ \case
    PDJust ((pfield @"_0" #) -> v) -> v
    _ -> ptraceError emsg

{- | Create an 'Address' from a given 'ValidatorHash' with no 'PlutusLedgerApi.V1.Credential.StakingCredential'.

     @since 0.1.0
-}
validatorHashToAddress :: ValidatorHash -> Address
validatorHashToAddress vh = Address (ScriptCredential vh) Nothing

{- | Compare two 'PAsData' value, return true if the first one is less than the second one.

 @since 0.2.0
-}
pltAsData ::
  forall (a :: PType) (s :: S).
  (POrd a, PIsData a) =>
  Term s (PAsData a :--> PAsData a :--> PBool)
pltAsData = phoistAcyclic $
  plam $
    \(pfromData -> l) (pfromData -> r) -> l #< r

{- | Plutarch level 'Data.Function.on'.

     @since 0.2.0
-}
pon ::
  forall (a :: PType) (b :: PType) (c :: PType) (s :: S).
  Term s ((b :--> b :--> c) :--> (a :--> b) :--> a :--> a :--> c)
pon = phoistAcyclic $
  plam $ \f g x y ->
    let a = g # x
        b = g # y
     in f # a # b

{- | Extract data stored in a 'PBuiltinPair' and call a function to process it.

     @since 0.2.0
-}
withBuiltinPairAsData ::
  forall (a :: PType) (b :: PType) (c :: PType) (s :: S).
  (PIsData a, PIsData b) =>
  (Term s a -> Term s b -> Term s c) ->
  Term
    s
    (PBuiltinPair (PAsData a) (PAsData b)) ->
  Term s c
withBuiltinPairAsData f p =
  let a = pfromData $ pfstBuiltin # p
      b = pfromData $ psndBuiltin # p
   in f a b
