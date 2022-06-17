{- |
Module     : Agora.Utils
Maintainer : emi@haskell.fyi
Description: Plutarch utility functions that should be upstreamed or don't belong anywhere else.

Plutarch utility functions that should be upstreamed or don't belong anywhere else.
-}
module Agora.Utils (
  findTxOutByTxOutRef,
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
  psingletonValue,
) where

--------------------------------------------------------------------------------

import PlutusLedgerApi.V1 (
  Address (..),
  Credential (..),
  CurrencySymbol,
  TokenName (..),
  ValidatorHash (..),
 )

--------------------------------------------------------------------------------

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
  PTxInInfo,
  PTxOut,
  PTxOutRef,
  PValidatorHash,
  PValue,
  mintingPolicySymbol,
  mkMintingPolicy,
 )
import Plutarch.Api.V1.AssocMap (PMap (PMap))
import Plutarch.Api.V1.ScriptContext (pfindDatum, pfindTxInByTxOutRef)
import "liqwid-plutarch-extra" Plutarch.Api.V1.Value (psymbolValueOf)
import "plutarch" Plutarch.Api.V1.Value (PValue (PValue))
import Plutarch.Builtin (pforgetData, ppairDataBuiltin)
import Plutarch.Extra.List (plookupTuple)
import Plutarch.Extra.TermCont (pletC, pmatchC)

{- Functions which should (probably) not be upstreamed
   All of these functions are quite inefficient.
-}

-- | Create a value with a single asset class.
psingletonValue ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PInteger :--> PValue keys amounts)
psingletonValue = phoistAcyclic $
  plam $ \sym tok int ->
    let innerTup = pcon $ PMap $ psingleton #$ ppairDataBuiltin # pdata tok # pdata int
        outerTup = pcon $ PMap $ psingleton #$ ppairDataBuiltin # pdata sym # pdata innerTup
        res = pcon $ PValue outerTup
     in res

-- | Finds the TxOut of an effect from TxInfo and TxOutRef
findTxOutByTxOutRef :: Term s (PTxOutRef :--> PBuiltinList (PAsData PTxInInfo) :--> PMaybe PTxOut)
findTxOutByTxOutRef = phoistAcyclic $
  plam $ \txOutRef inputs ->
    pmatch (pfindTxInByTxOutRef # txOutRef # inputs) $ \case
      PJust ((pfield @"resolved" #) -> txOut) -> pcon $ PJust txOut
      PNothing -> pcon PNothing

-- | Get script hash from an Address.
scriptHashFromAddress :: Term s (PAddress :--> PMaybe PValidatorHash)
scriptHashFromAddress = phoistAcyclic $
  plam $ \addr ->
    pmatch (pfromData $ pfield @"credential" # addr) $ \case
      PScriptCredential ((pfield @"_0" #) -> h) -> pcon $ PJust h
      _ -> pcon PNothing

-- | Return true if the given address is a script address.
isScriptAddress :: Term s (PAddress :--> PBool)
isScriptAddress = phoistAcyclic $
  plam $ \addr -> pnot #$ isPubKey #$ pfromData $ pfield @"credential" # addr

-- | Return true if the given credential is a pub-key-hash.
isPubKey :: Term s (PCredential :--> PBool)
isPubKey = phoistAcyclic $
  plam $ \cred ->
    pmatch cred $ \case
      PScriptCredential _ -> pconstant False
      _ -> pconstant True

-- | Find all TxOuts sent to an Address
findOutputsToAddress :: Term s (PBuiltinList (PAsData PTxOut) :--> PAddress :--> PBuiltinList (PAsData PTxOut))
findOutputsToAddress = phoistAcyclic $
  plam $ \outputs address' -> unTermCont $ do
    address <- pletC $ pdata address'
    pure $
      pfilter # plam (\(pfromData -> txOut) -> pfield @"address" # txOut #== address)
        # outputs

-- | Find the data corresponding to a TxOut, if there is one
findTxOutDatum :: Term s (PBuiltinList (PAsData (PTuple PDatumHash PDatum)) :--> PTxOut :--> PMaybe PDatum)
findTxOutDatum = phoistAcyclic $
  plam $ \datums out -> unTermCont $ do
    datumHash' <- pmatchC $ pfromData $ pfield @"datumHash" # out
    pure $ case datumHash' of
      PDJust ((pfield @"_0" #) -> datumHash) -> pfindDatum # datumHash # datums
      _ -> pcon PNothing

{- | Safely convert a 'PValidatorHash' into a 'PTokenName'. This can be useful for tagging
     tokens for extra safety.
-}
validatorHashToTokenName :: ValidatorHash -> TokenName
validatorHashToTokenName (ValidatorHash hash) = TokenName hash

-- | Plutarch level 'validatorHashToTokenName'.
pvalidatorHashToTokenName :: forall (s :: S). Term s PValidatorHash -> Term s PTokenName
pvalidatorHashToTokenName vh = pcon (PTokenName (pto vh))

-- | Get the CurrencySymbol of a PMintingPolicy.
getMintingPolicySymbol :: ClosedTerm PMintingPolicy -> CurrencySymbol
getMintingPolicySymbol v = mintingPolicySymbol $ mkMintingPolicy v

-- | The entire value only contains one token of the given currency symbol.
hasOnlyOneTokenOfCurrencySymbol ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term s (PCurrencySymbol :--> PValue keys amounts :--> PBool)
hasOnlyOneTokenOfCurrencySymbol = phoistAcyclic $
  plam $ \cs vs -> P.do
    psymbolValueOf # cs # vs #== 1
      #&& (plength #$ pto $ pto $ pto vs) #== 1

-- | Find datum given a maybe datum hash
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
    (d, _) <- tcont $ ptryFrom $ pforgetData $ pdata dt
    pure $ pfromData d

{- | Extract the value stored in a PMaybe container.
     If there's no value, throw an error with the given message.
-}
mustBePJust :: forall a s. Term s (PString :--> PMaybe a :--> a)
mustBePJust = phoistAcyclic $
  plam $ \emsg mv' -> pmatch mv' $ \case
    PJust v -> v
    _ -> ptraceError emsg

{- | Extract the value stored in a PMaybeData container.
     If there's no value, throw an error with the given message.
-}
mustBePDJust :: forall a s. (PIsData a) => Term s (PString :--> PMaybeData a :--> a)
mustBePDJust = phoistAcyclic $
  plam $ \emsg mv' -> pmatch mv' $ \case
    PDJust ((pfield @"_0" #) -> v) -> v
    _ -> ptraceError emsg

-- | Create an 'Address' from a given 'ValidatorHash' with no 'PlutusLedgerApi.V1.Credential.StakingCredential'.
validatorHashToAddress :: ValidatorHash -> Address
validatorHashToAddress vh = Address (ScriptCredential vh) Nothing
