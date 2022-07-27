{-# LANGUAGE QuantifiedConstraints #-}

{- |
Module     : Agora.Utils
Maintainer : emi@haskell.fyi
Description: Plutarch utility functions that should be upstreamed or don't belong anywhere else.

Plutarch utility functions that should be upstreamed or don't belong anywhere else.
-}
module Agora.Utils (
  validatorHashToTokenName,
  pvalidatorHashToTokenName,
  getMintingPolicySymbol,
  mustFindDatum',
  validatorHashToAddress,
  pltAsData,
) where

import Plutarch.Api.V1

import Plutarch.Builtin (pforgetData)
import Plutarch.Extra.List (plookupTuple)
import Plutarch.Extra.Maybe
import Plutarch.Extra.TermCont (ptryFromC)
import PlutusLedgerApi.V1

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
    let dh = passertPDJust # "Given TxOut dones't have a datum" # mdh
        dt = passertPJust # "Datum not found in the transaction" #$ plookupTuple # dh # datums
    (d, _) <- ptryFromC $ pforgetData $ pdata dt
    pure $ pfromData d

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
