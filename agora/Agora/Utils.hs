{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- |
Module     : Agora.Utils
Maintainer : emi@haskell.fyi
Description: Plutarch utility functions that should be upstreamed or don't belong anywhere else.

Plutarch utility functions that should be upstreamed or don't belong anywhere else.
-}
module Agora.Utils (
  validatorHashToTokenName,
  validatorHashToAddress,
  pltAsData,
  withBuiltinPairAsData,
  CompiledValidator (..),
  CompiledMintingPolicy (..),
  CompiledEffect (..),
  presolveOutputDatum,
  pfindDatum,
  pmustFindDatum,
  (#.*),
  (#.**),
  pfromDatumHash,
  pfromInlineDatum,
  ptryFindDatum,
) where

import Plutarch.Api.V1.AssocMap (KeyGuarantees (Unsorted), PMap)
import Plutarch.Api.V1.AssocMap qualified as PAssocMap
import Plutarch.Api.V2 (
  PDatum,
  PDatumHash,
  POutputDatum (..),
 )
import Plutarch.Extra.Functor (pfmap)
import Plutarch.Extra.Maybe (passertPJust, pjust, pnothing)
import PlutusLedgerApi.V2 (
  Address (..),
  Credential (..),
  MintingPolicy,
  TokenName (..),
  Validator,
  ValidatorHash (..),
 )

{- Functions which should (probably) not be upstreamed
   All of these functions are quite inefficient.
-}

{- | Safely convert a 'PValidatorHash' into a 'PTokenName'. This can be useful for tagging
     tokens for extra safety.

     @since 0.1.0
-}
validatorHashToTokenName :: ValidatorHash -> TokenName
validatorHashToTokenName (ValidatorHash hash) = TokenName hash

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

{- | Type-safe wrapper for compiled plutus validator.

     @since 0.2.0
-}
newtype CompiledValidator (datum :: Type) (redeemer :: Type) = CompiledValidator
  { getCompiledValidator :: Validator
  }

{- | Type-safe wrapper for compiled plutus miting policy.

     @since 0.2.0
-}
newtype CompiledMintingPolicy (redeemer :: Type) = CompiledMintingPolicy
  { getCompiledMintingPolicy :: MintingPolicy
  }

{- | Type-safe wrapper for compiled plutus effect.

     @since 0.2.0
-}
newtype CompiledEffect (datum :: Type) = CompiledEffect
  { getCompiledEffect :: Validator
  }

-- @since 0.3.0
presolveOutputDatum ::
  forall s.
  Term
    s
    ( POutputDatum
        :--> PMap 'Unsorted PDatumHash PDatum
        :--> PMaybe PDatum
    )
presolveOutputDatum = phoistAcyclic $
  plam $ \od m -> pmatch od $ \case
    PNoOutputDatum _ ->
      ptrace "no datum" pnothing
    POutputDatum ((pfield @"outputDatum" #) -> datum) ->
      ptrace "datum hash" pjust # datum
    POutputDatumHash ((pfield @"datumHash" #) -> hash) ->
      PAssocMap.plookup
        # hash
        # m

-- | @since 0.3.0
pfindDatum ::
  forall datum s.
  PTryFrom PData datum =>
  Term
    s
    ( POutputDatum
        :--> PMap 'Unsorted PDatumHash PDatum
        :--> PMaybe datum
    )
pfindDatum = phoistAcyclic $
  plam $ \od m ->
    pfmap
      # phoistAcyclic (plam $ flip ptryFrom fst . pto)
      # (presolveOutputDatum # od # m)

-- | @since 0.3.0
pmustFindDatum ::
  forall datum s.
  (PIsData datum, PTryFrom PData datum) =>
  Term
    s
    ( POutputDatum
        :--> PMap 'Unsorted PDatumHash PDatum
        :--> datum
    )
pmustFindDatum =
  phoistAcyclic $
    plam $
      (passertPJust # "datum not found") #.* pfindDatum

-- | @since 0.3.0
pfromDatumHash :: forall s. Term s (POutputDatum :--> PDatumHash)
pfromDatumHash = phoistAcyclic $
  plam $
    flip pmatch $ \case
      POutputDatumHash ((pfield @"datumHash" #) -> hash) -> hash
      _ -> ptraceError "not a datum hash"

-- | @since 0.3.0
pfromInlineDatum :: forall s. Term s (POutputDatum :--> PDatum)
pfromInlineDatum = phoistAcyclic $
  plam $
    flip pmatch $ \case
      POutputDatum ((pfield @"outputDatum" #) -> datum) -> datum
      _ -> ptraceError "not an inline datum"

{- | Find a datum with the given hash, and 'ptryFrom' it.

     @since 0.3.0
-}
ptryFindDatum ::
  forall datum (s :: S).
  PTryFrom PData datum =>
  Term
    s
    ( PDatumHash
        :--> PMap 'Unsorted PDatumHash PDatum
        :--> PMaybe datum
    )
ptryFindDatum =
  phoistAcyclic $
    plam $
      (pfmap # ptryFromDatum)
        #.* PAssocMap.plookup

{- | Convert a 'PDatum' to the given datum type.

     @since 0.3.0
-}
ptryFromDatum ::
  forall datum s.
  (PTryFrom PData datum) =>
  Term s (PDatum :--> datum)
ptryFromDatum = phoistAcyclic $ plam $ flip ptryFrom fst . pto

infixr 8 #.*
infixr 8 #.**

-- | @since 0.3.0
(#.*) ::
  forall d c b a s.
  Term s (c :--> d) ->
  Term s (a :--> b :--> c) ->
  Term s a ->
  Term s b ->
  Term s d
(#.*) f g x y = f #$ g # x # y

-- | @since 0.3.0
(#.**) ::
  forall e d c b a s.
  Term s (d :--> e) ->
  Term s (a :--> b :--> c :--> d) ->
  Term s a ->
  Term s b ->
  Term s c ->
  Term s e
(#.**) f g x y z = f #$ g # x # y # z
