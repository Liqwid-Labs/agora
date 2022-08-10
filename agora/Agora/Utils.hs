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
  pvalidatorHashToTokenName,
  mustFindDatum',
  validatorHashToAddress,
  pltAsData,
  withBuiltinPairAsData,
  CompiledValidator (..),
  CompiledMintingPolicy (..),
  CompiledEffect (..),
) where

import Plutarch.Api.V1 (
  PDatum,
  PDatumHash,
  PMaybeData,
  PTokenName (PTokenName),
  PTuple,
  PValidatorHash,
 )
import Plutarch.Builtin (pforgetData)
import Plutarch.Extra.List (plookupTuple)
import Plutarch.Extra.Maybe (passertPDJust, passertPJust)
import Plutarch.Extra.TermCont (ptryFromC)
import PlutusLedgerApi.V1 (
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

{- | Plutarch level 'validatorHashToTokenName'.

     @since 0.1.0
-}
pvalidatorHashToTokenName :: forall (s :: S). Term s PValidatorHash -> Term s PTokenName
pvalidatorHashToTokenName vh = pcon (PTokenName (pto vh))

{- | Find datum given a maybe datum hash

     @since 0.1.0
-}
mustFindDatum' ::
  forall (datum :: PType).
  (PIsData datum, PTryFrom PData datum) =>
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
    pure d

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
