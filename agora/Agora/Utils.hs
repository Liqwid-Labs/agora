{-# LANGUAGE QuantifiedConstraints #-}

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
  pvalidatorHashToTokenName,
  pscriptHashToTokenName,
  scriptHashToTokenName,
) where

import Plutarch.Api.V1 (PTokenName, PValidatorHash)
import Plutarch.Api.V2 (PScriptHash)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V2 (
  Address (Address),
  Credential (ScriptCredential),
  MintingPolicy,
  ScriptHash (ScriptHash),
  TokenName (TokenName),
  Validator,
  ValidatorHash (ValidatorHash),
 )

{- Functions which should (probably) not be upstreamed
   All of these functions are quite inefficient.
-}

{- | Safely convert a 'ValidatorHash' into a 'TokenName'. This can be useful for tagging
     tokens for extra safety.

     @since 0.1.0
-}
validatorHashToTokenName :: ValidatorHash -> TokenName
validatorHashToTokenName (ValidatorHash hash) = TokenName hash

{- | Safely convert a 'PValidatorHash' into a 'PTokenName'. This can be useful for tagging
     tokens for extra safety.

     @since 1.0.0
-}
pvalidatorHashToTokenName :: forall (s :: S). Term s PValidatorHash -> Term s PTokenName
pvalidatorHashToTokenName = punsafeCoerce

{- | Safely convert a 'PScriptHash' into a 'PTokenName'. This can be useful for tagging
     tokens for extra safety.

     @since 1.0.0
-}
scriptHashToTokenName :: ScriptHash -> TokenName
scriptHashToTokenName (ScriptHash hash) = TokenName hash

{- | Safely convert a 'PScriptHash' into a 'PTokenName'. This can be useful for tagging
     tokens for extra safety.

     @since 1.0.0
-}
pscriptHashToTokenName :: forall (s :: S). Term s PScriptHash -> Term s PTokenName
pscriptHashToTokenName = punsafeCoerce

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
