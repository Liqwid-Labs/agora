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
  plistEqualsBy,
  pstringIntercalate,
  punwords,
  pcurrentTimeDuration,
  pdelete,
  pdeleteBy,
  pisSingleton,
  pfromSingleton,
) where

import Plutarch.Api.V1 (PPOSIXTime, PTokenName, PValidatorHash)
import Plutarch.Api.V2 (PScriptHash)
import Plutarch.Extra.Time (PCurrentTime (PCurrentTime))
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
pvalidatorHashToTokenName :: forall (s :: S). Term s (PValidatorHash :--> PTokenName)
pvalidatorHashToTokenName = phoistAcyclic $ plam punsafeCoerce

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

-- | @since 1.0.0
plistEqualsBy ::
  forall
    (list1 :: PType -> PType)
    (list2 :: PType -> PType)
    (a :: PType)
    (b :: PType)
    (s :: S).
  (PIsListLike list1 a, PIsListLike list2 b) =>
  Term s ((a :--> b :--> PBool) :--> list1 a :--> list2 b :--> PBool)
plistEqualsBy = phoistAcyclic $
  plam $ \eq -> pfix #$ plam $ \self l1 l2 ->
    pelimList
      ( \x xs ->
          pelimList
            ( \y ys ->
                -- Avoid comparison if two lists have different length.
                self # xs # ys #&& eq # x # y
            )
            -- l2 is empty, but l1 is not.
            (pconstant False)
            l2
      )
      -- l1 is empty, so l2 should be empty as well.
      (pnull # l2)
      l1

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
pcurrentTimeDuration ::
  forall (s :: S).
  Term
    s
    ( PCurrentTime
        :--> PPOSIXTime
    )
pcurrentTimeDuration = phoistAcyclic $
  plam $
    flip pmatch $
      \(PCurrentTime lb ub) -> ub - lb

{- | / O(n) /. Remove the first occurance of a value from the given list.

     @since 1.0.0
-}
pdelete ::
  forall (a :: PType) (list :: PType -> PType) (s :: S).
  (PEq a, PIsListLike list a) =>
  Term s (a :--> list a :--> list a)
pdelete = phoistAcyclic $ pdeleteBy # plam (#==)

-- | @since 1.0.0
pdeleteBy ::
  forall (a :: PType) (list :: PType -> PType) (s :: S).
  (PIsListLike list a) =>
  Term s ((a :--> a :--> PBool) :--> a :--> list a :--> list a)
pdeleteBy = phoistAcyclic $
  plam $ \f' x -> plet (f' # x) $ \f ->
    precList
      ( \self h t ->
          pif
            (f # h)
            t
            (pcons # h #$ self # t)
      )
      (const pnil)

{- | / O(1) /.Return true if the given list has only one element.

     @since 1.0.0
-}
pisSingleton ::
  forall (a :: PType) (list :: PType -> PType) (s :: S).
  (PIsListLike list a) =>
  Term s (list a :--> PBool)
pisSingleton =
  phoistAcyclic $
    precList
      (\_ _ t -> pnull # t)
      (const $ pconstant False)

-- | @since 1.0.0
pfromSingleton ::
  forall (a :: PType) (list :: PType -> PType) (s :: S).
  (PIsListLike list a) =>
  Term s (list a :--> a)
pfromSingleton =
  phoistAcyclic $
    precList
      ( \_ h t ->
          pif
            (pnull # t)
            h
            (ptraceError "More than one element")
      )
      (const $ ptraceError "Empty list")
