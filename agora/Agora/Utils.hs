{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}

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
  compiledEffect,
) where

import Optics.TH (makeFieldLabelsNoPrefix)
import Plutarch.Api.V1 (PPOSIXTime, PTokenName, PValidatorHash)
import Plutarch.Api.V2 (PScriptHash)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Extra.Time (PCurrentTime (PCurrentTime))
import Plutarch.List (puncons)
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

makeFieldLabelsNoPrefix ''CompiledValidator

{- | Type-safe wrapper for compiled plutus miting policy.

     @since 0.2.0
-}
newtype CompiledMintingPolicy (redeemer :: Type) = CompiledMintingPolicy
  { getCompiledMintingPolicy :: MintingPolicy
  }

makeFieldLabelsNoPrefix ''CompiledMintingPolicy

{- | Type-safe wrapper for compiled plutus effect.

     @since 0.2.0
-}
newtype CompiledEffect (datum :: Type) = CompiledEffect
  { getCompiledEffect :: Validator
  }

compiledEffect :: forall (datum :: Type). Validator -> CompiledEffect datum
compiledEffect = CompiledEffect

makeFieldLabelsNoPrefix ''CompiledEffect

-- | @since 1.0.0
plistEqualsBy ::
  forall
    (list1 :: PType -> PType)
    (list2 :: PType -> PType)
    (a :: PType)
    (b :: PType)
    (s :: S).
  (PIsListLike list1 a, PIsListLike list2 b) =>
  Term s ((a :--> b :--> PBool) :--> list1 a :--> (list2 b :--> PBool))
plistEqualsBy = phoistAcyclic $ pfix # go
  where
    go = plam $ \self eq l1 l2 -> unTermCont $ do
      l1' <- pmatchC $ puncons # l1
      l2' <- pmatchC $ puncons # l2

      case (l1', l2') of
        (PJust l1'', PJust l2'') -> do
          (PPair h1 t1) <- pmatchC l1''
          (PPair h2 t2) <- pmatchC l2''

          pure $ eq # h1 # h2 #&& self # eq # t1 # t2
        (PNothing, PNothing) -> pure $ pconstant True
        _ -> pure $ pconstant False

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
