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
  pvalidatorHashToTokenName,
  pscriptHashToTokenName,
  scriptHashToTokenName,
  plistEqualsBy,
  pstringIntercalate,
  punwords,
  pcurrentTimeDuration,
  pdelete,
  pdeleteBy,
  pmustDeleteBy,
  pisSingleton,
  pfromSingleton,
  pmapMaybe,
  PAlternative (..),
  ppureIf,
  pltBy,
  pinsertUniqueBy,
  ptryFromRedeemer,
  passert,
  pisNothing,
  pisDNothing,
) where

import Plutarch.Api.V1 (KeyGuarantees (Unsorted), PPOSIXTime, PRedeemer, PTokenName, PValidatorHash)
import Plutarch.Api.V1.AssocMap (PMap, plookup)
import Plutarch.Api.V2 (PMaybeData (PDNothing), PScriptHash, PScriptPurpose)
import Plutarch.Extra.Applicative (PApplicative (ppure))
import Plutarch.Extra.Category (PCategory (pidentity))
import Plutarch.Extra.Functor (PFunctor (PSubcategory, pfmap))
import Plutarch.Extra.Maybe (pjust, pnothing)
import Plutarch.Extra.Ord (PComparator, POrdering (PLT), pcompareBy, pequateBy)
import Plutarch.Extra.Time (PCurrentTime (PCurrentTime))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V2 (
  Address (Address),
  Credential (ScriptCredential),
  ScriptHash (ScriptHash),
  TokenName (TokenName),
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
  Term s (a :--> list a :--> PMaybe (list a))
pdelete = phoistAcyclic $ pdeleteBy # plam (#==)

-- | @since 1.0.0
pdeleteBy ::
  forall (a :: PType) (list :: PType -> PType) (s :: S).
  (PIsListLike list a) =>
  Term s ((a :--> a :--> PBool) :--> a :--> list a :--> PMaybe (list a))
pdeleteBy = phoistAcyclic $
  plam $ \f' x -> plet (f' # x) $ \f ->
    precList
      ( \self h t ->
          pif
            (f # h)
            (pjust # t)
            (pfmap # (pcons # h) # (self # t))
      )
      (const pnothing)

-- | @since 1.0.0
pmustDeleteBy ::
  forall (a :: PType) (list :: PType -> PType) (s :: S).
  (PIsListLike list a) =>
  Term s ((a :--> a :--> PBool) :--> a :--> list a :--> list a)
pmustDeleteBy = phoistAcyclic $
  plam $ \f' x -> plet (f' # x) $ \f ->
    precList
      ( \self h t ->
          pif
            (f # h)
            t
            (pcons # h #$ self # t)
      )
      (const $ ptraceError "Cannot delete element")

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

{- Throws an error if the given list contains zero or more than one elements.
    Otherwise returns the only element.

   @since 1.0.0
-}
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

{- | A version of 'pmap' which can throw out elements and change the list type
      along the way.

     @since 1.0.0
-}
pmapMaybe ::
  forall
    (listO :: PType -> PType)
    (b :: PType)
    (listI :: PType -> PType)
    (a :: PType)
    (s :: S).
  (PIsListLike listI a, PIsListLike listO b) =>
  Term s ((a :--> PMaybe b) :--> listI a :--> listO b)
pmapMaybe = phoistAcyclic $
  plam $ \f ->
    precList
      ( \self h t ->
          pmatch
            (f # h)
            ( \case
                PJust x -> pcons # x
                PNothing -> pidentity
            )
            # (self # t)
      )
      (const pnil)

infixl 3 #<|>

-- | @since 1.0.0
class (PApplicative f) => PAlternative (f :: PType -> PType) where
  (#<|>) ::
    forall (a :: PType) (s :: S).
    (PSubcategory f a) =>
    Term s (f a :--> f a :--> f a)
  pempty ::
    forall (a :: PType) (s :: S).
    (PSubcategory f a) =>
    Term s (f a)

-- | @since 1.0.0
instance PAlternative PMaybe where
  (#<|>) = phoistAcyclic $
    plam $ \a b -> pmatch a $ \case
      PNothing -> b
      PJust _ -> a
  pempty = pnothing

-- | @since 1.0.0
ppureIf ::
  forall
    (f :: PType -> PType)
    (a :: PType)
    (s :: S).
  (PAlternative f, PSubcategory f a) =>
  Term s (PBool :--> a :--> f a)
ppureIf = phoistAcyclic $
  plam $ \cond x ->
    pif
      cond
      (ppure # x)
      pempty

{- | Less then check using a `PComparator`.

     @ since 1.0.0
-}
pltBy ::
  forall (a :: PType) (s :: S).
  Term
    s
    ( PComparator a
        :--> a
        :--> a
        :--> PBool
    )
pltBy = phoistAcyclic $
  plam $ \c x y ->
    pcompareBy # c # x # y #== pcon PLT

-- | @since 1.0.0
pinsertUniqueBy ::
  forall (list :: PType -> PType) (a :: PType) (s :: S).
  (PIsListLike list a) =>
  Term s (PComparator a :--> a :--> list a :--> list a)
pinsertUniqueBy = phoistAcyclic $
  plam $ \c x ->
    let lt = pltBy # c
        eq = pequateBy # c
     in precList
          ( \self h t ->
              let ensureUniqueness =
                    pif
                      (eq # x # h)
                      (ptraceError "inserted value already exists")
                  next =
                    pif
                      (lt # x # h)
                      (pcons # x #$ pcons # h # t)
                      (pcons # h #$ self # t)
               in ensureUniqueness next
          )
          (const $ psingleton # x)

-- | @since 1.0.0
ptryFromRedeemer ::
  forall (r :: PType) (s :: S).
  (PTryFrom PData r) =>
  Term
    s
    ( PScriptPurpose
        :--> PMap 'Unsorted PScriptPurpose PRedeemer
        :--> PMaybe r
    )
ptryFromRedeemer = phoistAcyclic $
  plam $ \p m ->
    pfmap
      # plam (flip ptryFrom fst . pto)
      # (plookup # p # m)

-- | @since 1.0.0
passert ::
  forall (a :: PType) (s :: S).
  Term s PString ->
  Term s PBool ->
  Term s a ->
  Term s a
passert msg cond x = pif cond x $ ptraceError msg

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
