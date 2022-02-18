-- | Plutarch utility functions that should be upstreamed or don't belong anywhere else
module Agora.Utils (module Agora.Utils) where

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Value (AssetClass (..))

--------------------------------------------------------------------------------

import Plutarch.Api.V1 (
  PCurrencySymbol,
  PDatum,
  PDatumHash,
  PMap (PMap),
  PPubKeyHash,
  PTokenName,
  PTuple,
  PTxInfo (PTxInfo),
  PValue (PValue),
 )
import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.Internal (punsafeCoerce)
import Plutarch.Monadic qualified as P

--------------------------------------------------------------------------------

-- | Assert a particular bool, trace on falsehood. Use in monadic context
passert :: Term s PString -> Term s PBool -> Term s k -> Term s k
passert errorMessage check k = pif check k (ptraceError errorMessage)

-- | Find a datum with the given hash.
pfindDatum :: Term s (PDatumHash :--> PTxInfo :--> PMaybe PDatum)
pfindDatum = phoistAcyclic $
  plam $ \datumHash txInfo'' -> P.do
    PTxInfo txInfo' <- pmatch txInfo''
    plookupTuple # datumHash #$ pfield @"data" # txInfo'

-- | Find a datum with the given hash. NOTE: this is unsafe in the sense that, if the data layout is wrong, this is UB.
pfindDatum' :: PIsData a => Term s (PDatumHash :--> PTxInfo :--> PMaybe (PAsData a))
pfindDatum' = phoistAcyclic $ plam $ \dh x -> punsafeCoerce $ pfindDatum # dh # x

-- | Check if a PubKeyHash signs this transaction
ptxSignedBy :: Term s (PTxInfo :--> PAsData PPubKeyHash :--> PBool)
ptxSignedBy = phoistAcyclic $
  plam $ \txInfo' pkh -> P.do
    txInfo <- pletFields @'["signatories"] txInfo'
    pelem @PBuiltinList # pkh # txInfo.signatories

-- | Get the first element that matches a predicate or return Nothing
pfind' ::
  PIsListLike list a =>
  (Term s a -> Term s PBool) ->
  Term s (list a :--> PMaybe a)
pfind' p =
  precList
    (\self x xs -> pif (p x) (pcon (PJust x)) (self # xs))
    (const $ pcon PNothing)

-- | Find the value for a given key in an assoclist
plookup ::
  (PEq a, PIsListLike list (PBuiltinPair a b)) =>
  Term s (a :--> list (PBuiltinPair a b) :--> PMaybe b)
plookup =
  phoistAcyclic $
    plam $ \k xs ->
      pmatch (pfind' (\p -> pfstBuiltin # p #== k) # xs) $ \case
        PNothing -> pcon PNothing
        PJust p -> pcon (PJust (psndBuiltin # p))

-- | Find the value for a given key in an assoclist which uses 'PTuple's
plookupTuple ::
  (PEq a, PIsListLike list (PAsData (PTuple a b)), PIsData a, PIsData b) =>
  Term s (a :--> list (PAsData (PTuple a b)) :--> PMaybe b)
plookupTuple =
  phoistAcyclic $
    plam $ \k xs ->
      pmatch (pfind' (\p -> (pfield @"_0" # pfromData p) #== k) # xs) $ \case
        PNothing -> pcon PNothing
        PJust p -> pcon (PJust (pfield @"_1" # pfromData p))

-- | Extract a Maybe by providing a default value in case of Just
pfromMaybe :: forall a s. Term s (a :--> PMaybe a :--> a)
pfromMaybe = phoistAcyclic $
  plam $ \e a ->
    pmatch a $ \case
      PJust a' -> a'
      PNothing -> e

-- | Escape with a particular value on expecting 'Just'. For use in monadic context
pexpectJust :: forall r a s. Term s r -> Term s (PMaybe a) -> (Term s a -> Term s r) -> Term s r
pexpectJust escape ma f =
  pmatch ma $ \case
    PJust v -> f v
    PNothing -> escape

passetClassValueOf ::
  Term s (PCurrencySymbol :--> PTokenName :--> PValue :--> PInteger)
passetClassValueOf =
  phoistAcyclic $
    plam $ \sym token value'' -> P.do
      PValue value' <- pmatch value''
      PMap value <- pmatch value'
      m' <- pexpectJust 0 (plookup # pdata sym # value)
      PMap m <- pmatch (pfromData m')
      v <- pexpectJust 0 (plookup # pdata token # m)
      pfromData v

passetClassValueOf' :: AssetClass -> Term s (PValue :--> PInteger)
passetClassValueOf' (AssetClass (sym, token)) =
  passetClassValueOf # pconstant sym # pconstant token

-- | Union two maps using a merge function on collisions
pmapUnionWith :: forall k v s. PIsData v => Term s ((v :--> v :--> v) :--> PMap k v :--> PMap k v :--> PMap k v)
pmapUnionWith = phoistAcyclic $
  -- TODO: this function is kinda suspect. I feel like a lot of optimizations could be done here
  plam $ \f xs' ys' -> P.do
    PMap xs <- pmatch xs'
    PMap ys <- pmatch ys'
    let ls =
          pmap
            # ( plam $ \p -> P.do
                  pf <- plet $ pfstBuiltin # p
                  ps <- plet $ psndBuiltin # p
                  pmatch (plookup # pf # ys) $ \case
                    PJust v -> P.do
                      -- Data conversions here are silly, aren't they?
                      ppairDataBuiltin # pf # (pdata (f # pfromData ps # pfromData v))
                    PNothing -> p
              )
            # xs
        rs =
          pfilter
            # ( plam $ \p ->
                  pnot # (pany # (plam $ \p' -> pfstBuiltin # p' #== pfstBuiltin # p) # xs)
              )
            # ys
    pcon (PMap $ pconcat # ls # rs)

-- | Add two 'PValue's together
paddValue :: forall s. Term s (PValue :--> PValue :--> PValue)
paddValue = phoistAcyclic $
  plam $ \a' b' -> P.do
    PValue a <- pmatch a'
    PValue b <- pmatch b'
    pcon
      ( PValue $
          pmapUnionWith # (plam $ \a' b' -> pmapUnionWith # (plam (+)) # a' # b') # a # b
      )
