{- |
Module     : Agora.Utils
Maintainer : emi@haskell.fyi
Description: Plutarch utility functions that should be upstreamed or don't belong anywhere else.

Plutarch utility functions that should be upstreamed or don't belong anywhere else.
-}
module Agora.Utils (
  -- * TermCont-based combinators. Some of these will live in plutarch eventually.
  tcassert,
  tclet,
  tcmatch,
  tctryFrom,

  -- * Validator-level utility functions
  pfind',
  pfindDatum,
  ptryFindDatum,
  pvalueSpent,
  ptxSignedBy,
  paddValue,
  plookup,
  pfromMaybe,
  psymbolValueOf,
  pgeqByClass,
  pgeqBySymbol,
  pgeqByClass',
  pfindTxInByTxOutRef,
  psingletonValue,
  pfindMap,
  pnotNull,
  pisJust,
  ptokenSpent,
  pkeysEqual,
  pnubSortBy,
  pisUniq,
  pisUniqBy,
  pisDJust,
  pisUTXOSpent,
  pmsortBy,
  pmsort,
  pnubSort,
  pupdate,
  pmapMap,
  pmapMaybe,

  -- * Functions which should (probably) not be upstreamed
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
  pmergeBy,
  phalve,
  isScriptAddress,
  isPubKey,
) where

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Api (
  Address (..),
  Credential (..),
  CurrencySymbol,
  TokenName (..),
  ValidatorHash (..),
 )
import Plutus.V1.Ledger.Value (AssetClass (..))

--------------------------------------------------------------------------------

import Plutarch.Api.V1 (
  PAddress,
  PCredential (PScriptCredential),
  PCurrencySymbol,
  PDatum,
  PDatumHash,
  PMap,
  PMaybeData (PDJust),
  PMintingPolicy,
  PPubKeyHash,
  PTokenName (PTokenName),
  PTuple,
  PTxInInfo (PTxInInfo),
  PTxOut (PTxOut),
  PTxOutRef,
  PValidatorHash,
  PValue,
  mintingPolicySymbol,
  mkMintingPolicy,
 )
import Plutarch.Api.V1.AssocMap (PMap (PMap))
import Plutarch.Api.V1.AssetClass (PAssetClass, passetClassValueOf, pvalueOf)
import "plutarch" Plutarch.Api.V1.Value (PValue (PValue))
import Plutarch.Builtin (pforgetData, ppairDataBuiltin)
import Plutarch.Reducible (Reducible (Reduce))
import Plutarch.TryFrom (PTryFrom (PTryFromExcess))
import Plutarch.Extra.Map (pkeys)


--------------------------------------------------------------------------------
-- TermCont-based combinators. Some of these will live in plutarch eventually.

-- | Assert a particular 'PBool', trace if false.
tcassert :: forall r (s :: S). Term s PString -> Term s PBool -> TermCont @r s ()
tcassert errorMessage check = tcont $ \k -> pif check (k ()) (ptraceError errorMessage)

-- | 'plet' but for use in 'TermCont'.
tclet :: forall r (s :: S) (a :: PType). Term s a -> TermCont @r s (Term s a)
tclet = tcont . plet

-- | 'pmatch' but for use in 'TermCont'.
tcmatch :: forall (a :: PType) (s :: S). PlutusType a => Term s a -> TermCont s (a s)
tcmatch = tcont . pmatch

-- | 'ptryFrom' but for use in 'TermCont'.
tctryFrom :: forall b a s r. PTryFrom a b => Term s a -> TermCont @r s (Term s b, Reduce (PTryFromExcess a b s))
tctryFrom = tcont . ptryFrom

-- | Escape with a particular value on expecting 'Just'. For use in monadic context.
tcexpectJust ::
  forall r (a :: PType) (s :: S).
  Term s r ->
  Term s (PMaybe a) ->
  TermCont @r s (Term s a)
tcexpectJust escape ma = tcont $ \f ->
  pmatch ma $ \case
    PJust v -> f v
    PNothing -> escape

--------------------------------------------------------------------------------
-- Validator-level utility functions

-- | Find a datum with the given hash.
pfindDatum :: Term s (PDatumHash :--> PBuiltinList (PAsData (PTuple PDatumHash PDatum)) :--> PMaybe PDatum)
pfindDatum = phoistAcyclic $
  plam $ \datumHash datums -> plookupTuple # datumHash # datums

-- | Find a datum with the given hash, and `ptryFrom` it.
ptryFindDatum :: forall (a :: PType) (s :: S). PTryFrom PData a => Term s (PDatumHash :--> PBuiltinList (PAsData (PTuple PDatumHash PDatum)) :--> PMaybe a)
ptryFindDatum = phoistAcyclic $
  plam $ \datumHash inputs ->
    pmatch (pfindDatum # datumHash # inputs) $ \case
      PNothing -> pcon PNothing
      PJust datum -> unTermCont $ do
        (datum', _) <- tctryFrom (pto datum)
        pure $ pcon (PJust datum')

-- | Check if a PubKeyHash signs this transaction.
ptxSignedBy :: Term s (PBuiltinList (PAsData PPubKeyHash) :--> PAsData PPubKeyHash :--> PBool)
ptxSignedBy = phoistAcyclic $
  plam $ \sigs sig -> pelem # sig # sigs

-- | Get the first element that matches a predicate or return Nothing.
pfind' ::
  PIsListLike list a =>
  (Term s a -> Term s PBool) ->
  Term s (list a :--> PMaybe a)
pfind' p =
  precList
    (\self x xs -> pif (p x) (pcon (PJust x)) (self # xs))
    (const $ pcon PNothing)

-- | Get the first element that maps to a PJust in a list.
pfindMap ::
  PIsListLike list a =>
  Term s ((a :--> PMaybe b) :--> list a :--> PMaybe b)
pfindMap =
  phoistAcyclic $
    plam $ \p ->
      precList
        ( \self x xs ->
            -- In the future, this should use `pmatchSum`, I believe?
            pmatch (p # x) $ \case
              PNothing -> self # xs
              PJust v -> pcon (PJust v)
        )
        (const $ pcon PNothing)

-- | Find the value for a given key in an associative list.
plookup ::
  (PEq a, PIsListLike list (PBuiltinPair a b)) =>
  Term s (a :--> list (PBuiltinPair a b) :--> PMaybe b)
plookup =
  phoistAcyclic $
    plam $ \k xs ->
      pmatch (pfind' (\p -> pfstBuiltin # p #== k) # xs) $ \case
        PNothing -> pcon PNothing
        PJust p -> pcon (PJust (psndBuiltin # p))

-- | Find the value for a given key in an assoclist which uses 'PTuple's.
plookupTuple ::
  (PEq a, PIsListLike list (PAsData (PTuple a b)), PIsData a, PIsData b) =>
  Term s (a :--> list (PAsData (PTuple a b)) :--> PMaybe b)
plookupTuple =
  phoistAcyclic $
    plam $ \k xs ->
      pmatch (pfind' (\p -> (pfield @"_0" # pfromData p) #== k) # xs) $ \case
        PNothing -> pcon PNothing
        PJust p -> pcon (PJust (pfield @"_1" # pfromData p))

-- | Extract a Maybe by providing a default value in case of Just.
pfromMaybe :: forall a s. Term s (a :--> PMaybe a :--> a)
pfromMaybe = phoistAcyclic $
  plam $ \e a ->
    pmatch a $ \case
      PJust a' -> a'
      PNothing -> e

-- | Yield True if a given PMaybe is of form @'PJust' _@.
pisJust :: forall a s. Term s (PMaybe a :--> PBool)
pisJust = phoistAcyclic $
  plam $ \v' ->
    pmatch v' $ \case
      PJust _ -> pconstant True
      PNothing -> pconstant False

-- | Get the sum of all values belonging to a particular CurrencySymbol.
psymbolValueOf :: Term s (PCurrencySymbol :--> PValue :--> PInteger)
psymbolValueOf =
  phoistAcyclic $
    plam $ \sym value'' -> unTermCont $ do
      PValue value' <- tcmatch value''
      PMap value <- tcmatch value'
      m' <- tcexpectJust 0 (plookup # pdata sym # value)
      PMap m <- tcmatch (pfromData m')
      pure $ pfoldr # plam (\x v -> pfromData (psndBuiltin # x) + v) # 0 # m

-- | Extract amount from PValue belonging to a Haskell-level AssetClass.
passetClassValueOf' :: AssetClass -> Term s (PValue :--> PInteger)
passetClassValueOf' (AssetClass (sym, token)) =
  phoistAcyclic $ plam $ \value -> pvalueOf # value # pconstant sym # pconstant token

-- | Return '>=' on two values comparing by only a particular AssetClass.
pgeqByClass :: Term s (PCurrencySymbol :--> PTokenName :--> PValue :--> PValue :--> PBool)
pgeqByClass =
  phoistAcyclic $
    plam $ \cs tn a b ->
      pvalueOf # b # cs # tn #<= pvalueOf # a # cs # tn

-- | Return '>=' on two values comparing by only a particular CurrencySymbol.
pgeqBySymbol :: Term s (PCurrencySymbol :--> PValue :--> PValue :--> PBool)
pgeqBySymbol =
  phoistAcyclic $
    plam $ \cs a b ->
      psymbolValueOf # cs # b #<= psymbolValueOf # cs # a

-- | Return '>=' on two values comparing by only a particular Haskell-level AssetClass.
pgeqByClass' :: AssetClass -> Term s (PValue :--> PValue :--> PBool)
pgeqByClass' ac =
  phoistAcyclic $
    plam $ \a b ->
      passetClassValueOf' ac # b #<= passetClassValueOf' ac # a

-- | Union two maps using a merge function on collisions.
pmapUnionWith :: forall k v s. PIsData v => Term s ((v :--> v :--> v) :--> PMap k v :--> PMap k v :--> PMap k v)
pmapUnionWith = phoistAcyclic $
  -- TODO: this function is kinda suspect. I feel like a lot of optimizations could be done here
  plam $ \f xs' ys' -> unTermCont $ do
    PMap xs <- tcmatch xs'
    PMap ys <- tcmatch ys'
    let ls =
          pmap
            # plam
              ( \p -> unTermCont $ do
                  pf <- tclet $ pfstBuiltin # p
                  pure $
                    pmatch (plookup # pf # ys) $ \case
                      PJust v ->
                        -- Data conversions here are silly, aren't they?
                        ppairDataBuiltin # pf # pdata (f # pfromData (psndBuiltin # p) # pfromData v)
                      PNothing -> p
              )
            # xs
        rs =
          pfilter
            # plam
              ( \p ->
                  pnot #$ pany # plam (\p' -> pfstBuiltin # p' #== pfstBuiltin # p) # xs
              )
            # ys
    pure $ pcon (PMap $ pconcat # ls # rs)

-- | A special version of `pmap` which allows list elements to be thrown out.
pmapMaybe :: forall s a list. (PIsListLike list a) => Term s ((a :--> PMaybe a) :--> list a :--> list a)
pmapMaybe = phoistAcyclic $
  pfix #$ plam $ \self f l -> pif (pnull # l) pnil $
    unTermCont $ do
      x <- tclet $ phead # l
      xs <- tclet $ ptail # l

      pure $
        pmatch (f # x) $ \case
          PJust ux -> pcons # ux #$ self # f # xs
          _ -> self # f # xs

-- | / O(n) /. Update the value at a given key in a `PMap`, have the same functionalities as 'Data.Map.update'.
pupdate :: forall s k v. (PIsData k, PIsData v) => Term s ((v :--> PMaybe v) :--> k :--> PMap k v :--> PMap k v)
pupdate = phoistAcyclic $
  plam $ \f (pdata -> tk) (pto -> (ps :: Term _ (PBuiltinList _))) ->
    pcon $
      PMap $
        pmapMaybe
          # plam
            ( \kv ->
                let k = pfstBuiltin # kv
                    v = pfromData $ psndBuiltin # kv
                 in pif
                      (k #== tk)
                      -- 'PBuiltinPair' doesn't have 'PFunctor', so:
                      ( pmatch (f # v) $
                          \case
                            PJust uv -> pcon $ PJust $ ppairDataBuiltin # k # pdata uv
                            _ -> pcon PNothing
                      )
                      (pcon $ PJust kv)
            )
          # ps

-- | / O(n) /. Map a function over all values in a 'PMap'.
pmapMap :: forall s k a b. (PIsData k, PIsData a, PIsData b) => Term s ((a :--> b) :--> PMap k a :--> PMap k b)
pmapMap = phoistAcyclic $
  plam $ \f (pto -> (ps :: Term _ (PBuiltinList _))) ->
    pcon $
      PMap $
        pmap
          # plam
            ( \kv ->
                let k = pfstBuiltin # kv
                    v = psndBuiltin # kv

                    nv = pdata $ f # pfromData v
                 in ppairDataBuiltin # k # nv
            )
          # ps

-- | Add two 'PValue's together.
paddValue :: forall s. Term s (PValue :--> PValue :--> PValue)
paddValue = phoistAcyclic $
  plam $ \a' b' -> unTermCont $ do
    PValue a <- tcmatch a'
    PValue b <- tcmatch b'
    pure $
      pcon
        ( PValue $
            pmapUnionWith # plam (\a' b' -> pmapUnionWith # plam (+) # a' # b') # a # b
        )

-- | Sum of all value at input.
pvalueSpent :: Term s (PBuiltinList (PAsData PTxInInfo) :--> PValue)
pvalueSpent = phoistAcyclic $
  plam $ \inputs ->
    pfoldr
      # plam
        ( \txInInfo' v ->
            pmatch
              (pfromData txInInfo')
              $ \(PTxInInfo txInInfo) ->
                paddValue
                  # pmatch
                    (pfield @"resolved" # txInInfo)
                    (\(PTxOut o) -> pfromData $ pfield @"value" # o)
                  # v
        )
      # pconstant mempty
      # inputs

-- | Find the TxInInfo by a TxOutRef.
pfindTxInByTxOutRef :: Term s (PTxOutRef :--> PBuiltinList (PAsData PTxInInfo) :--> PMaybe PTxInInfo)
pfindTxInByTxOutRef = phoistAcyclic $
  plam $ \txOutRef inputs ->
    pfindMap
      # plam
        ( \txInInfo' ->
            plet (pfromData txInInfo') $ \r ->
              pmatch r $ \(PTxInInfo txInInfo) ->
                pif
                  (pdata txOutRef #== pfield @"outRef" # txInInfo)
                  (pcon (PJust r))
                  (pcon PNothing)
        )
      #$ inputs

-- | True if a list is not empty.
pnotNull :: forall list a. PIsListLike list a => Term _ (list a :--> PBool)
pnotNull = phoistAcyclic $ plam $ pelimList (\_ _ -> pcon PTrue) (pcon PFalse)

{- | Check if a particular asset class has been spent in the input list.

     When using this as an authority check, you __MUST__ ensure the authority
     knows how to ensure its end of the contract.
-}
ptokenSpent :: forall {s :: S}. Term s (PAssetClass :--> PBuiltinList (PAsData PTxInInfo) :--> PBool)
ptokenSpent =
  plam $ \tokenClass inputs ->
    0
      #< pfoldr @PBuiltinList
        # plam
          ( \txInInfo' acc -> unTermCont $ do
              PTxInInfo txInInfo <- tcmatch (pfromData txInInfo')
              PTxOut txOut' <- tcmatch $ pfromData $ pfield @"resolved" # txInInfo
              txOut <- tcont $ pletFields @'["value"] txOut'
              let txOutValue = pfromData txOut.value
              pure $ acc + passetClassValueOf # txOutValue # tokenClass
          )
        # 0
        # inputs

{- | True if both maps have exactly the same keys.
     Using @'#=='@ is not sufficient, because keys returned are not ordered.
-}
pkeysEqual :: (POrd k, PIsData k) => forall (s :: S) a b. Term s (PMap k a :--> PMap k b :--> PBool)
pkeysEqual = phoistAcyclic $
  plam $ \p q -> unTermCont $ do
    pks <- tclet $ pkeys # p
    qks <- tclet $ pkeys # q

    pure $
      pif
        (plength # pks #== plength # qks)
        ( unTermCont $ do
            let comp = phoistAcyclic $ plam $ \(pfromData -> x) (pfromData -> y) -> x #< y
                spks = pmsortBy # comp # pks
                sqks = pmsortBy # comp # qks

            pure $ plistEquals # spks # sqks
        )
        (pcon PFalse)

-- | / O(nlogn) /. Sort and remove dupicate elements in a list.
pnubSortBy ::
  forall list a (s :: S).
  (PIsListLike list a) =>
  Term s ((a :--> a :--> PBool) :--> (a :--> a :--> PBool) :--> list a :--> list a)
pnubSortBy = phoistAcyclic $
  plam $ \eq comp l -> pif (pnull # l) l $
    unTermCont $ do
      sl <- tclet $ pmsortBy # comp # l

      let x = phead # sl
          xs = ptail # sl

      return $ pgo # eq # x # xs
  where
    pgo = phoistAcyclic pfix #$ plam pgo'
    pgo' self eq seen l =
      pif (pnull # l) (psingleton # seen) $
        unTermCont $ do
          x <- tclet $ phead # l
          xs <- tclet $ ptail # l

          return $
            pif
              (eq # x # seen)
              (self # eq # seen # xs)
              (pcons # seen #$ self # eq # x # xs)

-- | Special version of 'pnubSortBy', which requires elements have 'POrd'.
pnubSort ::
  forall list a (s :: S).
  (PIsListLike list a, POrd a) =>
  Term s (list a :--> list a)
pnubSort = phoistAcyclic $ pnubSortBy # eq # comp
  where
    eq = phoistAcyclic $ plam (#==)
    comp = phoistAcyclic $ plam (#<)

-- | / O(nlogn) /. Check if a list contains no duplicates.
pisUniqBy ::
  forall list a (s :: S).
  (PIsListLike list a) =>
  Term s ((a :--> a :--> PBool) :--> (a :--> a :--> PBool) :--> list a :--> PBool)
pisUniqBy = phoistAcyclic $
  plam $ \eq comp xs ->
    let nubbed = pnubSortBy # eq # comp # xs
     in plength # xs #== plength # nubbed

-- | A special case of 'pisUniqBy' which requires elements have 'POrd' instance.
pisUniq :: forall list a (s :: S). (POrd a, PIsListLike list a) => Term s (list a :--> PBool)
pisUniq = phoistAcyclic $ pisUniqBy # eq # comp
  where
    eq = phoistAcyclic $ plam (#==)
    comp = phoistAcyclic $ plam (#<)

-- | Yield True if a given PMaybeData is of form @'PDJust' _@.
pisDJust :: Term s (PMaybeData a :--> PBool)
pisDJust = phoistAcyclic $
  plam $ \x ->
    pmatch
      x
      ( \case
          PDJust _ -> pconstant True
          _ -> pconstant False
      )

-- | Determines if a given UTXO is spent.
pisUTXOSpent :: Term s (PTxOutRef :--> PBuiltinList (PAsData PTxInInfo) :--> PBool)
pisUTXOSpent = phoistAcyclic $
  plam $ \oref inputs -> P.do
    pisJust #$ pfindTxInByTxOutRef # oref # inputs

-- | / O(n) /. Merge two lists which are assumed to be ordered, given a custom comparator.
pmergeBy :: (PIsListLike l a) => Term s ((a :--> a :--> PBool) :--> l a :--> l a :--> l a)
pmergeBy = phoistAcyclic $ pfix #$ plam pmergeBy'
  where
    pmergeBy' self comp a b =
      pif (pnull # a) b $
        pif (pnull # b) a $
          unTermCont $ do
            ah <- tclet $ phead # a
            at <- tclet $ ptail # a
            bh <- tclet $ phead # b
            bt <- tclet $ ptail # b

            pure $
              pif
                (comp # ah # bh)
                (pcons # ah #$ self # comp # at # b)
                (pcons # bh #$ self # comp # a # bt)

{- | / O(nlogn) /. Merge sort, bottom-up version, given a custom comparator.

   Elements are arranged from lowest to highest,
   keeping duplicates in the order they appeared in the input.
-}
pmsortBy :: (PIsListLike l a) => Term s ((a :--> a :--> PBool) :--> l a :--> l a)
pmsortBy = phoistAcyclic $ pfix #$ plam pmsortBy'
  where
    pmsortBy' self comp xs = pif (pnull # xs) pnil $
      pif (pnull #$ ptail # xs) xs $
        pmatch (phalve # xs) $ \(PPair fh sh) ->
          let sfh = self # comp # fh
              ssh = self # comp # sh
           in pmergeBy # comp # sfh # ssh

-- | A special case of 'pmsortBy' which requires elements have 'POrd' instance.
pmsort :: (POrd a, PIsListLike l a) => Term s (l a :--> l a)
pmsort = phoistAcyclic $ pmsortBy # comp
  where
    comp = phoistAcyclic $ plam (#<)

-- | Split a list in half.
phalve :: (PIsListLike l a) => Term s (l a :--> PPair (l a) (l a))
phalve = phoistAcyclic $ plam $ \l -> go # l # l
  where
    go = phoistAcyclic $ pfix #$ plam go'
    go' self xs ys =
      pif
        (pnull # ys)
        (pcon $ PPair pnil xs)
        ( unTermCont $ do
            yt <- tclet $ ptail # ys

            xh <- tclet $ phead # xs
            xt <- tclet $ ptail # xs

            pure $
              pif (pnull # yt) (pcon $ PPair (psingleton # xh) xt) $
                unTermCont $ do
                  yt' <- tclet $ ptail # yt
                  pure $
                    pmatch (self # xt # yt') $ \(PPair first last) ->
                      pcon $ PPair (pcons # xh # first) last
        )

--------------------------------------------------------------------------------
{- Functions which should (probably) not be upstreamed
   All of these functions are quite inefficient.
-}

-- | Create a value with a single asset class.
psingletonValue :: forall s. Term s (PCurrencySymbol :--> PTokenName :--> PInteger :--> PValue)
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
    address <- tclet $ pdata address'
    pure $
      pfilter # plam (\(pfromData -> txOut) -> pfield @"address" # txOut #== address)
        # outputs

-- | Find the data corresponding to a TxOut, if there is one
findTxOutDatum :: Term s (PBuiltinList (PAsData (PTuple PDatumHash PDatum)) :--> PTxOut :--> PMaybe PDatum)
findTxOutDatum = phoistAcyclic $
  plam $ \datums out -> unTermCont $ do
    datumHash' <- tcmatch $ pfromData $ pfield @"datumHash" # out
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
hasOnlyOneTokenOfCurrencySymbol :: Term s (PCurrencySymbol :--> PValue :--> PBool)
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

validatorHashToAddress :: ValidatorHash -> Address
validatorHashToAddress vh = Address (ScriptCredential vh) Nothing
