{- |
Module     : Agora.Utils
Maintainer : emi@haskell.fyi
Description: Plutarch utility functions that should be upstreamed or don't belong anywhere else.

Plutarch utility functions that should be upstreamed or don't belong anywhere else.
-}
module Agora.Utils (
  -- * Validator-level utility functions
  passert,
  pfind',
  pfindDatum,
  pfindDatum',
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

  -- * Functions which should (probably) not be upstreamed
  anyOutput,
  allOutputs,
  anyInput,
  findTxOutByTxOutRef,
  scriptHashFromAddress,
  findOutputsToAddress,
  findTxOutDatum,
) where

--------------------------------------------------------------------------------

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
  PPubKeyHash,
  PTokenName,
  PTuple,
  PTxInInfo (PTxInInfo),
  PTxInfo (PTxInfo),
  PTxOut (PTxOut),
  PTxOutRef,
  PValidatorHash,
  PValue,
 )
import Plutarch.Api.V1.AssocMap (PMap (PMap))
import Plutarch.Api.V1.Extra (PAssetClass, passetClassValueOf, pvalueOf)
import Plutarch.Api.V1.Value (PValue (PValue))
import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.Internal (punsafeCoerce)
import Plutarch.Monadic qualified as P

--------------------------------------------------------------------------------
-- Validator-level utility functions

-- | Assert a particular 'PBool', trace if false. Use in monadic context.
passert :: Term s PString -> Term s PBool -> Term s k -> Term s k
passert errorMessage check k = pif check k (ptraceError errorMessage)

-- | Find a datum with the given hash.
pfindDatum :: Term s (PDatumHash :--> PTxInfo :--> PMaybe PDatum)
pfindDatum = phoistAcyclic $
  plam $ \datumHash txInfo'' -> P.do
    PTxInfo txInfo' <- pmatch txInfo''
    plookupTuple # datumHash #$ pfield @"data" # txInfo'

{- | Find a datum with the given hash.
NOTE: this is unsafe in the sense that, if the data layout is wrong, this is UB.
-}
pfindDatum' :: PIsData a => Term s (PDatumHash :--> PTxInfo :--> PMaybe (PAsData a))
pfindDatum' = phoistAcyclic $ plam $ \dh x -> punsafeCoerce $ pfindDatum # dh # x

-- | Check if a PubKeyHash signs this transaction.
ptxSignedBy :: Term s (PTxInfo :--> PAsData PPubKeyHash :--> PBool)
ptxSignedBy = phoistAcyclic $
  plam $ \txInfo' pkh -> P.do
    txInfo <- pletFields @'["signatories"] txInfo'
    pelem @PBuiltinList # pkh # txInfo.signatories

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

-- | Yield True if a given PMaybe is of form PJust _.
pisJust :: forall a s. Term s (PMaybe a :--> PBool)
pisJust = phoistAcyclic $
  plam $ \v' -> P.do
    v <- pmatch v'
    case v of
      PJust _ -> pconstant True
      PNothing -> pconstant False

-- | Escape with a particular value on expecting 'Just'. For use in monadic context.
pexpectJust ::
  forall r a s.
  Term s r ->
  Term s (PMaybe a) ->
  (Term s a -> Term s r) ->
  Term s r
pexpectJust escape ma f =
  pmatch ma $ \case
    PJust v -> f v
    PNothing -> escape

-- | Get the sum of all values belonging to a particular CurrencySymbol.
psymbolValueOf :: Term s (PCurrencySymbol :--> PValue :--> PInteger)
psymbolValueOf =
  phoistAcyclic $
    plam $ \sym value'' -> P.do
      PValue value' <- pmatch value''
      PMap value <- pmatch value'
      m' <- pexpectJust 0 (plookup # pdata sym # value)
      PMap m <- pmatch (pfromData m')
      pfoldr # plam (\x v -> pfromData (psndBuiltin # x) + v) # 0 # m

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
  plam $ \f xs' ys' -> P.do
    PMap xs <- pmatch xs'
    PMap ys <- pmatch ys'
    let ls =
          pmap
            # plam
              ( \p -> P.do
                  pf <- plet $ pfstBuiltin # p
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
    pcon (PMap $ pconcat # ls # rs)

-- | Add two 'PValue's together.
paddValue :: forall s. Term s (PValue :--> PValue :--> PValue)
paddValue = phoistAcyclic $
  plam $ \a' b' -> P.do
    PValue a <- pmatch a'
    PValue b <- pmatch b'
    pcon
      ( PValue $
          pmapUnionWith # plam (\a' b' -> pmapUnionWith # plam (+) # a' # b') # a # b
      )

-- | Sum of all value at input.
pvalueSpent :: Term s (PTxInfo :--> PValue)
pvalueSpent = phoistAcyclic $
  plam $ \txInfo' ->
    pmatch txInfo' $ \(PTxInfo txInfo) ->
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
        # (pfield @"inputs" # txInfo)

-- | Find the TxInInfo by a TxOutRef.
pfindTxInByTxOutRef :: Term s (PTxOutRef :--> PTxInfo :--> PMaybe PTxInInfo)
pfindTxInByTxOutRef = phoistAcyclic $
  plam $ \txOutRef txInfo' ->
    pmatch txInfo' $ \(PTxInfo txInfo) ->
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
        #$ (pfield @"inputs" # txInfo)

-- | True if a list is not empty.
pnotNull :: forall list a. PIsListLike list a => Term _ (list a :--> PBool)
pnotNull = phoistAcyclic $ plam $ pelimList (\_ _ -> pcon PTrue) (pcon PFalse)

--------------------------------------------------------------------------------
{- Functions which should (probably) not be upstreamed
   All of these functions are quite inefficient.
-}

-- | Check if any output matches the predicate.
anyOutput ::
  forall (datum :: PType) s.
  ( PIsData datum
  ) =>
  Term s (PTxInfo :--> (PValue :--> PAddress :--> datum :--> PBool) :--> PBool)
anyOutput = phoistAcyclic $
  plam $ \txInfo' predicate -> P.do
    txInfo <- pletFields @'["outputs"] txInfo'
    pany
      # plam
        ( \txOut'' -> P.do
            PTxOut txOut' <- pmatch (pfromData txOut'')
            txOut <- pletFields @'["value", "datumHash", "address"] txOut'
            PDJust dh <- pmatch txOut.datumHash
            pmatch (pfindDatum' @datum # (pfield @"_0" # dh) # txInfo') $ \case
              PJust datum -> P.do
                predicate # txOut.value # txOut.address # pfromData datum
              PNothing -> pcon PFalse
        )
      # pfromData txInfo.outputs

-- | Check if all outputs match the predicate.
allOutputs ::
  forall (datum :: PType) s.
  ( PIsData datum
  ) =>
  Term s (PTxInfo :--> (PTxOut :--> PValue :--> PAddress :--> datum :--> PBool) :--> PBool)
allOutputs = phoistAcyclic $
  plam $ \txInfo' predicate -> P.do
    txInfo <- pletFields @'["outputs"] txInfo'
    pall
      # plam
        ( \txOut'' -> P.do
            PTxOut txOut' <- pmatch (pfromData txOut'')
            txOut <- pletFields @'["value", "datumHash", "address"] txOut'
            PDJust dh <- pmatch txOut.datumHash
            pmatch (pfindDatum' @datum # (pfield @"_0" # dh) # txInfo') $ \case
              PJust datum -> P.do
                predicate # pfromData txOut'' # txOut.value # txOut.address # pfromData datum
              PNothing -> pcon PFalse
        )
      # pfromData txInfo.outputs

-- | Check if any (resolved) input matches the predicate.
anyInput ::
  forall (datum :: PType) s.
  ( PIsData datum
  ) =>
  Term s (PTxInfo :--> (PValue :--> PAddress :--> datum :--> PBool) :--> PBool)
anyInput = phoistAcyclic $
  plam $ \txInfo' predicate -> P.do
    txInfo <- pletFields @'["inputs"] txInfo'
    pany
      # plam
        ( \txInInfo'' -> P.do
            PTxInInfo txInInfo' <- pmatch (pfromData txInInfo'')
            let txOut'' = pfield @"resolved" # txInInfo'
            PTxOut txOut' <- pmatch (pfromData txOut'')
            txOut <- pletFields @'["value", "datumHash", "address"] txOut'
            PDJust dh <- pmatch txOut.datumHash
            pmatch (pfindDatum' @datum # (pfield @"_0" # dh) # txInfo') $ \case
              PJust datum -> P.do
                predicate # txOut.value # txOut.address # pfromData datum
              PNothing -> pcon PFalse
        )
      # pfromData txInfo.inputs

-- | Create a value with a single asset class.
psingletonValue :: forall s. Term s (PCurrencySymbol :--> PTokenName :--> PInteger :--> PValue)
psingletonValue = phoistAcyclic $
  plam $ \sym tok int ->
    let innerTup = pcon $ PMap $ psingleton #$ ppairDataBuiltin # pdata tok # pdata int
        outerTup = pcon $ PMap $ psingleton #$ ppairDataBuiltin # pdata sym # pdata innerTup
        res = pcon $ PValue outerTup
     in res

-- | Finds the TxOut of an effect from TxInfo and TxOutRef
findTxOutByTxOutRef :: Term s (PTxOutRef :--> PTxInfo :--> PMaybe PTxOut)
findTxOutByTxOutRef = phoistAcyclic $
  plam $ \txOutRef txInfo ->
    pmatch (pfindTxInByTxOutRef # txOutRef # txInfo) $ \case
      PJust ((pfield @"resolved" #) -> txOut) -> pcon $ PJust txOut
      PNothing -> pcon PNothing

-- | Get script hash from an Address.
scriptHashFromAddress :: Term s (PAddress :--> PMaybe PValidatorHash)
scriptHashFromAddress = phoistAcyclic $
  plam $ \addr ->
    pmatch (pfromData $ pfield @"credential" # addr) $ \case
      PScriptCredential ((pfield @"_0" #) -> h) -> pcon $ PJust h
      _ -> pcon PNothing

-- | Find all TxOuts sent to an Address
findOutputsToAddress :: Term s (PTxInfo :--> PAddress :--> PBuiltinList (PAsData PTxOut))
findOutputsToAddress = phoistAcyclic $
  plam $ \info address' -> P.do
    address <- plet $ pdata address'
    let outputs = pfromData $ pfield @"outputs" # info
        filteredOutputs =
          pfilter
            # plam
              (\(pfromData -> txOut) -> pfield @"address" # txOut #== address)
            # outputs
    filteredOutputs

-- | Find the data corresponding to a TxOut, if there is one
findTxOutDatum :: Term s (PTxInfo :--> PTxOut :--> PMaybe PDatum)
findTxOutDatum = phoistAcyclic $
  plam $ \info out -> P.do
    datumHash' <- pmatch $ pfromData $ pfield @"datumHash" # out
    case datumHash' of
      PDJust ((pfield @"_0" #) -> datumHash) -> pfindDatum # datumHash # info
      _ -> pcon PNothing

-- | Check if a particular asset class has been spent in the input list.
ptokenSpent :: forall {s :: S}. Term s (PAssetClass :--> PBuiltinList (PAsData PTxInInfo) :--> PBool)
ptokenSpent =
  plam $ \tokenClass inputs ->
    0
      #< pfoldr @PBuiltinList
        # plam
          ( \txInInfo' acc -> P.do
              PTxInInfo txInInfo <- pmatch (pfromData txInInfo')
              PTxOut txOut' <- pmatch $ pfromData $ pfield @"resolved" # txInInfo
              txOut <- pletFields @'["value"] txOut'
              let txOutValue = pfromData txOut.value
              acc + passetClassValueOf # txOutValue # tokenClass
          )
        # 0
        # inputs
