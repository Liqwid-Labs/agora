module Agora.Utils.BSM (pmkDatumTree, plookup, mustFindDatum') where

import Agora.Utils (mustBePDJust, mustBePJust)
import Agora.Utils.BST qualified as BST
import Plutarch.Api.V1 (PDatum, PTuple)
import Plutarch.Api.V1.Maybe (PMaybeData)
import Plutarch.Api.V1.Scripts (PDatumHash)
import Plutarch.Builtin (pforgetData)
import Plutarch.Extra.Functor (pfmap)
import Plutarch.Extra.TermCont (pmatchC)
import Prelude hiding (pnull)

type PMap k v = BST.PTree (PPair k v)

pmkDatumTree :: Term s (PBuiltinList (PAsData (PTuple PDatumHash PDatum)) :--> PMap PDatumHash PDatum)
pmkDatumTree =
  phoistAcyclic $
    plam $ \l ->
      pfoldr
        # plam
          ( \(pfromData -> t) bst ->
              let k = pfromData $ pfield @"_0" # t
                  v = pfromData $ pfield @"_1" # t
                  p = pcon $ PPair k v
               in BST.pinsertBy # lt # p # bst
          )
        # BST.pempty
        # l
  where
    lt :: POrd k => Term s (PPair k v :--> PPair k v :--> PBool)
    lt = phoistAcyclic $
      plam $ \pa pb -> unTermCont $ do
        (PPair k1 _) <- pmatchC pa
        (PPair k2 _) <- pmatchC pb
        pure $ k1 #< k2

plookup :: forall k v s. POrd k => Term s (k :--> PMap k v :--> PMaybe v)
plookup = phoistAcyclic $
  plam $ \k t ->
    let kv = BST.psearchBy # eq # lt # k # t
     in pfmap # getV # kv
  where
    wrap :: Term _ ((k :--> k :--> PBool) :--> k :--> PPair k v :--> PBool)
    wrap = phoistAcyclic $
      plam $ \f k p -> pmatch p $ \case
        PPair k' _ -> f # k # k'

    lt :: Term _ (k :--> PPair k v :--> PBool)
    lt = phoistAcyclic $ wrap #$ phoistAcyclic $ plam (#<)

    eq :: Term _ (k :--> PPair k v :--> PBool)
    eq = phoistAcyclic $ wrap #$ phoistAcyclic $ plam (#==)

    getV :: Term _ (PPair k v :--> v)
    getV = phoistAcyclic $
      plam $ \kv -> pmatch kv $ \case
        PPair _ v -> v

mustFindDatum' ::
  forall (datum :: PType).
  (PIsData datum, PTryFrom PData (PAsData datum)) =>
  forall s.
  Term
    s
    ( PMaybeData PDatumHash
        :--> PMap PDatumHash PDatum
        :--> datum
    )
mustFindDatum' = phoistAcyclic $
  plam $ \mdh t -> unTermCont $ do
    let dh = mustBePDJust # "Given TxOut dones't have a datum" # mdh
        dt = mustBePJust # "Datum not found in the transaction" #$ plookup # dh # t
    (d, _) <- tcont $ ptryFrom $ pforgetData $ pdata dt
    pure $ pfromData d