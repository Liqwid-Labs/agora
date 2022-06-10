module Agora.Utils.BST (
  PTree (..),
  pempty,
  pinsertBy,
  pinsertBy',
  pnull,
  psearchBy,
  fromListBy',
) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, HasDatatypeInfo, I (..))
import Prelude hiding (pnull)

data PTree (a :: PType) (s :: S)
  = PLeaf
  | PNode
      (Term s (PTree a))
      (Term s a)
      (Term s (PTree a))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType, HasDatatypeInfo, PEq)

pempty :: Term s (PTree a)
pempty = phoistAcyclic $ pcon PLeaf

pnull :: Term s (PTree a :--> PBool)
pnull = phoistAcyclic $
  plam $ \t -> pmatch t $ \case
    PLeaf -> pcon PTrue
    _ -> pcon PFalse

psearchBy ::
  Term
    s
    ( (b :--> a :--> PBool)
        :--> (b :--> a :--> PBool)
        :--> b
        :--> PTree a
        :--> PMaybe a
    )
psearchBy = phoistAcyclic $
  pfix #$ plam $ \self' eq lt x t ->
    pmatch t $ \case
      PLeaf -> pcon PNothing
      PNode l y r ->
        plet (self' # eq # lt # x) $ \self ->
          pif
            (eq # x # y)
            (pcon $ PJust y)
            $ pif
              (lt # x # y)
              (self # l)
              (self # r)

pinsertBy ::
  Term
    s
    ( (a :--> a :--> PBool)
        :--> a
        :--> PTree a
        :--> PTree a
    )
pinsertBy = phoistAcyclic $
  pfix #$ plam $ \self' lt x t ->
    pmatch t $ \case
      PLeaf -> pcon $ PNode pempty x pempty
      PNode l y r ->
        plet (self' # lt # x) $ \self ->
          pif
            (lt # x # y)
            (pcon $ PNode (self # l) y r)
            (pcon $ PNode l y (self # r))

pinsertBy' ::
  Term
    s
    ( (a :--> a :--> PBool)
        :--> (a :--> a :--> PBool)
        :--> a
        :--> PTree a
        :--> PTree a
    )
pinsertBy' = phoistAcyclic $
  pfix #$ plam $ \self' eq lt x t ->
    pmatch t $ \case
      PLeaf -> pcon $ PNode pempty x pempty
      PNode l y r ->
        plet (self' # eq # lt # x) $ \self ->
          pif (eq # x # y) (pcon $ PNode l x r) $
            pif
              (lt # x # y)
              (pcon $ PNode (self # l) y r)
              (pcon $ PNode l y (self # r))

fromListBy' ::
  (PIsListLike l a) =>
  Term
    s
    ( (a :--> a :--> PBool)
        :--> (a :--> a :--> PBool)
        :--> l a
        :--> PTree a
    )
fromListBy' = phoistAcyclic $
  plam $ \eq lt l ->
    plet (pinsertBy' # eq # lt) $ \ins ->
      pfoldr # ins # pempty # l
