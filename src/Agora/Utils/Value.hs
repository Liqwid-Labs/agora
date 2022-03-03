{-# OPTIONS_GHC -Wwarn #-}

module Agora.Utils.Value where

import GHC.Generics qualified as GHC
import Generics.SOP
import Plutarch.Api.V1.AssocMap (PMap (PMap))
import Plutarch.Api.V1.Tuple (PTuple, ptupleFromBuiltin)
import Plutarch.Api.V1.Value (PCurrencySymbol, PTokenName, PValue)
import Plutarch.DataRepr (PIsDataReprInstances (PIsDataReprInstances))
import Plutarch.Lift (PLifted, PUnsafeLiftDecl)
import Plutarch.Monadic qualified as P
import Plutus.V1.Ledger.Api qualified as Plutus
import PlutusTx.These qualified as PlutusThese

-- data PThese (a :: PType) (b :: PType) (s :: S)
--   = PThis (Term s a)
--   | PThat (Term s b)
--   | PThese (Term s a) (Term s b)
--   deriving stock (GHC.Generic)
--   deriving anyclass (Generic, PlutusType)

data PTheseData (a :: PType) (b :: PType) (s :: S)
  = PDThis (Term s (PDataRecord '["_0" ':= a]))
  | PDThat (Term s (PDataRecord '["_0" ':= b]))
  | PDThese (Term s (PDataRecord '["_0" ':= a, "_1" ':= b]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances (PTheseData a b)

instance
  ( Plutus.ToData (PLifted a)
  , Plutus.ToData (PLifted b)
  , Plutus.FromData (PLifted a)
  , Plutus.FromData (PLifted b)
  , PLift a
  , PLift b
  ) =>
  PUnsafeLiftDecl (PTheseData a b)
  where
  type PLifted (PTheseData a b) = PlutusThese.These (PLifted a) (PLifted b)

punionVal ::
  Term
    s
    ( PValue
        :--> PValue
        :--> PMap
              PCurrencySymbol
              (PMap PTokenName (PTheseData PInteger PInteger))
    )
punionVal = undefined

-- | Determines if a condition is true for all values in a map.
pmapAll ::
  (PUnsafeLiftDecl v, PIsData v) =>
  Term s ((v :--> PBool) :--> PMap k v :--> PBool)
pmapAll = plam $ \f m -> P.do
  PMap builtinMap <- pmatch $ m

  let getV :: PIsData v => Term s (PBuiltinPair (PAsData k) (PAsData v) :--> v)
      getV = plam $ \bip -> P.do
        let tuple = pfromData $ ptupleFromBuiltin (pdata bip)
        pfield @"_1" # tuple

  let vs = pmap # getV # builtinMap
  pall # f # vs

pcheckPred ::
  forall {s :: S}.
  Term
    s
    ( (PTheseData PInteger PInteger :--> PBool)
        :--> PValue
        :--> PValue
        :--> PBool
    )
pcheckPred = plam $ \f l r -> P.do
  let inner :: Term s (PMap PTokenName (PTheseData PInteger PInteger) :--> PBool)
      inner = pmapAll # f
  pmapAll # inner # (punionVal # l # r)

pcheckBinRel ::
  forall {s :: S}.
  Term
    s
    ( (PInteger :--> PInteger :--> PBool)
        :--> PValue
        :--> PValue
        :--> PBool
    )
pcheckBinRel = plam $ \f l r -> P.do
  let unThese :: Term s (PTheseData PInteger PInteger :--> PBool)
      unThese = plam $ \k' ->
        pmatch k' $ \case
          PDThis r -> f # (pfield @"_0" # r) # 0
          PDThat r -> f # 0 # (pfield @"_0" # r)
          PDThese r -> f # (pfield @"_0" # r) # (pfield @"_1" # r)
  pcheckPred # unThese # l # r

pleq :: Term s (PValue :--> PValue :--> PBool)
pleq = plam $ \v0 v1 -> (pcheckBinRel # pleq') # v0 # v1

pleq' :: Term s (PInteger :--> PInteger :--> PBool)
pleq' = plam $ \m n -> m #<= n
