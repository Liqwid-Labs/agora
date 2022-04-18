{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Agora.Utils.Value (pgeq, pleq, pgt, plt) where

import Plutarch.Api.V1.AssocMap (PMap (PMap))
import Plutarch.Api.V1.These (PTheseData (..))
import Plutarch.Api.V1.Tuple (ptupleFromBuiltin)
import Plutarch.Api.V1.Value
import Plutarch.Lift (PUnsafeLiftDecl)
import Plutarch.List (pconvertLists)
import Plutarch.Monadic qualified as P

import Agora.Utils

passocMapUnion :: forall k v r s. PIsData v => Term s (PMap k v :--> PMap k r :--> PMap k (PTheseData v r))
passocMapUnion = undefined

punionVal ::
  Term
    s
    ( PValue
        :--> PValue
        :--> PMap
              PCurrencySymbol
              (PMap PTokenName (PTheseData PInteger PInteger))
    )
punionVal = plam $ \x' y' -> P.do
  PValue x <- pmatch x'
  PValue y <- pmatch y'
  let combined = passocMapUnion # x # y
      unThese :: Term s ((PTheseData (PMap PTokenName PInteger) (PMap PTokenName PInteger))
                         :--> PMap PTokenName (PTheseData PInteger PInteger))
      unThese = plam $ \k' ->
        pmatch k' $ \case
          PDThis (pfromData . (pfield @"_0" #) -> a') -> P.do
            PMap a <- pmatch a'
            pcon $ PMap $ pmap # plam (\v -> PBuiltinPair (pfstBuiltin # v) (psndBuiltin # v)) # a
          PDThat (pfromData . (pfield @"_0" #) -> b) -> undefined
          PDThese (pletFields @'["_0", "_1"] -> d) -> passocMapUnion # d._0 # d._1
  combined

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

-- | Establishes if a value is less than or equal to another.
pleq :: Term s (PValue :--> PValue :--> PBool)
pleq = plam $ \v0 v1 -> (pcheckBinRel # pleq') # v0 # v1

pleq' :: Term s (PInteger :--> PInteger :--> PBool)
pleq' = plam $ \m n -> m #<= n

-- | Establishes if a value is strictly less than another.
plt :: Term s (PValue :--> PValue :--> PBool)
plt = plam $ \v0 v1 -> (pcheckBinRel # plt') # v0 # v1

plt' :: Term s (PInteger :--> PInteger :--> PBool)
plt' = plam $ \m n -> m #< n

-- | Establishes if a value is greater than or equal to another.
pgeq :: Term s (PValue :--> PValue :--> PBool)
pgeq = plam $ \v0 v1 -> pnot #$ plt # v0 # v1

-- | Establishes if a value is strictly greater than another.
pgt :: Term s (PValue :--> PValue :--> PBool)
pgt = plam $ \v0 v1 -> pnot #$ pleq # v0 # v1
