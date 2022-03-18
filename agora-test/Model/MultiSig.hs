module Model.MultiSig () where

import Agora.MultiSig (MultiSig (..))
import Apropos (Apropos (Apropos), Formula (ExactlyOne), (:+))
import Apropos.Script (HasScriptRunner (expect, script))
import Plutus.V1.Ledger.Api (PubKeyHash, Script)

{-

1. Create proposition sum type.
2. Create logical model defining relationships between propositions.
3. Associating propositions with the "concrete" type i.e. MultiSig.
4. Create Generators.
5. Run tests (with magic).

-}

{-

1. Create a

Define a prop, as if it is the way a script can pass.
  1. keys signed exceeds `minSigs`
  2. `minSigs` is lte zero.

Props not passing:
  1. No signatures present.
  2. Signatures present is less than `minSigs`.

-}

data MultiSigModel = MultiSigModel
  { ms :: MultiSig
  , ctx :: ScriptContext
  }

data MultiSigProp
  = MeetsMinSigs
  | DoesNotMeetMinSigs

instance LogicalModel MultiSigModel where
  logic = ExactlyOne [Var MeetsMinSigs, Var DoesNotMeetMinSigs]

instance HasLogicalModel MultiSigProp MultiSigModel where
  satisfiesProperty :: MultiSigProp -> MultiSigModel -> Bool
  satisfiesProperty p m =
    let minSigs = m.ms.minSigs
        signatories = m.ctx.txInfo.txInfoSignatories
        matchingSigs = intersect m.ms.keys signatories
     in case p of
          MeetsMinSigs -> length matchingSigs >= minSigs
          DoesNotMeetMinSigs -> length matchingSigs < minSigs

instance HasScriptRunner MultiSigProp MultiSig where
  expect :: (MultiSigModel :+ MultiSigProp) -> Formula MultiSigProp
  expect = undefined

  script :: (MultiSigModel :+ MultiSigProp) -> MultiSig -> Script
  script Apropos msm = compile $ validatedByMultisig msm . ms
