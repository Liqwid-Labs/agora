module Model.MultiSig (plutarchTests, genTests) where

import Agora.MultiSig (MultiSig (..), validatedByMultisig)
import Apropos (Apropos (Apropos), Formula (ExactlyOne, Var, Yes), HasLogicalModel (..), HasParameterisedGenerator, LogicalModel (logic), parameterisedGenerator, runGeneratorTestsWhere, (:+))
import Apropos.Gen (Gen, choice, int, linear, list)
import Apropos.LogicalModel (Enumerable)
import Apropos.LogicalModel.Enumerable (Enumerable (enumerated))
import Apropos.Script (HasScriptRunner (expect, runScriptTestsWhere, script))
import Data.List (intersect)
import Plutarch (compile)
import Plutus.V1.Ledger.Api (
  Script,
  ScriptContext (scriptContextPurpose),
  ScriptPurpose (Spending),
  TxInfo (txInfoDCert, txInfoData, txInfoFee, txInfoId, txInfoInputs, txInfoMint, txInfoOutputs, txInfoValidRange, txInfoWdrl),
  TxOutRef (TxOutRef),
  scriptContextTxInfo,
  txInfoSignatories,
 )
import Plutus.V1.Ledger.Contexts (ScriptContext (ScriptContext), TxInfo (TxInfo))
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Value qualified as Value
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

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
  deriving stock (Eq, Show)

data MultiSigProp
  = MeetsMinSigs
  | DoesNotMeetMinSigs
  deriving stock (Eq, Show, Ord)

instance Enumerable MultiSigProp where
  enumerated = [MeetsMinSigs, DoesNotMeetMinSigs]

instance LogicalModel MultiSigProp where
  logic = ExactlyOne [Var MeetsMinSigs, Var DoesNotMeetMinSigs]

instance HasLogicalModel MultiSigProp MultiSigModel where
  satisfiesProperty :: MultiSigProp -> MultiSigModel -> Bool
  satisfiesProperty p m =
    let minSigs = m.ms.minSigs
        signatories = txInfoSignatories $ scriptContextTxInfo $ m.ctx
        matchingSigs = intersect m.ms.keys signatories
     in case p of
          MeetsMinSigs -> length matchingSigs >= fromInteger minSigs
          DoesNotMeetMinSigs -> length matchingSigs < fromInteger minSigs

contextWithSignatures :: [PubKeyHash] -> ScriptContext
contextWithSignatures sigs =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs = []
          , txInfoOutputs = []
          , txInfoFee = Value.singleton "" "" 2
          , txInfoMint = mempty
          , txInfoDCert = []
          , txInfoWdrl = []
          , txInfoValidRange = Interval.always
          , txInfoSignatories = sigs
          , txInfoData = []
          , txInfoId = "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
          }
    , scriptContextPurpose = Spending (TxOutRef "" 0)
    }

genPK :: Gen PubKeyHash
genPK =
  choice
    [ pure "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be7401214142019c"
    , pure "0b12051dd2da4b3629cebb92e2be111e0e99c63c04727ed55b74a296"
    , pure "87f5f31e4d7437463cd901c4c9edb7a51903ac858661503e9d72f492"
    , pure "f74ccaee8244264b3c73fce3b66bd2337de3db70efff4261d6ff145b"
    ]

instance HasParameterisedGenerator MultiSigProp MultiSigModel where
  parameterisedGenerator s = do
    expectedSignatures <- list (linear 1 4) genPK
    minSigs <- toInteger <$> int (linear 1 (length expectedSignatures))
    let msig = MultiSig expectedSignatures minSigs

    actualSignaturesLength <-
      if MeetsMinSigs `elem` s
        then int (linear (fromInteger minSigs) (length expectedSignatures))
        else pure 0
    let actualSignatures = take actualSignaturesLength expectedSignatures

    let ctx = contextWithSignatures actualSignatures
    pure (MultiSigModel msig ctx)

instance HasScriptRunner MultiSigProp MultiSigModel where
  expect :: (MultiSigModel :+ MultiSigProp) -> Formula MultiSigProp
  expect Apropos = Var MeetsMinSigs

  script :: (MultiSigModel :+ MultiSigProp) -> MultiSigModel -> Script
  script Apropos msm =
    compile $
      pif
        (validatedByMultisig msm.ms # pconstant msm.ctx.scriptContextTxInfo)
        (pcon PUnit)
        perror

genTests :: TestTree
genTests =
  testGroup "genTests" $
    fromGroup
      <$> [ runGeneratorTestsWhere (Apropos :: MultiSigModel :+ MultiSigProp) "Generator" Yes
          ]

plutarchTests :: TestTree
plutarchTests =
  testGroup "plutarchTests" $
    fromGroup
      <$> [ runScriptTestsWhere (Apropos :: MultiSigModel :+ MultiSigProp) "ScriptValid" Yes
          ]
