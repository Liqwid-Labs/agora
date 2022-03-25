{- |
Module     : Spec.Model.MultiSig
Maintainer : emi@haskell.fyi
Description: apropos-tx model and tests for 'MultiSig' functions

apropos-tx model and tests for 'MultiSig' functions
-}
module Spec.Model.MultiSig (
  plutarchTests,
  genTests,
) where

import Data.List (intersect)

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Api (
  Script,
  ScriptContext (scriptContextPurpose),
  ScriptPurpose (Spending),
  TxInfo (
    txInfoDCert,
    txInfoData,
    txInfoFee,
    txInfoId,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoValidRange,
    txInfoWdrl
  ),
  TxOutRef (TxOutRef),
  scriptContextTxInfo,
  txInfoSignatories,
 )
import Plutus.V1.Ledger.Contexts (ScriptContext (ScriptContext), TxInfo (TxInfo))
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Value qualified as Value

--------------------------------------------------------------------------------

import Apropos (
  Apropos (Apropos),
  Formula (ExactlyOne, Var, Yes),
  HasLogicalModel (..),
  HasParameterisedGenerator,
  LogicalModel (logic),
  parameterisedGenerator,
  runGeneratorTestsWhere,
  (:+),
 )
import Apropos.Gen (Gen, choice, int, linear, list)
import Apropos.LogicalModel (Enumerable)
import Apropos.LogicalModel.Enumerable (Enumerable (enumerated))
import Apropos.Script (HasScriptRunner (expect, runScriptTestsWhere, script))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

--------------------------------------------------------------------------------

import Agora.MultiSig (MultiSig (..), validatedByMultisig)

--------------------------------------------------------------------------------

-- | apropos model for testing multisigs.
data MultiSigModel = MultiSigModel
  { ms :: MultiSig
  -- ^ `MultiSig` value to be tested.
  , ctx :: ScriptContext
  -- ^ The `ScriptContext` of the transaction.
  }
  deriving stock (Eq, Show)

-- | Propositions that may hold true of a `MultiSigModel`.
data MultiSigProp
  = -- | Sufficient number of signatories in the script context.
    MeetsMinSigs
  | -- | Insufficient number of signatories in the script context.
    DoesNotMeetMinSigs
  deriving stock (Eq, Show, Ord)

instance Enumerable MultiSigProp where
  enumerated = [MeetsMinSigs, DoesNotMeetMinSigs]

instance LogicalModel MultiSigProp where
  -- Only logical relationship between the two propositions is
  -- that exactly one of them holds for a given model.
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

{- | Given a list of key hashes, returns a dummy `ScriptContext`,
     with those hashes as signatories.
-}
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

-- | Generator returning one of four dummy public key hashes.
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
    -- Gen between one and four signatures for the `MultiSig`.
    expectedSignatures <- list (linear 1 4) genPK

    -- Gen the value of `MultiSig.minSigs`.
    minSigs <- toInteger <$> int (linear 1 (length expectedSignatures))

    -- Assign values to msig.
    let msig = MultiSig expectedSignatures minSigs

    actualSignaturesLength <-
      -- If we would like to generate a MultiSig model which passes...
      if MeetsMinSigs `elem` s
        then -- ... have a sufficient number of signatories.
          int (linear (fromInteger minSigs) (length expectedSignatures))
        else -- ... have zero signatories.
          pure 0

    -- Get a list of signatories for the script context.
    let actualSignatures = take actualSignaturesLength expectedSignatures

    let ctx = contextWithSignatures actualSignatures

    -- Return the generated model.
    pure (MultiSigModel msig ctx)

instance HasScriptRunner MultiSigProp MultiSigModel where
  -- When the script runs, we want the model to meet the minimum signatures.
  expect :: (MultiSigModel :+ MultiSigProp) -> Formula MultiSigProp
  expect Apropos = Var MeetsMinSigs

  -- Function making a valid script from the model and propositions.
  script :: (MultiSigModel :+ MultiSigProp) -> MultiSigModel -> Script
  script Apropos msm =
    compile $
      pif
        (validatedByMultisig msm.ms # pconstant msm.ctx.scriptContextTxInfo)
        (pcon PUnit)
        perror

-- | Consistency tests for the 'HasParameterisedGenerator' instance of 'MultiSigModel'
genTests :: TestTree
genTests =
  testGroup "genTests" $
    fromGroup
      <$> [ runGeneratorTestsWhere
              (Apropos :: MultiSigModel :+ MultiSigProp)
              "Generator"
              Yes
          ]

-- | Tests for the 'HasScriptRunner' instance of 'MultiSigModel'
plutarchTests :: TestTree
plutarchTests =
  testGroup "plutarchTests" $
    fromGroup
      <$> [ runScriptTestsWhere
              (Apropos :: MultiSigModel :+ MultiSigProp)
              "ScriptValid"
              Yes
          ]
