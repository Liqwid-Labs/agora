{-# OPTIONS_GHC -Wwarn #-}

module Spec.Model.Treasury (
  ) where

import Apropos (
  Apropos (Apropos),
  Enumerable (enumerated),
  Formula (
    ExactlyOne,
    Not,
    Var,
    Yes,
    (:&&:),
    (:->:)
  ),
  Gen,
  HasLogicalModel (satisfiesProperty),
  HasParameterisedGenerator (parameterisedGenerator),
  LogicalModel (logic),
  runGeneratorTestsWhere,
  (:+),
 )
import Apropos.Script (HasScriptRunner (expect, runScriptTestsWhere, script))
import Data.Set (Set)
import Plutus.V1.Ledger.Api (
  CurrencySymbol,
  ScriptContext (scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Minting),
  TxInfo (txInfoMint),
  Value,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

{-

A Treasury transaction should pass if:

1. A GAT is burned.
2. All GATs are valid.

If either of these things do _not_ hold, then the transaction
should fail.

-}

data TreasuryTxProp
  = GATIsBurned
  | GATIsNotBurned
  | AllGATsValid
  | SomeGATsInvalid
  | ScriptPurposeIsNotMinting
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data TreasuryTxModel = TreasuryTxModel
  { gatCs :: CurrencySymbol
  , ctx :: ScriptContext
  }

instance Enumerable TreasuryTxProp where
  enumerated :: [TreasuryTxProp]
  enumerated = [minBound .. maxBound]

instance LogicalModel TreasuryTxProp where
  logic :: Formula TreasuryTxProp
  logic =
    ExactlyOne [Var GATIsBurned, Var GATIsNotBurned]
      :&&: Var SomeGATsInvalid :->: Not (Var AllGATsValid)

instance HasLogicalModel TreasuryTxProp TreasuryTxModel where
  satisfiesProperty :: TreasuryTxProp -> TreasuryTxModel -> Bool
  satisfiesProperty prop model =
    let purpose = model.ctx.scriptContextPurpose :: ScriptPurpose
        txInfo = model.ctx.scriptContextTxInfo :: TxInfo
        amountMinted = txInfo.txInfoMint :: Value
     in case prop of
          ScriptPurposeIsNotMinting -> case purpose of
            Minting _ -> False
            _ -> True
          _ -> undefined

-- instance HasParameterisedGenerator TreasuryTxProp Int where
--   parameterisedGenerator :: Set TreasuryTxProp -> Gen Int
--   parameterisedGenerator = undefined

-- instance HasScriptRunner TreasuryTxProp Int where
--   expect = undefined
--   script = undefined

-- genTests :: TestTree
-- genTests =
--   testGroup "genTests" $
--     fromGroup
--       <$> [ runGeneratorTestsWhere
--               (Apropos :: Int :+ TreasuryTxProp)
--               "Generator"
--               Yes
--           ]

-- plutarchTests :: TestTree
-- plutarchTests =
--   testGroup "plutarchTests" $
--     fromGroup
--       <$> [ runScriptTestsWhere
--               (Apropos :: Int :+ TreasuryTxProp)
--               "ScriptValid"
--               Yes
--           ]
