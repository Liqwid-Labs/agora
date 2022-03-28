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
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Plutus.V1.Ledger.Address (Address (addressCredential))
import Plutus.V1.Ledger.Contexts (
  ScriptContext (scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Minting),
  TxInfo (txInfoMint, txInfoOutputs),
  TxOut (txOutAddress, txOutValue),
 )
import Plutus.V1.Ledger.Credential (Credential (PubKeyCredential, ScriptCredential))
import Plutus.V1.Ledger.Scripts (ValidatorHash (ValidatorHash))
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName (unTokenName), Value (getValue))
import PlutusTx.AssocMap (Map, elems, keys)
import PlutusTx.AssocMap qualified as AssocMap (all, lookup)
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
  deriving stock (Show)

instance Enumerable TreasuryTxProp where
  enumerated :: [TreasuryTxProp]
  enumerated = [minBound .. maxBound]

instance LogicalModel TreasuryTxProp where
  logic :: Formula TreasuryTxProp
  logic =
    ExactlyOne [Var GATIsBurned, Var GATIsNotBurned]
      :&&: Var SomeGATsInvalid :->: Not (Var AllGATsValid)

isMinting :: ScriptPurpose -> Bool
isMinting (Minting _) = True
isMinting _ = False

authorityTokensValidIn :: CurrencySymbol -> TxOut -> Bool
authorityTokensValidIn cs out =
  let add = out.txOutAddress :: Address
      outValue = out.txOutValue :: Value

      tokenMap :: Maybe (Map TokenName Integer)
      tokenMap = AssocMap.lookup cs $ getValue outValue

      cred = add.addressCredential :: Credential

      validCred :: Map TokenName Integer -> Bool
      validCred m = case cred of
        PubKeyCredential _ -> False
        ScriptCredential (ValidatorHash vh) ->
          all (\tn -> vh == unTokenName tn) $ keys m
   in case tokenMap of
        Nothing -> True
        Just m -> validCred m

instance HasLogicalModel TreasuryTxProp TreasuryTxModel where
  satisfiesProperty :: TreasuryTxProp -> TreasuryTxModel -> Bool
  satisfiesProperty prop model =
    let purpose = model.ctx.scriptContextPurpose :: ScriptPurpose
        txInfo = model.ctx.scriptContextTxInfo :: TxInfo
        amountMinted = txInfo.txInfoMint :: Value

        csValue :: Maybe (Map TokenName Integer)
        csValue = AssocMap.lookup model.gatCs (getValue amountMinted)

        csValueSum :: Integer
        csValueSum = case csValue of
          Nothing -> 0
          Just m -> sum $ elems m
     in case prop of
          GATIsBurned -> csValueSum <= -1
          GATIsNotBurned -> csValueSum >= 0
          AllGATsValid ->
            all
              (authorityTokensValidIn model.gatCs)
              txInfo.txInfoOutputs
          SomeGATsInvalid ->
            any
              (not . authorityTokensValidIn model.gatCs)
              txInfo.txInfoOutputs
          ScriptPurposeIsNotMinting -> not $ isMinting purpose

instance HasParameterisedGenerator TreasuryTxProp TreasuryTxModel where
  parameterisedGenerator :: Set TreasuryTxProp -> Gen TreasuryTxModel
  parameterisedGenerator = undefined

instance HasScriptRunner TreasuryTxProp TreasuryTxModel where
  expect = undefined
  script = undefined

genTests :: TestTree
genTests =
  testGroup "genTests" $
    fromGroup
      <$> [ runGeneratorTestsWhere
              (Apropos :: TreasuryTxModel :+ TreasuryTxProp)
              "Generator"
              Yes
          ]

plutarchTests :: TestTree
plutarchTests =
  testGroup "plutarchTests" $
    fromGroup
      <$> [ runScriptTestsWhere
              (Apropos :: TreasuryTxModel :+ TreasuryTxProp)
              "ScriptValid"
              Yes
          ]
