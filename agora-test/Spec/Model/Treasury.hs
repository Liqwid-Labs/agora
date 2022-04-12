{-# OPTIONS_GHC -Wwarn #-}

{- |
Module: Spec.Model.Treasury
Description: `apropos-tx` tests for Treasury validator.
Maintainer: jack@mlabs.city

This module contains `apropos-tx` tests for ensuring that
the `Agora.Treasury` validator acts as desired. Notes on desired
behaviour and invluded in this description.

A Treasury transaction should pass if:

  1. A GAT is burned.

  2. All GATs are valid.

  3. The script purpose is Minting.

If either of these things do /not/ hold, then the transaction
should fail.
-}
module Spec.Model.Treasury (
  plutarchTests,
  genTests,
) where

import Agora.Treasury (
  PTreasuryDatum (PTreasuryDatum),
  PTreasuryRedeemer (PAlterTreasuryParams),
  treasuryValidator,
 )
import Apropos (
  Apropos (Apropos),
  Contract,
  Enumerable (enumerated),
  Formula (
    Not,
    Some,
    Var,
    Yes,
    (:&&:)
  ),
  Gen,
  HasLogicalModel (satisfiesProperty),
  HasParameterisedGenerator (parameterisedGenerator),
  HasPermutationGenerator (buildGen, generators),
  LogicalModel (logic),
  Morphism (Morphism, contract, match, morphism, name),
  add,
  runGeneratorTestsWhere,
  (:+),
 )
import Apropos.Gen.Contexts (scriptContext, txInInfo)
import Apropos.Gen.Value (currencySymbol)
import Apropos.Script (ScriptModel (expect, runScriptTestsWhere, script))
import Data.Set (Set)
import Plutarch.Api.V1 (PCurrencySymbol, PScriptContext)
import Plutarch.Builtin (pforgetData)
import Plutus.V1.Ledger.Address (Address (addressCredential))
import Plutus.V1.Ledger.Contexts (
  ScriptContext (scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Minting),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoInputs, txInfoMint, txInfoOutputs),
  TxOut (txOutAddress, txOutValue),
 )
import Plutus.V1.Ledger.Credential (Credential (PubKeyCredential, ScriptCredential))
import Plutus.V1.Ledger.Scripts (Script, ValidatorHash (ValidatorHash))
import Plutus.V1.Ledger.Value (
  CurrencySymbol (CurrencySymbol),
  TokenName (unTokenName),
  Value (Value, getValue),
 )
import PlutusTx.AssocMap (Map, elems, fromList, keys, singleton, toList, unionWith)
import PlutusTx.AssocMap qualified as AssocMap (lookup)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data TreasuryTxProp
  = GATIsBurned
  | AllGATsValid
  | ScriptPurposeIsMinting
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance LogicalModel TreasuryTxProp where
  logic :: Formula TreasuryTxProp
  logic =
    Some
      [ Var GATIsBurned
      , Var AllGATsValid
      , Var ScriptPurposeIsMinting
      ]

data TreasuryTxModel = TreasuryTxModel
  { gatCs :: CurrencySymbol
  , ctx :: ScriptContext
  }
  deriving stock (Show)

instance Enumerable TreasuryTxProp where
  enumerated :: [TreasuryTxProp]
  enumerated = [minBound .. maxBound]

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
          GATIsBurned -> csValueSum == -1
          AllGATsValid ->
            all
              (authorityTokensValidIn model.gatCs . txInInfoResolved)
              txInfo.txInfoInputs
          ScriptPurposeIsMinting -> isMinting purpose

instance HasParameterisedGenerator TreasuryTxProp TreasuryTxModel where
  parameterisedGenerator :: Set TreasuryTxProp -> Gen TreasuryTxModel
  parameterisedGenerator = buildGen baseGen
    where
      baseGen :: Gen TreasuryTxModel
      baseGen = do
        cs <- currencySymbol
        ctx <- scriptContext
        return $ TreasuryTxModel cs ctx

{- | Updates the `Integer` and `TokenName` for a given
     `CurrencySymbol` for a given value.
-}
replaceValue ::
  -- | The value whose entry to update.
  Value ->
  -- | The currency symbol of the entry to update.
  CurrencySymbol ->
  -- | The token name of the entry to place in the new value.
  TokenName ->
  -- | The number of tokens to place in the new value.
  Integer ->
  -- | The updated value.
  Value
replaceValue (Value v) cs tn n = Value $ unionWith (\_ x -> x) v v'
  where
    v' :: Map CurrencySymbol (Map TokenName Integer)
    v' = singleton cs $ singleton tn n

kmap :: (k -> k') -> Map k v -> Map k' v
kmap g = fromList . fmap (\(x, y) -> (g x, y)) . toList

fixTokenNames :: TxInInfo -> TxInInfo
fixTokenNames inf =
  let cred = inf.txInInfoResolved.txOutAddress.addressCredential
      val = inf.txInInfoResolved.txOutValue
   in case cred of
        PubKeyCredential _ -> inf
        ScriptCredential (ValidatorHash bs) -> undefined

-- | TODO: Define.
instance HasPermutationGenerator TreasuryTxProp TreasuryTxModel where
  generators :: [Morphism TreasuryTxProp TreasuryTxModel]
  generators =
    [ Morphism
        { name = "Ensure GAT is burned"
        , match = Not $ Var GATIsBurned
        , contract = add GATIsBurned
        , morphism = \m ->
            let ctx' = m.ctx
                txInfo = ctx'.scriptContextTxInfo
                mint = txInfo.txInfoMint
                newMint = replaceValue mint m.gatCs "gat" (-1)
             in return
                  m
                    { ctx =
                        ctx'
                          { scriptContextTxInfo =
                              txInfo
                                { txInfoMint = newMint
                                }
                          }
                    }
        }
    , Morphism
        { name = "Ensure all GATs are valid"
        , match = Not $ Var AllGATsValid
        , contract = add AllGATsValid
        , {- For every GAT to be considered "valid", their
             `TokenName`s have to be equal to the address
             of their script. To represent this as a `Morphism`:

             - FOR every UTXO input in the transaction:
               - FOR every value in the input:
                 - IF the currency symbol matches the recognised
                   GAT currency symbol:
                     - THEN: set the `TokenName` to be equal to
                       the UTXO's address.
                 - ELSE: ignore it.
          -}
          morphism = \m ->
            let ctx' = m.ctx
                txInfo = ctx'.scriptContextTxInfo
                infoInputs :: [TxInInfo] = txInfo.txInfoInputs
             in return $
                  m
                    { ctx =
                        ctx'
                          { scriptContextTxInfo =
                              txInfo
                                { txInfoInputs =
                                    fixTokenNames <$> infoInputs
                                }
                          }
                    }
        }
    , Morphism
        { name = "Ensure script purpose is minting"
        , match = Not $ Var ScriptPurposeIsMinting
        , contract = add ScriptPurposeIsMinting
        , morphism = \m ->
            return
              m
                { ctx =
                    m.ctx
                      { scriptContextPurpose = Minting m.gatCs
                      }
                }
        }
    ]

instance ScriptModel TreasuryTxProp TreasuryTxModel where
  expect :: (TreasuryTxModel :+ TreasuryTxProp) -> Formula TreasuryTxProp
  expect _ =
    Var GATIsBurned
      :&&: Var AllGATsValid
      :&&: Var ScriptPurposeIsMinting

  script :: (TreasuryTxModel :+ TreasuryTxProp) -> TreasuryTxModel -> Script
  script _ m = compile result
    where
      result :: Term s POpaque
      result =
        treasuryValidator cs
          # (pforgetData $ pdata d)
          # (pforgetData $ pdata r)
          # ctx

      cs :: CurrencySymbol
      cs = m.gatCs

      d :: Term s PTreasuryDatum
      d = pcon $ PTreasuryDatum fields
        where
          adaStateThread :: Term _ PCurrencySymbol
          adaStateThread = pconstant $ CurrencySymbol ""

          fields :: Term _ (PDataRecord '["stateThread" ':= PCurrencySymbol])
          fields = pdcons # (pdata adaStateThread) # pdnil

      r :: Term s PTreasuryRedeemer
      r = pcon $ PAlterTreasuryParams pdnil

      ctx :: Term s PScriptContext
      ctx = pconstant m.ctx

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
