{- |
Module     : Spec.Effect.TreasuryWithdrawalEffect
Maintainer : seungheon.ooh@gmail.com
Description: Sample based testing for Treasury Withdrawal Effect

This module tests the Treasury Withdrawal Effect.
-}
module Spec.Effect.TreasuryWithdrawal (tests) where

import Agora.Effect.TreasuryWithdrawal (
  TreasuryWithdrawalDatum (TreasuryWithdrawalDatum),
  treasuryWithdrawalValidator,
  PTreasuryWithdrawalDatum
 )
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutarch.Api.V1
import Spec.Sample.Effect.TreasuryWithdrawal
import Spec.Sample.Sample
import Spec.Util (effectFailsWith, effectSucceedsWith)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.Plutarch.Property (classifiedProperty)

import Data.Tagged
import Data.Universe

type TWETestInput = (TreasuryWithdrawalDatum, ScriptContext)
data TWETestCases
  = PaysToEffect
  | OutputsDoNotMatchReceivers
  | InputsHaveOtherScriptInput
  | RemaindersDoNotReturnToTreasuries
  | EffectShouldPass
  deriving stock (Eq)

instance Show TWETestCases where
  show = \case
    PaysToEffect -> "Transaction pays to effect"
    OutputsDoNotMatchReceivers -> "Transaction outputs do not match receivers"
    InputsHaveOtherScriptInput -> "Remainding Values do not return to input treasuries"
    RemaindersDoNotReturnToTreasuries -> "Transaction has script input that is not specified in datum"
    EffectShouldPass -> "Effect should pass"

instance Universe TWETestCases where
  universe =
    [ PaysToEffect
    , OutputsDoNotMatchReceivers
    , InputsHaveOtherScriptInput
    , RemaindersDoNotReturnToTreasuries
    , EffectShouldPass
    ]

instance Finite TWETestCases where
  universeF = universe
  cardinality = Tagged 5

genTWECases :: TWETestCases -> Gen TWETestInput
genTWECases PaysToEffect                      = undefined
genTWECases OutputsDoNotMatchReceivers        = undefined
genTWECases InputsHaveOtherScriptInput        = undefined
genTWECases RemaindersDoNotReturnToTreasuries = undefined
genTWECases EffectShouldPass                  = undefined

classifyTWE :: TWETestInput -> TWETestCases
classifyTWE = undefined

shrinkTWE :: TWETestInput -> [TWETestInput]
shrinkTWE = undefined

expectedTWE :: Term s (PBuiltinPair PTreasuryWithdrawalDatum PScriptContext :--> PMaybe PBool)
expectedTWE = undefined

definitionTWE :: Term s (PBuiltinPair PTreasuryWithdrawalDatum PScriptContext :--> PBool)
definitionTWE = undefined

propertyTWE :: Property
propertyTWE = classifiedProperty genTWECases shrinkTWE expectedTWE classifyTWE definitionTWE

{- | Generates "lawful" ScriptContext from given TreasuryWithdrawalDatum.
Other cases can use this ScriptContext to derive from and develop
a case specific contexts with generators.

TODO: will this work okay with generators adding and removing
parts? I don't see particular reason it will not to, but will
that be a "good" generator?
-}
_scriptContextFromTWEDatum :: TreasuryWithdrawalDatum -> ScriptContext
_scriptContextFromTWEDatum datum@(TreasuryWithdrawalDatum r t) =
  ScriptContext txinfo sp
  where
    txinfo =
      TxInfo
      { txInfoInputs = inputs
      , txInfoOutputs = outputs
      , txInfoFee = Value.singleton "" "" 2
      , txInfoMint = Value.singleton currSymbol validatorHashTN (-1)
      , txInfoDCert = []
      , txInfoWdrl = []
      , txInfoValidRange = Interval.always
      , txInfoSignatories = [signer]
      , txInfoData = []
      , txInfoId = "TODO: what should I put here?"
      }
    outputs = _expectedTxOutFromTWEDatum datum
    inputs = _expectedTxInInfoFromTWEDatum datum
    sp = Spending (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 1)

_expectedTxOutFromTWEDatum :: TreasuryWithdrawalDatum -> [TxOut]
_expectedTxOutFromTWEDatum (TreasuryWithdrawalDatum r _) =
  f <$> r -- add outputs to treasuries, returning excess STs.
  where 
    f (addr, val) = TxOut
      { txOutAddress = Address addr Nothing
      , txOutValue = val
      , txOutDatumHash = Nothing
      }

_expectedTxInInfoFromTWEDatum :: TreasuryWithdrawalDatum -> [TxInInfo]
_expectedTxInInfoFromTWEDatum (TreasuryWithdrawalDatum r t) =
  undefined -- TODO: What should I do here?
  where
    totalValues = mconcat $ snd <$> r

genTWEDatum :: Gen TreasuryWithdrawalDatum
genTWEDatum = do
  -- Make several random assetclasses to choose from
  ac <- listOf1 genAssetClass

  -- Make several random users
  users <- listOf1 genUserCredential

  -- Make several random treasuries
  treas <- listOf1 genScriptCredential

  -- Make random amounts of values that transaction will have
  values <- listOf1 $ elements ac >>= genValue

  let receiverList = zipWith (,) users values
  pure $ TreasuryWithdrawalDatum receiverList treas

genTWETestInput :: Gen TWETestInput
genTWETestInput = do
  a <- genTWEDatum
  b <- pure $ buildScriptContext [] []
  pure $ (a, b)

_asdf :: IO ()
_asdf = undefined
  where
    _a = genTWETestInput
    _b = genTWEDatum

tests :: [TestTree]
tests =
  [ testProperty "effect" propertyTWE
  , testGroup
      "effect"
      [ effectSucceedsWith
          "Simple"
          (treasuryWithdrawalValidator currSymbol)
          datum1
          ( buildScriptContext
              [ inputGAT
              , inputCollateral 10
              , inputTreasury 1 (asset1 10)
              ]
              $ outputTreasury 1 (asset1 7) :
              buildReceiversOutputFromDatum datum1
          )
      , effectSucceedsWith
          "Simple with multiple treasuries "
          (treasuryWithdrawalValidator currSymbol)
          datum1
          ( buildScriptContext
              [ inputGAT
              , inputCollateral 10
              , inputTreasury 1 (asset1 10)
              , inputTreasury 2 (asset1 100)
              , inputTreasury 3 (asset1 500)
              ]
              $ [ outputTreasury 1 (asset1 7)
                , outputTreasury 2 (asset1 100)
                , outputTreasury 3 (asset1 500)
                ]
                ++ buildReceiversOutputFromDatum datum1
          )
      , effectSucceedsWith
          "Mixed Assets"
          (treasuryWithdrawalValidator currSymbol)
          datum2
          ( buildScriptContext
              [ inputGAT
              , inputCollateral 10
              , inputTreasury 1 (asset1 20)
              , inputTreasury 2 (asset2 20)
              ]
              $ [ outputTreasury 1 (asset1 13)
                , outputTreasury 2 (asset2 14)
                ]
                ++ buildReceiversOutputFromDatum datum2
          )
      , effectFailsWith
          "Pay to uknown 3rd party"
          (treasuryWithdrawalValidator currSymbol)
          datum2
          ( buildScriptContext
              [ inputGAT
              , inputCollateral 10
              , inputTreasury 1 (asset1 20)
              , inputTreasury 2 (asset2 20)
              ]
              $ [ outputUser 100 (asset1 2)
                , outputTreasury 1 (asset1 11)
                , outputTreasury 2 (asset2 14)
                ]
                ++ buildReceiversOutputFromDatum datum2
          )
      , effectFailsWith
          "Missing receiver"
          (treasuryWithdrawalValidator currSymbol)
          datum2
          ( buildScriptContext
              [ inputGAT
              , inputCollateral 10
              , inputTreasury 1 (asset1 20)
              , inputTreasury 2 (asset2 20)
              ]
              $ [ outputTreasury 1 (asset1 13)
                , outputTreasury 2 (asset2 14)
                ]
                ++ drop 1 (buildReceiversOutputFromDatum datum2)
          )
      , effectFailsWith
          "Unauthorized treasury"
          (treasuryWithdrawalValidator currSymbol)
          datum3
          ( buildScriptContext
              [ inputGAT
              , inputCollateral 10
              , inputTreasury 999 (asset1 20)
              ]
              $ outputTreasury 999 (asset1 17) :
              buildReceiversOutputFromDatum datum3
          )
      , effectFailsWith
          "Prevent transactions besides the withdrawal"
          (treasuryWithdrawalValidator currSymbol)
          datum3
          ( buildScriptContext
              [ inputGAT
              , inputTreasury 1 (asset1 20)
              , inputTreasury 999 (asset1 20)
              , inputUser 99 (asset2 100)
              ]
              $ [ outputTreasury 1 (asset1 17)
                , outputUser 100 (asset2 100)
                ]
                ++ buildReceiversOutputFromDatum datum3
          )
      ]
  ]
  where
    asset1 = Value.singleton "abbc12" "OrangeBottle"
    asset2 = Value.singleton "abbc12" "19721121"
    datum1 =
      TreasuryWithdrawalDatum
        [ (head users, asset1 1)
        , (users !! 1, asset1 1)
        , (users !! 2, asset1 1)
        ]
        [ treasuries !! 1
        , treasuries !! 2
        , treasuries !! 3
        ]
    datum2 =
      TreasuryWithdrawalDatum
        [ (head users, asset1 4 <> asset2 5)
        , (users !! 1, asset1 2 <> asset2 1)
        , (users !! 2, asset1 1)
        ]
        [ head treasuries
        , treasuries !! 1
        , treasuries !! 2
        ]
    datum3 =
      TreasuryWithdrawalDatum
        [ (head users, asset1 1)
        , (users !! 1, asset1 1)
        , (users !! 2, asset1 1)
        ]
        [treasuries !! 1]
