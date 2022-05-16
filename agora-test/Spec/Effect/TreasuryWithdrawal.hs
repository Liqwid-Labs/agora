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
import Plutarch.Builtin
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutarch.Api.V1
import Sample.Effect.TreasuryWithdrawal
import Sample.Sample
import Test.Util (effectFailsWith, effectSucceedsWith)
import Agora.Utils
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck 
import Test.Tasty.Plutarch.Property (classifiedProperty)

import Data.Tagged
import Data.Universe

import Control.Applicative

type TWETestInput = (TreasuryWithdrawalDatum, TxInfo)
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
    InputsHaveOtherScriptInput -> "Transaction has script input that is not specified in datum"
    RemaindersDoNotReturnToTreasuries -> "Remainding Values do not return to input treasuries"
    EffectShouldPass -> "Effect should pass"

instance Universe TWETestCases where
  universe =
    [ PaysToEffect
    , OutputsDoNotMatchReceivers
    , InputsHaveOtherScriptInput
    , RemaindersDoNotReturnToTreasuries
    ]

instance Finite TWETestCases where
  universeF = universe
  cardinality = Tagged 4
  
-- TODO: Some unideal repeated patterns here
genTWECases :: TWETestCases -> Gen TWETestInput
genTWECases PaysToEffect                      = do
  datum <- genTWEDatum
  -- Would be nice to randomize number of outputs to effect
  vals <- listOf1 genAnyValue
  let toEffect =
        (\val -> TxOut
        { txOutAddress = Address (ScriptCredential $ validatorHash validator) Nothing
        , txOutValue = val
        , txOutDatumHash = Nothing
        }) <$> vals
      modify (i, o) = return (i, o <> toEffect)
  txinfo <- txInfoFromTWEDatum modify datum
  return (datum, txinfo)
genTWECases OutputsDoNotMatchReceivers        = do
  datum <- genTWEDatum
  let modify (i, o) = do
        -- unfortunately `sublistOf` sometimes returns same array.
        -- So tail is used to make sure there is aleast one thing missing
        output <- sublistOf $ tail o
        return (i, output)
  txinfo <- txInfoFromTWEDatum modify datum
  return (datum, txinfo)
genTWECases InputsHaveOtherScriptInput        = do
  datum <- genTWEDatum
  let modify (i, o) = do
        d <- listOf1 $ liftA2 (,) genScriptCredential genAnyValue 
        
        let unauthorizedScriptInputs =
              (\(addr, val) ->
                  TxInInfo
                  (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 1)
                  TxOut
                  { txOutAddress = Address addr Nothing
                  , txOutValue = val
                  , txOutDatumHash = Nothing
                  }) <$> d
        return (i <> unauthorizedScriptInputs, o)
  txinfo <- txInfoFromTWEDatum modify datum
  return (datum, txinfo)
genTWECases RemaindersDoNotReturnToTreasuries = do
  -- suchThat here might be unideal, but it's what we've got...
  -- Possible Fix, use random input amount for treasury inputs
  -- so that it always have excess
  datum <- genTWEDatum `suchThat` (\(TreasuryWithdrawalDatum r t) ->
                                     let totalExpected = mconcat $ snd <$> r
                                         ts = length t
                                         in
                                     totalExpected /= mconcat (replicate ts $ distributeValue ts totalExpected)
                                     )
  let modify (i, o) = do
        -- We'll drop all outputs directed to ScriptCredential
        let treasuryDroppedOutput =
              filter (\(addressCredential . txOutAddress -> x) -> case x of
                         PubKeyCredential _ -> True
                         ScriptCredential _ -> False
                        ) o
        return (i, treasuryDroppedOutput)
  txinfo <- txInfoFromTWEDatum modify datum
  return (datum, txinfo)
genTWECases EffectShouldPass                  = do
  datum <- genTWEDatum
  txinfo <- txInfoFromTWEDatum return datum
  return (datum, txinfo)

classifyTWE :: TWETestInput -> TWETestCases
classifyTWE ((TreasuryWithdrawalDatum r t), info)
  | paysToEffect = PaysToEffect
  | outputsDoNotMatchReceivers = OutputsDoNotMatchReceivers
  | inputsHaveOtherScriptInput = InputsHaveOtherScriptInput
  | remaindersDoNotReturnToTreasuries = RemaindersDoNotReturnToTreasuries
  | otherwise = EffectShouldPass
  where
    extractCredVal txout = (addressCredential (txOutAddress txout), txOutValue txout)
    credentialValuePairs = extractCredVal <$> txInfoOutputs info
    paysToEffect = elem (ScriptCredential $ validatorHash validator) $ fst . extractCredVal <$> (txInfoOutputs info)
    outputsDoNotMatchReceivers = not $ and $ fmap (\x -> elem x credentialValuePairs) r
    inputsHaveOtherScriptInput = or $ (\(fst . extractCredVal . txInInfoResolved -> x) ->
                                         (not $ elem x t) &&
                                         x /= (ScriptCredential $ validatorHash validator)
                                      ) <$> txInfoInputs info

    sumValueOfTreasury x = mconcat $ snd <$> (filter (\(c,_) -> elem c t) $ x)
    treasuryInputSum = sumValueOfTreasury $ extractCredVal . txInInfoResolved <$> txInfoInputs info
    treasuryOutputSum = sumValueOfTreasury credentialValuePairs
    expected = Value.unionWith (-) treasuryInputSum (mconcat $ snd <$> r)
    remaindersDoNotReturnToTreasuries = treasuryOutputSum /= expected
    
shrinkTWE :: TWETestInput -> [TWETestInput]
shrinkTWE = const []

expectedTWE :: Term s (PBuiltinPair PTreasuryWithdrawalDatum PTxInfo :--> PMaybe PUnit)
expectedTWE = plam $ \_input -> pcon $ PNothing

opaqueToUnit :: Term s (POpaque :--> PUnit)
opaqueToUnit = plam $ \_ -> pconstant ()

definitionTWE :: Term s (PBuiltinPair PTreasuryWithdrawalDatum PTxInfo :--> PUnit)
definitionTWE = plam $ \input -> unTermCont $ do
  datum <- tclet $ pfstBuiltin # input
  txinfo <- tclet $ psndBuiltin # input

  let scriptContext =
        pcon $ PScriptContext $
          pdcons @"txInfo" # pdata txinfo
          #$ pdcons @"purpose" # pdata (pconstant $ Spending (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 1))
          # pdnil

  pure $ opaqueToUnit #$ treasuryWithdrawalValidator currSymbol
    # pforgetData (pdata datum)
    # pforgetData (pdata (pconstant ()))
    # scriptContext

propertyTWE :: Property
propertyTWE = classifiedProperty genTWECases shrinkTWE expectedTWE classifyTWE definitionTWE

{- | Generates "lawful" ScriptContext from given TreasuryWithdrawalDatum.
Other cases can use this ScriptContext to derive from and develop
a case specific contexts with generators.

TODO: will this work okay with generators adding and removing
parts? I don't see particular reason it will not to, but will
that be a "good" generator?
-}
txInfoFromTWEDatum :: (([TxInInfo], [TxOut]) -> Gen ([TxInInfo], [TxOut])) -> TreasuryWithdrawalDatum -> Gen TxInfo
txInfoFromTWEDatum cb datum = do
  (input, output) <- cb (expectedInput, expectedOutput)
  return $
      TxInfo
      { txInfoInputs = input
      , txInfoOutputs = output
      , txInfoFee = Value.singleton "" "" 2
      , txInfoMint = Value.singleton currSymbol validatorHashTN (-1)
      , txInfoDCert = []
      , txInfoWdrl = []
      , txInfoValidRange = Interval.always
      , txInfoSignatories = [signer]
      , txInfoData = []
      , txInfoId = "0b123412341234"
      }
  where
    (expectedInput, excessOutputs)  = expectedTxInInfoFromTWEDatum datum
    expectedOutput = expectedTxOutFromTWEDatum datum <> excessOutputs

expectedTxOutFromTWEDatum :: TreasuryWithdrawalDatum -> [TxOut]
expectedTxOutFromTWEDatum (TreasuryWithdrawalDatum r _) =
  f <$> r -- add outputs to treasuries, returning excess STs.
  where 
    f (addr, val) = TxOut
      { txOutAddress = Address addr Nothing
      , txOutValue = val
      , txOutDatumHash = Nothing
      }

{- | Generates expected inputs from given Datum
-}
expectedTxInInfoFromTWEDatum :: TreasuryWithdrawalDatum -> ([TxInInfo], [TxOut])
expectedTxInInfoFromTWEDatum (TreasuryWithdrawalDatum r t) =
  (inputGAT:((\addr -> TxInInfo
    (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 1)
    TxOut
    { txOutAddress = Address addr Nothing
    , txOutValue = treasuryInputValue
    , txOutDatumHash = Nothing
    })
  <$> t)
  , [TxOut
    { txOutAddress = Address (head t) Nothing
    , txOutValue = extras
    , txOutDatumHash = Nothing
    }])
  where
    totalValues = mconcat $ snd <$> r
    treasuryInputValue = distributeValue (length t) totalValues
    extras = Value.unionWith (-) (mconcat (replicate (length t) treasuryInputValue)) $ totalValues

distributeValue :: Int -> Value -> Value
distributeValue n v = mconcat $ (\(cs, tn, (toInteger -> val)) -> Value.singleton cs tn val) <$> vals
  where
     vals = (\(cs, tn, (fromInteger -> val)) -> (cs, tn, val `divRound` n)) <$> Value.flattenValue v
     divRound x y = case divMod x y of
                      (x, 0) -> x
                      (x, _) -> x + 1

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

prop :: PropertyM IO Bool
prop = forAllM (elements (universe :: [TWETestCases])) (\c -> run $ generate (genTWECases c) >>= (\gen -> return $ c == classifyTWE gen))

tests :: [TestTree]
tests =
  [ testGroup
      "Property"
      [ testProperty "Generator <-> Classifier" (monadicIO prop)
      , testProperty "effect" propertyTWE
      ]
  , testGroup
      "Unit"
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
