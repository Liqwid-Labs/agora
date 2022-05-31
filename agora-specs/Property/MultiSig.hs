{- |
Module     : Property.MultiSig
Maintainer : seungheon.ooh@gmail.com
Description: Property tests for 'MultiSig' functions

Property model and tests for 'MultiSig' functions
-}
module Property.MultiSig (props) where

import Agora.MultiSig (
  MultiSig (MultiSig),
  PMultiSig,
  pvalidatedByMultisig,
 )
import Agora.Utils (tclet)
import Data.Maybe (fromJust)
import Data.Tagged (Tagged (Tagged))
import Data.Universe (Finite (..), Universe (..))
import Plutarch.Api.V1 (PScriptContext)
import Plutarch.Context.Config (defaultConfig)
import Plutarch.Context.Spending (
  ValidatorUTXO (ValidatorUTXO),
  inputSelfExtra,
  signedWith,
  spendingContext,
 )
import Plutus.V1.Ledger.Api (
  ScriptContext (scriptContextTxInfo),
  TxInfo (txInfoSignatories),
 )
import Spec.Generator (genAnyValue, genPubKeyHash)
import Test.Tasty (TestTree)
import Test.Tasty.Plutarch.Property (classifiedProperty)
import Test.Tasty.QuickCheck (
  Gen,
  Property,
  chooseInt,
  listOf,
  testProperty,
  vectorOf,
 )

-- | apropos model for testing multisigs.
type MultiSigModel = (MultiSig, ScriptContext)

-- | Propositions that may hold true of a `MultiSigModel`.
data MultiSigProp
  = -- | Sufficient number of signatories in the script context.
    MeetsMinSigs
  | -- | Insufficient number of signatories in the script context.
    DoesNotMeetMinSigs
  deriving stock (Eq, Show, Ord)

instance Universe MultiSigProp where
  universe = [MeetsMinSigs, DoesNotMeetMinSigs]

instance Finite MultiSigProp where
  universeF = universe
  cardinality = Tagged 2

-- | Generate model with given proposition.
genMultiSigProp :: MultiSigProp -> Gen MultiSigModel
genMultiSigProp prop = do
  size <- chooseInt (4, 20)
  pkhs <- vectorOf size genPubKeyHash
  vutxo <- pure . ValidatorUTXO () <$> genAnyValue
  minSig <- chooseInt (1, length pkhs)
  othersigners <- take 20 <$> listOf genPubKeyHash

  let ms = MultiSig pkhs (toInteger minSig)

  n <- case prop of
    MeetsMinSigs -> chooseInt (minSig, length pkhs)
    DoesNotMeetMinSigs -> chooseInt (0, minSig - 1)

  let builder = foldr (<>) (inputSelfExtra mempty ()) (signedWith <$> take n pkhs <> othersigners)
      ctx = fromJust $ spendingContext defaultConfig builder vutxo
  pure (ms, ctx)

-- | Classify model into propositions.
classifyMultiSigProp :: MultiSigModel -> MultiSigProp
classifyMultiSigProp (MultiSig keys (fromIntegral -> minsig), ctx)
  | minsig <= length signer = MeetsMinSigs
  | otherwise = DoesNotMeetMinSigs
  where
    signer = filter (`elem` keys) $ txInfoSignatories . scriptContextTxInfo $ ctx

-- | Shrinker. Not used.
shrinkMultiSigProp :: MultiSigModel -> [MultiSigModel]
shrinkMultiSigProp = const []

expected :: Term s (PBuiltinPair PMultiSig PScriptContext :--> PMaybe PBool)
expected = plam $ \x -> unTermCont $ do
  ms <- tclet $ pfstBuiltin # x
  sc <- tclet $ psndBuiltin # x
  multsig <- tcont $ pletFields @'["keys", "minSigs"] ms
  let signers = pfromData $ pfield @"signatories" #$ pfromData (pfield @"txInfo" # sc)
      validSigners = plength #$ pfilter # plam (\x -> pelem # x # multsig.keys) # signers
  pure $ pcon $ PJust $ pfromData multsig.minSigs #<= validSigners

actual :: Term s (PBuiltinPair PMultiSig PScriptContext :--> PBool)
actual = plam $ \x -> unTermCont $ do
  ms <- tclet $ pfstBuiltin # x
  sc <- tclet $ psndBuiltin # x
  pure $ pvalidatedByMultisig # ms # (pfield @"txInfo" # sc)

prop :: Property
prop = classifiedProperty genMultiSigProp shrinkMultiSigProp expected classifyMultiSigProp actual

props :: [TestTree]
props =
  [ testProperty "MultiSig property" prop
  ]
