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
import Data.Tagged (Tagged (Tagged))
import Data.Universe (Finite (..), Universe (..))
import Plutarch.Api.V1 (PScriptContext)
import Plutarch.Context
import Plutarch.Extra.TermCont (pletC)
import PlutusLedgerApi.V1 (
  ScriptContext (..),
  ScriptPurpose (..),
  TxInfo (txInfoSignatories),
  TxOutRef (..),
 )
import Property.Generator (genPubKeyHash)
import Test.Tasty (TestTree)
import Test.Tasty.Plutarch.Property (classifiedPropertyNative)
import Test.Tasty.QuickCheck (
  Gen,
  Property,
  chooseInt,
  listOf,
  testProperty,
  vectorOf,
 )

-- | Model for testing multisigs.
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
  minSig <- chooseInt (1, length pkhs)
  othersigners <- take 20 <$> listOf genPubKeyHash

  let ms = MultiSig pkhs (toInteger minSig)

  n <- case prop of
    MeetsMinSigs -> chooseInt (minSig, length pkhs)
    DoesNotMeetMinSigs -> chooseInt (0, minSig - 1)

  let builder = mconcat $ signedWith <$> take n pkhs <> othersigners
      txinfo = either error id $ buildTxInfo builder
  pure (ms, ScriptContext txinfo (Spending (TxOutRef "" 0)))

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

-- | Expected behavior of @pvalidatedByMultisig@.
expectedHs :: MultiSigModel -> Maybe Bool
expectedHs model = case classifyMultiSigProp model of
  MeetsMinSigs -> Just True
  _ -> Just False

-- | Actual implementation of @pvalidatedByMultisig@.
actual :: Term s (PBuiltinPair PMultiSig PScriptContext :--> PBool)
actual = plam $ \x -> unTermCont $ do
  ms <- pletC $ pfstBuiltin # x
  sc <- pletC $ psndBuiltin # x
  pure $ pvalidatedByMultisig # ms # (pfield @"txInfo" # sc)

-- | Proposed property.
prop :: Property
prop = classifiedPropertyNative genMultiSigProp shrinkMultiSigProp expectedHs classifyMultiSigProp actual

props :: [TestTree]
props =
  [ testProperty "MultiSig property" prop
  ]
