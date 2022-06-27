{- |
Module     : Property.MultiSig
Maintainer : seungheon.ooh@gmail.com
Description: Property tests various utility functions.

Properties for various utility function.
-}
module Property.Utility (props) where

import Agora.Utils (
  findOutputsToAddress,
  findTxOutDatum,
  isPubKey,
  isScriptAddress,
  mustBePDJust,
  mustBePJust,
 )
import Data.Maybe (isNothing)
import Data.Tagged (Tagged (Tagged))
import Data.Universe (Finite (..), Universe (..))
import Plutarch.Api.V1 (
  PAddress,
  PDatum,
  PDatumHash,
  PMaybeData (..),
  PTxOut,
  datumHash,
  ptuple,
 )
import PlutusLedgerApi.V1 (
  Address (Address),
  Credential (..),
  Datum (Datum),
  DatumHash,
  ToData (toBuiltinData),
  TxOut (TxOut, txOutAddress, txOutDatumHash),
 )
import Property.Generator (
  genAddress,
  genScriptCredential,
  genSingletonValue,
  genUserCredential,
 )
import Test.Tasty (TestTree)
import Test.Tasty.Plutarch.Property (
  classifiedPropertyNative,
  peqPropertyNative',
 )
import Test.Tasty.QuickCheck (
  Arbitrary (arbitrary),
  Gen,
  Property,
  elements,
  listOf,
  listOf1,
  testProperty,
 )

data Case2
  = Case2A
  | Case2B
  deriving stock (Show, Eq)

instance Universe Case2 where
  universe =
    [ Case2A
    , Case2B
    ]

instance Finite Case2 where
  universeF = universe
  cardinality = Tagged 2

data Case3
  = Case3A
  | Case3B
  | Case3C
  deriving stock (Show, Eq)

instance Universe Case3 where
  universe =
    [ Case3A
    , Case3B
    , Case3C
    ]

instance Finite Case3 where
  universeF = universe
  cardinality = Tagged 3

isScriptAddressProp :: Property
isScriptAddressProp =
  classifiedPropertyNative gen (const []) expected classifier isScriptAddress
  where
    classifier :: Address -> Case2
    classifier (Address (PubKeyCredential _) _) = Case2A
    classifier (Address (ScriptCredential _) _) = Case2B

    expected :: Address -> Maybe Bool
    expected c = Just $ classifier c == Case2B

    gen :: Case2 -> Gen Address
    gen Case2A = flip Address Nothing <$> genUserCredential
    gen Case2B = flip Address Nothing <$> genScriptCredential

isPubKeyProp :: Property
isPubKeyProp =
  classifiedPropertyNative gen (const []) expected classifier isPubKey
  where
    classifier :: Credential -> Case2
    classifier (PubKeyCredential _) = Case2A
    classifier (ScriptCredential _) = Case2B

    expected :: Credential -> Maybe Bool
    expected c = Just $ classifier c == Case2A

    gen :: Case2 -> Gen Credential
    gen Case2A = genUserCredential
    gen Case2B = genScriptCredential

-- Assumes returning TxOuts are in input order.
findOutputsToAddressProp :: Property
findOutputsToAddressProp =
  peqPropertyNative' expected gen (const []) actual
  where
    expected :: ([TxOut], Address) -> [TxOut]
    expected (outs, addr) = filter ((addr ==) . txOutAddress) outs

    gen :: Gen ([TxOut], Address)
    gen = do
      outs <- listOf $ do
        addr <- genAddress
        val <- genSingletonValue
        return $ TxOut addr val Nothing

      targetAddr <-
        case outs of
          [] -> genAddress
          _ -> return . txOutAddress . head $ outs

      return (outs, targetAddr)

    -- There's gotta be a better way than this abomination
    actual ::
      Term s (PBuiltinPair (PBuiltinList PTxOut) PAddress :--> PBuiltinList PTxOut)
    actual = phoistAcyclic $
      plam $ \x ->
        pmap # plam pfromData
          #$ findOutputsToAddress # (pmap # plam pdata #$ pfstBuiltin # x) # (psndBuiltin # x)

findTxOutDatumProp :: Property
findTxOutDatumProp =
  classifiedPropertyNative gen (const []) expected classifier actual
  where
    -- Case2A - Nothing
    -- Case2B - Not nothing
    -- Case2C - TxOut given with no datum hash
    expected :: ([(DatumHash, Datum)], TxOut) -> Maybe (Maybe Datum)
    expected (pair, out)
      | isNothing (txOutDatumHash out) = Just Nothing
      | null p = Just Nothing
      | otherwise = Just . Just . snd . head $ p
      where
        p = filter ((== txOutDatumHash out) . Just . fst) pair

    gen :: Case3 -> Gen ([(DatumHash, Datum)], TxOut)
    gen Case3A = do
      datumPairs <- listOf $ do
        dat <- Datum . toBuiltinData <$> (arbitrary :: Gen Integer)
        return (datumHash dat, dat)
      ref <- datumHash . Datum . toBuiltinData <$> (arbitrary :: Gen Integer)
      cred <- genAddress
      let out = TxOut cred mempty $ Just ref
          pairs = filter ((/= ref) . fst) datumPairs
      return (pairs, out)
    gen Case3B = do
      datumPairs <- listOf1 $ do
        dat <- Datum . toBuiltinData <$> (arbitrary :: Gen Integer)
        return (datumHash dat, dat)
      ref <- elements $ fst <$> datumPairs
      cred <- genAddress
      let out = TxOut cred mempty $ Just ref
      return (datumPairs, out)
    gen Case3C = do
      datumPairs <- listOf $ do
        dat <- Datum . toBuiltinData <$> (arbitrary :: Gen Integer)
        return (datumHash dat, dat)

      cred <- genAddress
      let out = TxOut cred mempty Nothing
      return (datumPairs, out)

    classifier :: ([(DatumHash, Datum)], TxOut) -> Case3
    classifier (p, out)
      | isNothing (txOutDatumHash out) = Case3C
      | any ((== txOutDatumHash out) . Just) (fst <$> p) = Case3B
      | otherwise = Case3A

    actual ::
      Term s (PBuiltinPair (PBuiltinList (PBuiltinPair PDatumHash PDatum)) PTxOut :--> PMaybeData PDatum)
    actual = phoistAcyclic $
      plam $ \x ->
        let pairs' = pfstBuiltin # x
            out = psndBuiltin # x
            pairs =
              pmap # plam (\y -> pdata $ ptuple # pdata (pfstBuiltin # y) # pdata (psndBuiltin # y)) # pairs'
         in pmaybeToPMaybeData $ findTxOutDatum # pairs # out

mustBePJustProp :: Property
mustBePJustProp =
  classifiedPropertyNative gen (const []) expected classifier actual
  where
    classifier :: Maybe a -> Case2
    classifier (Just _) = Case2A
    classifier Nothing = Case2B

    gen :: Case2 -> Gen (Maybe Integer)
    gen Case2A = Just <$> arbitrary
    gen Case2B = return Nothing

    expected :: Maybe a -> Maybe a
    expected = id

    actual ::
      Term s (PMaybeData PInteger :--> PInteger)
    actual = phoistAcyclic $ plam $ \x -> mustBePJust # "is PNothing" # pmaybeDataToPMaybe x

mustBePDJustProp :: Property
mustBePDJustProp =
  classifiedPropertyNative gen (const []) expected classifier actual
  where
    classifier :: Maybe a -> Case2
    classifier (Just _) = Case2A
    classifier Nothing = Case2B

    gen :: Case2 -> Gen (Maybe Integer)
    gen Case2A = Just <$> arbitrary
    gen Case2B = return Nothing

    expected :: Maybe a -> Maybe a
    expected = id

    actual ::
      Term s (PMaybeData PInteger :--> PInteger)
    actual = phoistAcyclic $ plam $ \x -> mustBePDJust # "is PNothing" # x

pmaybeToPMaybeData :: PIsData a => Term s (PMaybe a) -> Term s (PMaybeData a)
pmaybeToPMaybeData = flip pmatch $ \case
  PJust a -> pcon . PDJust $ pdcons @"_0" # pdata a # pdnil
  PNothing -> pcon . PDNothing $ pdnil

pmaybeDataToPMaybe :: PIsData a => Term s (PMaybeData a) -> Term s (PMaybe a)
pmaybeDataToPMaybe = flip pmatch $ \case
  PDJust ((pfield @"_0" #) -> a) -> pcon $ PJust a
  PDNothing _ -> pcon PNothing

props :: [TestTree]
props =
  [ testProperty "isScriptAddress" isScriptAddressProp
  , testProperty "isPubKey" isPubKeyProp
  , testProperty "findOutputsToAddress" findOutputsToAddressProp
  , testProperty "findTxOutDatum" findTxOutDatumProp
  , testProperty "mustBePJustProp" mustBePJustProp
  , testProperty "mustBePDJustProp" mustBePDJustProp
  ]
