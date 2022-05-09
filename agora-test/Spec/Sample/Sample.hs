{- |
Module     : Spec.Sample.Sample
Maintainer : seungheon.ooh@gmail.com
Description: Useful components for constructing property tests

Basic components for constructing case-specific property tests.
-}
module Spec.Sample.Sample (
  -- * Credentials
  -- $credentials
  genUserCredential,
  genScriptCredential,
  genCredential,
  genAddress,

  -- * Values
  -- $values
  genValue,
  genAssetClass,
  genAnyValue,

  -- * Tx info
  -- $txinfo
  genTxOut,
  genTxInInfo,
) where

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value
import Test.QuickCheck

import Control.Applicative

import Data.ByteString.Char8 qualified as C
import Data.ByteString.Hash (sha2)

{- | Generate a random Hash
Hashs cannot be shrunken; functions utilizing this function,
therefore, cannot be shrunken as well.
-}
genHashByteString :: Gen C.ByteString
genHashByteString = sha2 . C.pack . show <$> (chooseAny :: Gen Integer)

-- TODO: How do I need to ensure uniqueness?

-- | Random user credential.
genUserCredential :: Gen Credential
genUserCredential = PubKeyCredential . PubKeyHash . toBuiltin <$> genHashByteString

-- | Random script credential.
genScriptCredential :: Gen Credential
genScriptCredential = ScriptCredential . ValidatorHash . toBuiltin <$> genHashByteString

-- | Random credential: combination of user and script credential generators.
genCredential :: Gen Credential
genCredential = oneof [genUserCredential, genScriptCredential]

genAddress :: Gen Address
genAddress = flip Address Nothing <$> genCredential

{- | Random Value of given AssetClass
`genAnyValue` will create a random value with a random assetclass.
-}
genValue :: AssetClass -> Gen Value
genValue ac = assetClassValue ac . abs <$> (arbitrary :: Gen Integer)

genPrettyByteString :: Gen C.ByteString
genPrettyByteString = C.pack <$> (listOf1 $ elements ['a' .. 'z'])

genAssetClass :: Gen AssetClass
genAssetClass =
  AssetClass
    <$> liftA2
      (,)
      (currencySymbol <$> genHashByteString)
      (tokenName <$> genPrettyByteString)

genAnyValue :: Gen Value
genAnyValue = genAssetClass >>= genValue

genTxOut :: Gen TxOut
genTxOut = do
  addr <- genAddress
  val <- listOf genAnyValue
  pure
    TxOut
      { txOutAddress = addr
      , txOutValue = foldr (<>) mempty val
      , txOutDatumHash = Just (DatumHash "")
      }

genTxInInfo :: Gen TxInInfo
genTxInInfo = TxInInfo (TxOutRef "" 1) <$> genTxOut

{- | $credentials
Generators for Plutus Credentials. These will generate
randomized credential.
-}

{- | $values
Generators for Plutus Values and AssetClass. These will generate
randomized Values and AssetClass.
-}
