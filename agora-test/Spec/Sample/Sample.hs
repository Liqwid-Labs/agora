module Spec.Sample.Sample
  ( -- * Credentials
    -- $credentials
    genUserCredential
  , genScriptCredential
  , genCredential
    -- * Values
    -- $values
  , genValue
  , genAssetClass
  , genAnyValue
  ) where

import Test.QuickCheck 
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value

import Control.Applicative

import Data.ByteString.Char8 qualified as C
import Data.ByteString.Hash (sha2)

genHashByteString :: Gen C.ByteString
genHashByteString = sha2 . C.pack . show <$> (chooseAny :: Gen Integer)

-- TODO: How do I need to ensure uniqueness?
genUserCredential :: Gen Credential
genUserCredential = PubKeyCredential . PubKeyHash . toBuiltin <$> genHashByteString

genScriptCredential :: Gen Credential
genScriptCredential = ScriptCredential . ValidatorHash . toBuiltin <$> genHashByteString

genCredential :: Gen Credential
genCredential = oneof [genUserCredential, genScriptCredential]

genValue :: AssetClass -> Gen Value
genValue ac = assetClassValue ac . abs <$> (chooseAny :: Gen Integer)

genAssetClass :: Gen AssetClass
genAssetClass = liftA2 assetClass (currencySymbol <$> genHashByteString) (tokenName <$> genHashByteString)

genAnyValue :: Gen Value
genAnyValue = genAssetClass >>= genValue

txInInfoGen :: Gen TxInInfo
txInInfoGen = do
  undefined

{- | $credentials
Generators for Plutus Credentials. These will generate
randomized credential.
-}

{- | $values
Generators for Plutus Values and AssetClass. These will generate
randomized Values and AssetClass.
-}
