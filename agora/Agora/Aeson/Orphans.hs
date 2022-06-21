{-# OPTIONS_GHC -Wno-orphans #-}

module Agora.Aeson.Orphans (AsBase16Bytes (..)) where

--------------------------------------------------------------------------------

import Data.Coerce (Coercible, coerce)
import Prelude

--------------------------------------------------------------------------------

import Codec.Serialise qualified as Codec
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Lazy qualified as Lazy
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

--------------------------------------------------------------------------------

import PlutusLedgerApi.V1 qualified as Plutus
import PlutusLedgerApi.V1.Bytes qualified as Plutus
import PlutusLedgerApi.V1.Value qualified as Plutus

--------------------------------------------------------------------------------

newtype AsBase16Bytes a = AsBase16Bytes {unAsBase16Bytes :: a}
newtype AsBase16Codec a = AsBase16Codec {unAsBase16Codec :: a}

deriving via
  (Plutus.CurrencySymbol, Plutus.TokenName)
  instance
    Aeson.ToJSON Plutus.AssetClass

deriving via
  (Plutus.CurrencySymbol, Plutus.TokenName)
  instance
    Aeson.FromJSON Plutus.AssetClass

deriving via
  AsBase16Bytes Plutus.TxId
  instance
    Aeson.FromJSON Plutus.TxId

deriving via
  AsBase16Bytes Plutus.TxId
  instance
    Aeson.ToJSON Plutus.TxId

deriving anyclass instance Aeson.FromJSON Plutus.TxOutRef
deriving anyclass instance Aeson.ToJSON Plutus.TxOutRef

instance (Coercible a Plutus.LedgerBytes) => Aeson.ToJSON (AsBase16Bytes a) where
  toJSON =
    Aeson.String
      . Plutus.encodeByteString
      . Plutus.bytes
      . coerce @(AsBase16Bytes a) @Plutus.LedgerBytes

instance (Coercible Plutus.LedgerBytes a) => Aeson.FromJSON (AsBase16Bytes a) where
  parseJSON v =
    Aeson.parseJSON @T.Text v
      >>= either (Aeson.parserThrowError [] . show) (pure . coerce @_ @(AsBase16Bytes a))
        . Plutus.fromHex
        . T.encodeUtf8

instance (Codec.Serialise a) => Aeson.ToJSON (AsBase16Codec a) where
  toJSON =
    Aeson.String
      . Plutus.encodeByteString
      . Lazy.toStrict
      . Codec.serialise @a
      . (.unAsBase16Codec)

instance (Codec.Serialise a) => Aeson.FromJSON (AsBase16Codec a) where
  parseJSON v =
    Aeson.parseJSON @T.Text v
      >>= either (Aeson.parserThrowError [] . show) (pure . AsBase16Codec)
        . Codec.deserialiseOrFail
        . Lazy.fromStrict
        . T.encodeUtf8

--------------------------------------------------------------------------------

deriving via
  (AsBase16Bytes Plutus.CurrencySymbol)
  instance
    (Aeson.ToJSON Plutus.CurrencySymbol)
deriving via
  (AsBase16Bytes Plutus.CurrencySymbol)
  instance
    (Aeson.FromJSON Plutus.CurrencySymbol)

deriving via
  (AsBase16Bytes Plutus.TokenName)
  instance
    (Aeson.ToJSON Plutus.TokenName)
deriving via
  (AsBase16Bytes Plutus.TokenName)
  instance
    (Aeson.FromJSON Plutus.TokenName)

deriving via
  (AsBase16Bytes Plutus.ValidatorHash)
  instance
    (Aeson.ToJSON Plutus.ValidatorHash)
deriving via
  (AsBase16Bytes Plutus.ValidatorHash)
  instance
    (Aeson.FromJSON Plutus.ValidatorHash)

deriving via
  (AsBase16Bytes Plutus.BuiltinByteString)
  instance
    (Aeson.ToJSON Plutus.BuiltinByteString)
deriving via
  (AsBase16Bytes Plutus.BuiltinByteString)
  instance
    (Aeson.FromJSON Plutus.BuiltinByteString)

deriving via
  (AsBase16Codec Plutus.Validator)
  instance
    (Aeson.ToJSON Plutus.Validator)
deriving via
  (AsBase16Codec Plutus.Validator)
  instance
    (Aeson.FromJSON Plutus.Validator)

deriving via
  (AsBase16Codec Plutus.MintingPolicy)
  instance
    (Aeson.ToJSON Plutus.MintingPolicy)
deriving via
  (AsBase16Codec Plutus.MintingPolicy)
  instance
    (Aeson.FromJSON Plutus.MintingPolicy)

deriving via
  (AsBase16Codec Plutus.Script)
  instance
    (Aeson.ToJSON Plutus.Script)
deriving via
  (AsBase16Codec Plutus.Script)
  instance
    (Aeson.FromJSON Plutus.Script)

deriving via
  Integer
  instance
    (Aeson.ToJSON Plutus.POSIXTime)
deriving via
  Integer
  instance
    (Aeson.FromJSON Plutus.POSIXTime)
