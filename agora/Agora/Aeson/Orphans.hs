{-# OPTIONS_GHC -Wno-orphans #-}

module Agora.Aeson.Orphans (AsBase16Bytes (..)) where

--------------------------------------------------------------------------------

import Data.Coerce (Coercible, coerce)
import Plutarch.Orphans ()

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
