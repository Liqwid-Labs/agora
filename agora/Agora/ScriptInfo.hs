{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Agora.ScriptInfo
Maintainer : emi@haskell.fyi
Description: Exportable script bundles for off-chain consumption.

Exportable script bundles for off-chain consumption.
-}
module Agora.ScriptInfo (
  -- * Types
  ScriptInfo (..),

  -- * Introduction functions
  mkValidatorInfo,
  mkPolicyInfo,
) where

import Agora.Aeson.Orphans ()
import Cardano.Binary qualified as CBOR
import Codec.Serialise qualified as Codec
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Text (Text)
import GHC.Generics qualified as GHC
import Plutarch.Api.V1 (PMintingPolicy, PValidator, mkMintingPolicy, mkValidator, scriptHash)
import PlutusLedgerApi.V1 (
  MintingPolicy (getMintingPolicy),
  Script,
  Validator (getValidator),
 )
import PlutusLedgerApi.V1.Scripts (ScriptHash)

{- | Bundle containing a 'Validator' and its hash.

     @since 0.2.0
-}
data ScriptInfo = ScriptInfo
  { cborHex :: Text
  -- ^ The validator script encoded as cbor hex.
  , rawHex :: Text
  -- ^ The validator script encoded as raw hex.
  , hash :: ScriptHash
  -- ^ Hash of the validator.
  }
  deriving stock
    ( -- | @since 0.2.0
      Show
    , -- | @since 0.2.0
      Eq
    , -- | @since 0.2.0
      GHC.Generic
    )
  deriving anyclass
    ( -- | @since 0.2.0
      Aeson.ToJSON
    , -- | @since 0.2.0
      Aeson.FromJSON
    )

mkScriptInfo :: Script -> ScriptInfo
mkScriptInfo script =
  let scriptRaw = LBS.toStrict $ Codec.serialise script
      scriptCBOR = CBOR.serialize' $ SBS.toShort scriptRaw
   in ScriptInfo
        { cborHex = Base16.encodeBase16 scriptCBOR
        , rawHex = Base16.encodeBase16 scriptRaw
        , hash = scriptHash script
        }

{- | Create a 'ScriptInfo' given a Plutarch term of a policy.

     @since 0.2.0
-}
mkPolicyInfo :: ClosedTerm PMintingPolicy -> ScriptInfo
mkPolicyInfo term =
  mkScriptInfo (getMintingPolicy $ mkMintingPolicy term)

{- | Create a 'ScriptInfo' given a Plutarch term of a validator.

     @since 0.2.0
-}
mkValidatorInfo :: ClosedTerm PValidator -> ScriptInfo
mkValidatorInfo term =
  mkScriptInfo (getValidator $ mkValidator term)
