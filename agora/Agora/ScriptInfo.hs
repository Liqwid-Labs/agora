{- |
Module     : Agora.ScriptInfo
Maintainer : emi@haskell.fyi
Description: Exportable script bundles for off-chain consumption.

Exportable script bundles for off-chain consumption.
-}
module Agora.ScriptInfo (
  -- * Types
  PolicyInfo (..),
  ValidatorInfo (..),
  ScriptInfo (..),

  -- * Introduction functions
  mkValidatorInfo,
  mkPolicyInfo,
) where

import Agora.Aeson.Orphans ()
import Data.Aeson qualified as Aeson
import GHC.Generics qualified as GHC
import Plutarch.Api.V1 (PMintingPolicy, PValidator, mintingPolicySymbol, mkMintingPolicy, mkValidator, validatorHash)
import PlutusLedgerApi.V1 (BuiltinByteString, CurrencySymbol (unCurrencySymbol), MintingPolicy, Script, Validator, ValidatorHash, unMintingPolicyScript)

-- | Bundle containing a 'Script' and its hash.
data ScriptInfo = ScriptInfo
  { script :: Script
  -- ^ The validator script.
  , hash :: BuiltinByteString
  -- ^ Hash of the script.
  }
  deriving stock (Show, Eq, GHC.Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

{- | Bundle containing a 'Validator' and its hash.

     @since 0.1.0
-}
data ValidatorInfo = ValidatorInfo
  { script :: Validator
  -- ^ The validator script.
  , hash :: ValidatorHash
  -- ^ Hash of the validator.
  }
  deriving stock
    ( -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      GHC.Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      Aeson.ToJSON
    , -- | @since 0.1.0
      Aeson.FromJSON
    )

{- | Create a 'ValidatorInfo' given a Plutarch term.

     @since 0.1.0
-}
mkValidatorInfo :: ClosedTerm PValidator -> ValidatorInfo
mkValidatorInfo term =
  ValidatorInfo
    { script = validator
    , hash = validatorHash validator
    }
  where
    validator = mkValidator term

{- | Bundle containing a 'MintingPolicy' and its symbol.

     @since 0.1.0
-}
data PolicyInfo = PolicyInfo
  { policy :: MintingPolicy
  -- ^ The minting policy.
  , currencySymbol :: CurrencySymbol
  -- ^ The symbol given by the minting policy.
  }
  deriving stock
    ( -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      GHC.Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      Aeson.ToJSON
    , -- | @since 0.1.0
      Aeson.FromJSON
    )

{- | Create a 'PolicyInfo' given a Plutarch term.

     @since 0.1.0
-}
mkPolicyInfo :: ClosedTerm PMintingPolicy -> PolicyInfo
mkPolicyInfo term =
  PolicyInfo
    { policy = policy
    , currencySymbol = mintingPolicySymbol policy
    }
  where
    policy = mkMintingPolicy term
