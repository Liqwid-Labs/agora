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

  -- * Introduction functions
  mkValidatorInfo,
  mkPolicyInfo,
) where

import Agora.Aeson.Orphans ()
import Data.Aeson qualified as Aeson
import GHC.Generics qualified as GHC
import Plutarch.Api.V1 (PMintingPolicy, PValidator, mintingPolicySymbol, mkMintingPolicy, mkValidator, validatorHash)
import Plutus.V1.Ledger.Api (MintingPolicy, Validator, ValidatorHash)
import Plutus.V1.Ledger.Value (CurrencySymbol)

-- | Bundle containing a 'Validator' and its hash.
data ValidatorInfo = ValidatorInfo
  { script :: Validator
  , hash :: ValidatorHash
  }
  deriving stock (Show, Eq, GHC.Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

-- | Create a 'ValidatorInfo' given a Plutarch term.
mkValidatorInfo :: ClosedTerm PValidator -> ValidatorInfo
mkValidatorInfo term =
  ValidatorInfo
    { script = validator
    , hash = validatorHash validator
    }
  where
    validator = mkValidator term

-- | Bundle containing a 'MintingPolicy' and its symbol.
data PolicyInfo = PolicyInfo
  { policy :: MintingPolicy
  , currencySymbol :: CurrencySymbol
  }
  deriving stock (Show, Eq, GHC.Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

-- | Create a 'PolicyInfo' given a Plutarch term.
mkPolicyInfo :: ClosedTerm PMintingPolicy -> PolicyInfo
mkPolicyInfo term =
  PolicyInfo
    { policy = policy
    , currencySymbol = mintingPolicySymbol policy
    }
  where
    policy = mkMintingPolicy term
