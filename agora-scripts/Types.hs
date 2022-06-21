{- |
Module     : Types
Maintainer : emi@haskell.fyi
Description: Param and script types for generation.

Param and script types for generation.
-}
module Types (ScriptParams (..), AgoraScripts (..)) where

import Agora.SafeMoney (GTTag)
import Agora.ScriptInfo (PolicyInfo, ValidatorInfo)
import Data.Aeson qualified as Aeson
import Data.Tagged (Tagged)
import GHC.Generics qualified as GHC
import PlutusLedgerApi.V1 (TxOutRef)
import PlutusLedgerApi.V1.Value (AssetClass)

-- | Params required for creating script export.
data ScriptParams where
  ScriptParams ::
    { governorInitialSpend :: TxOutRef
    , gtClassRef :: Tagged GTTag AssetClass
    , maximumCosigners :: Integer
    } ->
    ScriptParams
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
  deriving stock (Show, Eq, GHC.Generic, Ord)

-- | Scripts that get exported.
data AgoraScripts = AgoraScripts
  { gitRev :: String
  , governorPolicyInfo :: PolicyInfo
  , governorValidatorInfo :: ValidatorInfo
  , stakePolicyInfo :: PolicyInfo
  , stakeValidatorInfo :: ValidatorInfo
  , proposalPolicyInfo :: PolicyInfo
  , proposalValidatorInfo :: ValidatorInfo
  , treasuryValidatorInfo :: ValidatorInfo
  , authorityTokenPolicyInfo :: PolicyInfo
  }
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
  deriving stock (Show, Eq, GHC.Generic)
