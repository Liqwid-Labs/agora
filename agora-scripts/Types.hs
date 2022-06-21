{- |
Module     : Types
Maintainer : emi@haskell.fyi
Description: Param and script types for generation.

Param and script types for generation.
-}
module Types (
  ScriptParams (..),
  AgoraScripts (..),
  ScriptQuery (..),
  Builders (..),
  throughJSON,
  runQuery,
  insertBuilder,
) where

import Agora.SafeMoney (GTTag)
import Agora.ScriptInfo (PolicyInfo, ValidatorInfo)
import Data.Aeson qualified as Aeson
import Data.Coerce (coerce)
import Data.Default.Class (Default (def))
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Tagged (Tagged)
import Data.Text (Text)
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

data ScriptQuery = ScriptQuery
  { name :: Text
  , paramsPayload :: Aeson.Value
  }
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
  deriving stock (Show, Eq, GHC.Generic, Ord)
  deriving anyclass (Hashable)

-- | Run a query on Builders.
runQuery :: ScriptQuery -> Builders -> Aeson.Value
runQuery s (Builders b) =
  maybe Aeson.Null ($ s.paramsPayload) (Map.lookup s.name b)

throughJSON :: (Aeson.FromJSON p, Aeson.ToJSON s) => (p -> s) -> (Aeson.Value -> Aeson.Value)
throughJSON f = Aeson.toJSON . \case { Aeson.Error _ -> Nothing; Aeson.Success v -> Just (f v) } . Aeson.fromJSON

-- | Represents a list of named pure functions.
newtype Builders = Builders
  { getBuilders :: Map Text (Aeson.Value -> Aeson.Value)
  }

instance Default Builders where
  def = Builders Map.empty

-- | Insert a pure function into the Builders map.
insertBuilder :: (Aeson.FromJSON p, Aeson.ToJSON s) => Text -> (p -> s) -> Builders -> Builders
insertBuilder k = coerce . Map.insert k . throughJSON
