{- |
Module     : ScriptExport.Types
Maintainer : emi@haskell.fyi
Description: Param and script types for generation.

Param and script types for generation.
-}
module ScriptExport.Types (
  ScriptQuery (..),
  Builders,
  runQuery,
  insertBuilder,
  toList,
) where

import Data.Aeson qualified as Aeson
import Data.Coerce (coerce)
import Data.Default.Class (Default (def))
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics qualified as GHC

{- | Query data for getting script info.

     @since 0.2.0
-}
data ScriptQuery = ScriptQuery
  { name :: Text
  , paramsPayload :: Aeson.Value
  }
  deriving anyclass
    ( -- | @since 0.2.0
      Aeson.ToJSON
    , -- | @since 0.2.0
      Aeson.FromJSON
    )
  deriving stock
    ( -- | @since 0.2.0
      Show
    , -- | @since 0.2.0
      Eq
    , -- | @since 0.2.0
      GHC.Generic
    , -- | @since 0.2.0
      Ord
    )
  deriving anyclass
    ( -- | @since 0.2.0
      Hashable
    )

{- | Run a query on Builders.

     @since 0.2.0
-}
runQuery :: ScriptQuery -> Builders -> Aeson.Value
runQuery s =
  maybe Aeson.Null ($ s.paramsPayload) . Map.lookup s.name . getBuilders

{- | Represents a list of named pure functions.

     @since 0.2.0
-}
newtype Builders = Builders
  { getBuilders :: Map Text (Aeson.Value -> Aeson.Value)
  }

-- | @since 0.2.0
instance Default Builders where
  def = Builders Map.empty

{- | Insert a pure function into the Builders map.

     @since 0.2.0
-}
insertBuilder ::
  forall p s.
  (Aeson.FromJSON p, Aeson.ToJSON s) =>
  Text ->
  (p -> s) ->
  Builders ->
  Builders
insertBuilder k = coerce . Map.insert k . throughJSON
  where
    throughJSON f = Aeson.toJSON . \case { Aeson.Error _ -> Nothing; Aeson.Success v -> Just (f v) } . Aeson.fromJSON

{- | Get a list of the available builders.

     @since 0.2.0
-}
toList :: Builders -> [Text]
toList = Map.keys . getBuilders
