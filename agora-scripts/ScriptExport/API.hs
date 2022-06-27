{- | Module   : ScriptExport.API
     Maintainer : emi@haskell.fyi
     Description: API for script exporter.

     API for script exporter.
-}
module ScriptExport.API (
  API,
  runServer,
) where

import Codec.Serialise.Orphans ()
import Data.Aeson qualified as Aeson
import Data.Cache.Cached (cachedForM)
import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import GHC.Generics qualified as GHC
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (CorsResourcePolicy (corsRequestHeaders), cors, simpleCorsResourcePolicy)
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, hsep, layoutPretty, viaShow)
import Prettyprinter.Render.String (renderString)
import ScriptExport.Options (Options (..))
import ScriptExport.Types (Builders, ScriptQuery, runQuery)
import ScriptExport.Types qualified as Builders
import Servant.API (Get, JSON, Post, ReqBody, (:<|>) (..), type (:>))
import Servant.Server qualified as Servant
import System.Clock (TimeSpec (TimeSpec))
import Text.Printf (printf)

{- | Servant API type for script generation.

     @since 0.2.0
-}
type API =
  "query-script"
    :> ReqBody '[JSON] ScriptQuery
    :> Post '[JSON] Aeson.Value
    :<|> "info"
    :> Get '[JSON] ServerInfo

{- | Information about the server.

     @since 0.2.0
-}
data ServerInfo = ServerInfo
  { revision :: Text
  , exposedBuilders :: [Text]
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
    )

-- | Run a Warp server that exposes a script generation endpoint.
runServer :: Text -> Builders -> Options -> IO ()
runServer revision builders options = do
  let settings =
        Warp.defaultSettings
          & Warp.setPort options.port
          & Warp.setLogger
            ( \req status _maybeFileSize ->
                putStrLn . renderString . layoutPretty defaultLayoutOptions $
                  hsep
                    [ "[info]"
                    , viaShow $ Wai.requestMethod req
                    , viaShow $ Wai.rawPathInfo req
                    , "(" <> pretty (Http.statusCode status) <> ")"
                    ]
            )

      corsPolicy =
        simpleCorsResourcePolicy
          { -- NOTE: Webpack dev server requires this for CORS workaround.
            corsRequestHeaders = "content-type" : corsRequestHeaders simpleCorsResourcePolicy
          }
      corsMiddleware = cors . const $ Just corsPolicy

  -- Scripts stay cached for five minutes
  query <- cachedForM (Just $ TimeSpec 300 0) (`runQuery` builders)

  let serverInfo =
        ServerInfo
          { revision = revision
          , exposedBuilders = Builders.toList builders
          }

  printf "[info] Running 'agora-scripts' on :%d\n" (Warp.getPort settings)
  Servant.serve (Proxy @API) (query :<|> pure serverInfo)
    & corsMiddleware
    & Warp.runSettings settings
