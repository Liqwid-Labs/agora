{-# LANGUAGE TemplateHaskell #-}

{- | Module     : API
   Maintainer : emi@haskell.fyi
   Description: API for script exporter.

   API for script exporter.
-}
module API (
  AgoraScripts (..),
  ScriptParams (..),
  API,
  agoraScripts,
  runServer,
) where

import Agora.AuthorityToken (AuthorityToken (..), authorityTokenPolicy)
import Agora.Governor (Governor (..))
import Agora.Governor qualified as Governor
import Agora.Governor.Scripts (authorityTokenFromGovernor, authorityTokenSymbolFromGovernor, governorPolicy, governorValidator, proposalFromGovernor, stakeFromGovernor)
import Agora.Proposal (Proposal (..))
import Agora.Proposal.Scripts (proposalPolicy, proposalValidator)
import Agora.ScriptInfo (mkPolicyInfo, mkValidatorInfo)
import Agora.Stake (Stake (..))
import Agora.Stake.Scripts (stakePolicy, stakeValidator)
import Agora.Treasury (treasuryValidator)
import Codec.Serialise.Orphans ()
import Data.Aeson qualified as Aeson
import Data.Cache.Cached (cachedFor)
import Data.Default.Class (Default (def))
import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import Development.GitRev (gitBranch, gitHash)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (CorsResourcePolicy (corsRequestHeaders), cors, simpleCorsResourcePolicy)
import Options (Options (..))
import Plutarch.Api.V1 (mintingPolicySymbol, mkMintingPolicy)
import PlutusLedgerApi.V1.Value (AssetClass, CurrencySymbol)
import PlutusLedgerApi.V1.Value qualified as Value
import Prettyprinter (defaultLayoutOptions, hsep, layoutPretty, viaShow, (<+>))
import Prettyprinter.Render.String (renderString)
import Servant.API (JSON, Post, ReqBody, (:<|>) (..), type (:>))
import Servant.Server qualified as Servant
import System.Clock (TimeSpec (TimeSpec))
import Text.Printf (printf)
import Types (AgoraScripts (..), Builders, ScriptParams (..), ScriptQuery, insertBuilder, runQuery)

-- | Servant API type for script generation.
type API =
  "generate-scripts" :> ReqBody '[JSON] ScriptParams :> Post '[JSON] AgoraScripts
    :<|> "query-script" :> ReqBody '[JSON] ScriptQuery :> Post '[JSON] Aeson.Value

-- | Run a Warp server that exposes a script generation endpoint.
runServer :: Options -> IO ()
runServer options = do
  let settings =
        Warp.defaultSettings
          & Warp.setPort options.port
          & Warp.setLogger
            ( \req status _maybeFileSize ->
                putStrLn . renderString . layoutPretty defaultLayoutOptions $
                  hsep
                    [ "[info]"
                    , "[" <> "Status:" <+> viaShow (Http.statusCode status) <> "]"
                    , viaShow $ Wai.requestMethod req
                    , viaShow $ Wai.rawPathInfo req
                    ]
            )

      corsPolicy =
        simpleCorsResourcePolicy
          { -- NOTE: Webpack dev server requires this for CORS workaround.
            corsRequestHeaders = "content-type" : corsRequestHeaders simpleCorsResourcePolicy
          }
      corsMiddleware = cors . const $ Just corsPolicy

  -- Scripts stay cached for five minutes
  agoraScripts' <- cachedFor (Just $ TimeSpec 300 0) agoraScripts
  query <- cachedFor (Just $ TimeSpec 300 0) (`runQuery` agoraBuilders)

  printf "[info] Running 'agora-scripts' on :%d\n" (Warp.getPort settings)
  Servant.serve
    (Proxy @API)
    (agoraScripts' :<|> query)
    & corsMiddleware
    & Warp.runSettings settings

agoraBuilders :: Builders
agoraBuilders =
  def
    & insertBuilder "governorPolicy" ((.governorPolicyInfo) . agoraScripts)
    & insertBuilder "governorValidator" ((.governorValidatorInfo) . agoraScripts)
    & insertBuilder "stakePolicyInfo" ((.stakePolicyInfo) . agoraScripts)
    & insertBuilder "stakeValidatorInfo" ((.stakeValidatorInfo) . agoraScripts)
    & insertBuilder "proposalPolicyInfo" ((.proposalPolicyInfo) . agoraScripts)
    & insertBuilder "proposalValidatorInfo" ((.proposalValidatorInfo) . agoraScripts)
    & insertBuilder "treasuryValidatorInfo" ((.treasuryValidatorInfo) . agoraScripts)
    & insertBuilder "authorityTokenPolicyInfo" ((.authorityTokenPolicyInfo) . agoraScripts)

-- | Create scripts from params.
agoraScripts :: ScriptParams -> AgoraScripts
agoraScripts params =
  AgoraScripts
    { gitRev = revision
    , governorPolicyInfo = mkPolicyInfo (governorPolicy governor)
    , governorValidatorInfo = mkValidatorInfo (governorValidator governor)
    , stakePolicyInfo = mkPolicyInfo (stakePolicy params.gtClassRef)
    , stakeValidatorInfo = mkValidatorInfo (stakeValidator stake)
    , proposalPolicyInfo = mkPolicyInfo (proposalPolicy governorSTAssetClass)
    , proposalValidatorInfo = mkValidatorInfo (proposalValidator proposal)
    , treasuryValidatorInfo = mkValidatorInfo (treasuryValidator authorityTokenSymbol)
    , authorityTokenPolicyInfo = mkPolicyInfo (authorityTokenPolicy authorityToken)
    }
  where
    -- This encodes the git revision of the server. It's useful for the caller
    -- to be able to ensure they are compatible with it.
    revision :: String
    revision = $(gitBranch) <> "@" <> $(gitHash)

    governor :: Governor
    governor =
      Governor
        { Governor.gstOutRef = params.governorInitialSpend
        , Governor.gtClassRef = params.gtClassRef
        , Governor.maximumCosigners = params.maximumCosigners
        }

    authorityToken :: AuthorityToken
    authorityToken = authorityTokenFromGovernor governor

    authorityTokenSymbol :: CurrencySymbol
    authorityTokenSymbol = authorityTokenSymbolFromGovernor governor

    governorSTAssetClass :: AssetClass
    governorSTAssetClass =
      Value.assetClass (mintingPolicySymbol $ mkMintingPolicy $ governorPolicy governor) ""

    proposal :: Proposal
    proposal = proposalFromGovernor governor

    stake :: Stake
    stake = stakeFromGovernor governor
