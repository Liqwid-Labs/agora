{-# LANGUAGE TemplateHaskell #-}
{- |
Module     : API
Maintainer : emi@haskell.fyi
Description: API for script exporter.

API for script exporter.
-}

module API (AgoraScripts(..), ScriptParams(..), API, agoraScripts, runServer) where

import Servant.API (type (:>), ReqBody, JSON, Post)
import PlutusLedgerApi.V1 (TxOutRef)
import Data.Tagged (Tagged)
import Agora.SafeMoney (GTTag)
import PlutusLedgerApi.V1.Value (AssetClass, CurrencySymbol)
import qualified Data.Aeson as Aeson
import qualified GHC.Generics as GHC
import Agora.ScriptInfo (PolicyInfo, ValidatorInfo, mkPolicyInfo, mkValidatorInfo)
import Agora.Stake (Stake(..))
import Agora.Proposal (Proposal(..))
import Agora.AuthorityToken (AuthorityToken(..), authorityTokenPolicy)
import Agora.Governor (Governor(..))
import Development.GitRev (gitBranch, gitHash)
import qualified Agora.Governor as Governor
import qualified PlutusLedgerApi.V1.Value as Value
import Plutarch.Api.V1 (mintingPolicySymbol, mkMintingPolicy)
import Agora.Governor.Scripts (governorPolicy, authorityTokenSymbolFromGovernor, authorityTokenFromGovernor, proposalFromGovernor, stakeFromGovernor, governorValidator)
import Agora.Treasury (treasuryValidator)
import Agora.Proposal.Scripts (proposalValidator, proposalPolicy)
import Agora.Stake.Scripts (stakeValidator, stakePolicy)
import qualified Servant.Server as Servant
import Data.Proxy (Proxy(Proxy))
import qualified Network.Wai.Handler.Warp as Warp
import Data.Function ((&))
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as Http
import Prettyprinter (layoutPretty, defaultLayoutOptions, hsep, viaShow, (<+>))
import Prettyprinter.Render.String (renderString)
import Options (Options(..))
import Text.Printf (printf)

-- | Params required for creating script export.
data ScriptParams = ScriptParams
  { governorInitialSpend :: TxOutRef
  , gtClassRef :: Tagged GTTag AssetClass
  , maximumCosigners :: Integer
  }
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
  deriving stock (Show, Eq, GHC.Generic)

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

-- | Servant API type for script generation.
type API = "generate-scripts" :> ReqBody '[JSON] ScriptParams :> Post '[JSON] AgoraScripts

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

  printf "[info] Running 'agora-scripts' on :%d\n" (Warp.getPort settings)
  Warp.runSettings settings $ Servant.serve (Proxy @API) (pure . agoraScripts)

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
