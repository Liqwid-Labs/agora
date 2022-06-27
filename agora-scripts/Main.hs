{-# LANGUAGE TemplateHaskell #-}

{- | Module     : Main
     Maintainer : emi@haskell.fyi
     Description: Export scripts given configuration.

     Export scripts given configuration.
-}
module Main (main) where

import Agora.AuthorityToken (AuthorityToken, authorityTokenPolicy)
import Agora.Governor (Governor (Governor))
import Agora.Governor qualified as Governor
import Agora.Governor.Scripts (authorityTokenFromGovernor, authorityTokenSymbolFromGovernor, governorPolicy, governorValidator, proposalFromGovernor, stakeFromGovernor)
import Agora.Proposal (Proposal)
import Agora.Proposal.Scripts (proposalPolicy, proposalValidator)
import Agora.SafeMoney (GTTag)
import Agora.ScriptInfo (ScriptInfo, mkPolicyInfo, mkValidatorInfo)
import Agora.Stake (Stake)
import Agora.Stake.Scripts (stakePolicy, stakeValidator)
import Agora.Treasury (treasuryValidator)
import Data.Aeson qualified as Aeson
import Data.Default (def)
import Data.Function ((&))
import Data.Tagged (Tagged)
import Data.Text (Text)
import Development.GitRev (gitBranch, gitHash)
import GHC.Generics qualified as GHC
import Plutarch.Api.V1 (mintingPolicySymbol, mkMintingPolicy)
import PlutusLedgerApi.V1 (TxOutRef)
import PlutusLedgerApi.V1.Value (AssetClass, CurrencySymbol)
import PlutusLedgerApi.V1.Value qualified as Value
import ScriptExport.API (runServer)
import ScriptExport.Options (parseOptions)
import ScriptExport.Types (Builders, insertBuilder)

main :: IO ()
main =
  parseOptions >>= runServer revision builders
  where
    -- This encodes the git revision of the server. It's useful for the caller
    -- to be able to ensure they are compatible with it.
    revision :: Text
    revision = $(gitBranch) <> "@" <> $(gitHash)

{- | Builders for Agora scripts.

     @since 0.2.0
-}
builders :: Builders
builders =
  def
    & insertBuilder "governorPolicy" ((.governorPolicyInfo) . agoraScripts)
    & insertBuilder "governorValidator" ((.governorValidatorInfo) . agoraScripts)
    & insertBuilder "stakePolicyInfo" ((.stakePolicyInfo) . agoraScripts)
    & insertBuilder "stakeValidatorInfo" ((.stakeValidatorInfo) . agoraScripts)
    & insertBuilder "proposalPolicyInfo" ((.proposalPolicyInfo) . agoraScripts)
    & insertBuilder "proposalValidatorInfo" ((.proposalValidatorInfo) . agoraScripts)
    & insertBuilder "treasuryValidatorInfo" ((.treasuryValidatorInfo) . agoraScripts)
    & insertBuilder "authorityTokenPolicyInfo" ((.authorityTokenPolicyInfo) . agoraScripts)

{- | Create scripts from params.

     @since 0.2.0
-}
agoraScripts :: ScriptParams -> AgoraScripts
agoraScripts params =
  AgoraScripts
    { governorPolicyInfo = mkPolicyInfo (governorPolicy governor)
    , governorValidatorInfo = mkValidatorInfo (governorValidator governor)
    , stakePolicyInfo = mkPolicyInfo (stakePolicy params.gtClassRef)
    , stakeValidatorInfo = mkValidatorInfo (stakeValidator stake)
    , proposalPolicyInfo = mkPolicyInfo (proposalPolicy governorSTAssetClass)
    , proposalValidatorInfo = mkValidatorInfo (proposalValidator proposal)
    , treasuryValidatorInfo = mkValidatorInfo (treasuryValidator authorityTokenSymbol)
    , authorityTokenPolicyInfo = mkPolicyInfo (authorityTokenPolicy authorityToken)
    }
  where
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

{- | Params required for creating script export.

     @since 0.2.0
-}
data ScriptParams where
  ScriptParams ::
    { governorInitialSpend :: TxOutRef
    , gtClassRef :: Tagged GTTag AssetClass
    , maximumCosigners :: Integer
    } ->
    ScriptParams
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
  deriving stock (Show, Eq, GHC.Generic, Ord)

{- | Scripts that get exported.

     @since 0.2.0
-}
data AgoraScripts = AgoraScripts
  { governorPolicyInfo :: ScriptInfo
  , governorValidatorInfo :: ScriptInfo
  , stakePolicyInfo :: ScriptInfo
  , stakeValidatorInfo :: ScriptInfo
  , proposalPolicyInfo :: ScriptInfo
  , proposalValidatorInfo :: ScriptInfo
  , treasuryValidatorInfo :: ScriptInfo
  , authorityTokenPolicyInfo :: ScriptInfo
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
