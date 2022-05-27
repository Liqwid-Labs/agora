{- |
Module     : Scripts
Maintainer : emi@haskell.fyi
Description: Export scripts given configuration.

Export scripts given configuration.
-}
module Main (main) where

import Agora.AuthorityToken (AuthorityToken, authorityTokenPolicy)
import Agora.Governor (Governor (Governor))
import Agora.Governor qualified as Governor
import Agora.Governor.Scripts (
  authorityTokenFromGovernor,
  authorityTokenSymbolFromGovernor,
  governorPolicy,
  governorValidator,
  proposalFromGovernor,
  stakeFromGovernor,
 )
import Agora.Proposal (Proposal)
import Agora.Proposal.Scripts (proposalPolicy, proposalValidator)
import Agora.SafeMoney (GTTag)
import Agora.ScriptInfo (PolicyInfo, ValidatorInfo, mkPolicyInfo, mkValidatorInfo)
import Agora.Stake (Stake)
import Agora.Stake.Scripts (stakePolicy, stakeValidator)
import Agora.Treasury (treasuryValidator)
import Control.Monad ((>=>))
import Data.Aeson qualified as Aeson
import Data.Tagged (Tagged)
import GHC.Generics qualified as GHC
import Options (Options (..), parseOptions)
import Plutarch.Api.V1 (mintingPolicySymbol, mkMintingPolicy)
import Plutus.V1.Ledger.Api (TxOutRef)
import Plutus.V1.Ledger.Value (AssetClass, CurrencySymbol)
import Plutus.V1.Ledger.Value qualified as Value
import System.Exit (exitFailure)
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
  { governorPolicyInfo :: PolicyInfo
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

main :: IO ()
main = do
  options <- parseOptions

  params <-
    Aeson.eitherDecodeFileStrict @ScriptParams options.config
      >>= either (putStrLn >=> const exitFailure) pure

  let scripts = agoraScripts params

  Aeson.encodeFile options.output scripts

  printf "Done! Wrote to %s\n" options.output

-- | Create scripts from params.
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
