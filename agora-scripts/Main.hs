{-# LANGUAGE TemplateHaskell #-}

{- | Module     : Main
     Maintainer : emi@haskell.fyi
     Description: Export scripts given configuration.

     Export scripts given configuration.
-}
module Main (main) where

import Agora.Bootstrap qualified as Bootstrap
import Agora.Governor (Governor (Governor))
import Agora.SafeMoney (GTTag)
import Agora.Scripts qualified as Scripts
import Agora.Utils (CompiledMintingPolicy (getCompiledMintingPolicy), CompiledValidator (getCompiledValidator))
import Data.Aeson qualified as Aeson
import Data.Default (def)
import Data.Function ((&))
import Data.Tagged (Tagged)
import Data.Text (Text)
import Development.GitRev (gitBranch, gitHash)
import GHC.Generics qualified as GHC
import Plutarch (Config (Config, tracingMode), TracingMode (DoTracing))
import PlutusLedgerApi.V1 (
  MintingPolicy (getMintingPolicy),
  TxOutRef,
  Validator (getValidator),
 )
import PlutusLedgerApi.V1.Value (AssetClass)
import ScriptExport.API (runServer)
import ScriptExport.Options (parseOptions)
import ScriptExport.ScriptInfo (ScriptInfo, mkPolicyInfo, mkScriptInfo, mkValidatorInfo)
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
    -- Agora scripts
    & insertBuilder "governorPolicy" ((.governorPolicyInfo) . agoraScripts)
    & insertBuilder "governorValidator" ((.governorValidatorInfo) . agoraScripts)
    & insertBuilder "stakePolicy" ((.stakePolicyInfo) . agoraScripts)
    & insertBuilder "stakeValidator" ((.stakeValidatorInfo) . agoraScripts)
    & insertBuilder "proposalPolicy" ((.proposalPolicyInfo) . agoraScripts)
    & insertBuilder "proposalValidator" ((.proposalValidatorInfo) . agoraScripts)
    & insertBuilder "treasuryValidator" ((.treasuryValidatorInfo) . agoraScripts)
    & insertBuilder "authorityTokenPolicy" ((.authorityTokenPolicyInfo) . agoraScripts)
    -- Trivial scripts. These are useful for testing, but they likely aren't useful
    -- to you if you are actually interested in deploying to mainnet.
    & insertBuilder
      "alwaysSucceedsPolicy"
      (\() -> mkPolicyInfo $ plam $ \_ _ -> popaque (pconstant ()))
    & insertBuilder
      "alwaysSucceedsValidator"
      (\() -> mkValidatorInfo $ plam $ \_ _ _ -> popaque (pconstant ()))
    & insertBuilder
      "neverSucceedsPolicy"
      (\() -> mkPolicyInfo $ plam $ \_ _ -> perror)
    & insertBuilder
      "neverSucceedsValidator"
      (\() -> mkValidatorInfo $ plam $ \_ _ _ -> perror)

{- | Create scripts from params.

     @since 0.2.0
-}
agoraScripts :: ScriptParams -> AgoraScripts
agoraScripts params =
  AgoraScripts
    { governorPolicyInfo = mkPolicyInfo' scripts.compiledGovernorPolicy
    , governorValidatorInfo = mkValidatorInfo' scripts.compiledGovernorValidator
    , stakePolicyInfo = mkPolicyInfo' scripts.compiledStakePolicy
    , stakeValidatorInfo = mkValidatorInfo' scripts.compiledStakeValidator
    , proposalPolicyInfo = mkPolicyInfo' scripts.compiledProposalPolicy
    , proposalValidatorInfo = mkValidatorInfo' scripts.compiledProposalValidator
    , treasuryValidatorInfo = mkValidatorInfo' scripts.compiledTreasuryValidator
    , authorityTokenPolicyInfo = mkPolicyInfo' scripts.compiledAuthorityTokenPolicy
    }
  where
    governor =
      Agora.Governor.Governor
        params.governorInitialSpend
        params.gtClassRef
        params.maximumCosigners

    scripts = Bootstrap.agoraScripts plutarchConfig governor

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

{- | Default plutarch configuration for compiling scripts.

     TODO: we should have an option to control this.

     @since 0.2.0
-}
plutarchConfig :: Config
plutarchConfig = Config {tracingMode = DoTracing}

{- | Turn a precompiled minting policy to a 'ScriptInfo'.

     @since 0.2.0
-}
mkPolicyInfo' :: forall redeemer. CompiledMintingPolicy redeemer -> ScriptInfo
mkPolicyInfo' = mkScriptInfo . getMintingPolicy . getCompiledMintingPolicy

{- | Turn a precompiled validator to a 'ScriptInfo'.

     @since 0.2.0
-}
mkValidatorInfo' :: forall redeemer datum. CompiledValidator datum redeemer -> ScriptInfo
mkValidatorInfo' = mkScriptInfo . getValidator . getCompiledValidator
