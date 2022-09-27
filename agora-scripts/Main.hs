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
import Agora.Utils (CompiledMintingPolicy, CompiledValidator)
import Data.Aeson qualified as Aeson
import Data.Default (def)
import Data.Function ((&))
import Data.Tagged (Tagged)
import Data.Text (Text)
import Development.GitRev (gitBranch, gitHash)
import GHC.Generics qualified as GHC
import Optics.Core (view)
import Plutarch (Config (Config, tracingMode), TracingMode (DoTracing, NoTracing))
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
    & insertBuilder "governorPolicy" (view #governorPolicyInfo . agoraScripts)
    & insertBuilder "governorValidator" (view #governorValidatorInfo . agoraScripts)
    & insertBuilder "stakePolicy" (view #stakePolicyInfo . agoraScripts)
    & insertBuilder "stakeValidator" (view #stakeValidatorInfo . agoraScripts)
    & insertBuilder "proposalPolicy" (view #proposalPolicyInfo . agoraScripts)
    & insertBuilder "proposalValidator" (view #proposalValidatorInfo . agoraScripts)
    & insertBuilder "treasuryValidator" (view #treasuryValidatorInfo . agoraScripts)
    & insertBuilder "authorityTokenPolicy" (view #authorityTokenPolicyInfo . agoraScripts)
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
    -- Provided Effect scripts
    & insertBuilder "treasuryWithdrawalEffect" (view #treasuryWithdrawalEffectInfo . agoraScripts)

{- | Create scripts from params.

     @since 0.2.0
-}
agoraScripts :: ScriptParams -> AgoraScripts
agoraScripts params =
  AgoraScripts
    { governorPolicyInfo = mkPolicyInfo' (view #compiledGovernorPolicy scripts)
    , governorValidatorInfo = mkValidatorInfo' (view #compiledGovernorValidator scripts)
    , stakePolicyInfo = mkPolicyInfo' (view #compiledStakePolicy scripts)
    , stakeValidatorInfo = mkValidatorInfo' (view #compiledStakeValidator scripts)
    , proposalPolicyInfo = mkPolicyInfo' (view #compiledProposalPolicy scripts)
    , proposalValidatorInfo = mkValidatorInfo' (view #compiledProposalValidator scripts)
    , treasuryValidatorInfo = mkValidatorInfo' (view #compiledTreasuryValidator scripts)
    , authorityTokenPolicyInfo = mkPolicyInfo' (view #compiledAuthorityTokenPolicy scripts)
    , treasuryWithdrawalEffectInfo = mkValidatorInfo' (view #compiledTreasuryWithdrawalEffect scripts)
    }
  where
    governor =
      Agora.Governor.Governor
        (view #governorInitialSpend params)
        (view #gtClassRef params)
        (view #maximumCosigners params)

    scripts = Bootstrap.agoraScripts plutarchConfig governor

    plutarchConfig :: Config
    plutarchConfig = Config {tracingMode = if view #tracing params then DoTracing else NoTracing}

{- | Params required for creating script export.

     @since 1.0.0
-}
data ScriptParams = ScriptParams
  { governorInitialSpend :: TxOutRef
  , gtClassRef :: Tagged GTTag AssetClass
  , maximumCosigners :: Integer
  , tracing :: Bool
  }
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
  , treasuryWithdrawalEffectInfo :: ScriptInfo
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

{- | Turn a precompiled minting policy to a 'ScriptInfo'.

     @since 0.2.0
-}
mkPolicyInfo' :: forall redeemer. CompiledMintingPolicy redeemer -> ScriptInfo
mkPolicyInfo' = mkScriptInfo . getMintingPolicy . view #getCompiledMintingPolicy

{- | Turn a precompiled validator to a 'ScriptInfo'.

     @since 0.2.0
-}
mkValidatorInfo' :: forall redeemer datum. CompiledValidator datum redeemer -> ScriptInfo
mkValidatorInfo' = mkScriptInfo . getValidator . view #getCompiledValidator
