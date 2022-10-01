{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-all #-}

{- | Module     : Main
     Maintainer : emi@haskell.fyi
     Description: Export scripts given configuration.

     Export scripts given configuration.
-}
module Main (main) where

import Agora.Bootstrap qualified as Bootstrap
import Agora.Governor (Governor (Governor, gstOutRef, gtClassRef, maximumCosigners))
import Agora.SafeMoney (GTTag)
import Agora.Utils (validatorHashToAddress)
import Data.Aeson qualified as Aeson
import Data.Default (def)
import Data.Function ((&))
import Data.Tagged (Tagged, untag)
import Data.Text (Text, unpack)
import Development.GitRev (gitBranch, gitHash)
import GHC.Generics qualified as GHC
import Plutarch (Config (Config, tracingMode), TracingMode (DoTracing, NoTracing))
import Plutarch.Api.V2 (mintingPolicySymbol, validatorHash)
import PlutusLedgerApi.V1 (TxOutRef, Address, CurrencySymbol)
import PlutusLedgerApi.V1.Value (AssetClass(AssetClass))
import ScriptExport.API (runServer)
import ScriptExport.Options (parseOptions)
import ScriptExport.ScriptInfo (ScriptInfo, mkPolicyInfo, mkScriptInfo, mkValidatorInfo)
import ScriptExport.Types (Builders, insertBuilder)
import Ply
import Ply.Core.TypedReader

import System.Environment
import Data.Default
import Data.Map
import Control.Arrow
import Data.Kind (Type)
import Data.Coerce

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

linker :: ScriptParams -> Map Text TypedScriptEnvelope -> Map Text ScriptInfo
linker param as = either error id $ do
  govPol <- get @MintingPolicyRole @'[TxOutRef] "agora:governorPolicy"
  govVal <- get @ValidatorRole @'[Address, CurrencySymbol, CurrencySymbol, CurrencySymbol, CurrencySymbol] "agora:governorValidator"
  stkPol <- get @MintingPolicyRole @'[AssetClass] "agora:stakePolicy"
  stkVal <- get @ValidatorRole @'[CurrencySymbol, AssetClass, AssetClass] "agora:stakeValidator"
  prpPol <- get @MintingPolicyRole @'[AssetClass] "agora:proposalPolicy"
  prpVal <- get @ValidatorRole @'[CurrencySymbol, CurrencySymbol, CurrencySymbol, Integer] "agora:proposalValidator"
  treVal <- get @ValidatorRole @'[CurrencySymbol] "agora:treasuryValidator"
  atkPol <- get @MintingPolicyRole @'[AssetClass] "agora:authorityTokenPolicy"
  treaWithdrawalVal <- get @ValidatorRole @'[CurrencySymbol] "agora:treasuryWithdrawalValidator"
  mutateGovVal <- get @ValidatorRole @'[AssetClass, CurrencySymbol] "agora:mutateGovernorValidator"

  let governor =
        Agora.Governor.Governor
          param.governorInitialSpend
          param.gtClassRef
          param.maximumCosigners

      govPol' = govPol # governor.gstOutRef
      govVal' =
        govVal #
          propValAddress #
          sstSymbol #
          gstSymbol #
          pstSymbol #
          atSymbol
      gstSymbol =
        mintingPolicySymbol $
          toMintingPolicy $
            govPol'
      gstAssetClass =
        AssetClass (gstSymbol, "")

      at = gstAssetClass
      atPol' = atkPol # at
      atSymbol = mintingPolicySymbol $ toMintingPolicy $ atPol'

      propPol' = prpPol # gstAssetClass
      propVal' =
        prpVal #
          sstSymbol #
          gstSymbol #
          pstSymbol #
          governor.maximumCosigners
      propValAddress =
        validatorHashToAddress $ validatorHash $ toValidator propVal'
      pstSymbol = mintingPolicySymbol $ toMintingPolicy propPol'
      pstAssetClass = AssetClass (pstSymbol, "")

      stakPol' = stkPol # untag governor.gtClassRef
      stakVal' = stkVal # sstSymbol # pstAssetClass # gstAssetClass
      sstSymbol = mintingPolicySymbol $ toMintingPolicy stakPol'

      treaVal' = treVal # atSymbol

      treaWithdrawalVal' = treaWithdrawalVal # atSymbol
      mutateGovVal' = mutateGovVal # gstAssetClass # atSymbol

  return $
    fromList
    [ ("agora:governorPolicy", mkScriptInfo' govPol')
    , ("agora:governorValidator", mkScriptInfo' govVal')
    , ("agora:stakePolicy", mkScriptInfo' stakPol')
    , ("agora:stakeValidator", mkScriptInfo' stakVal')
    , ("agora:proposalPolicy", mkScriptInfo' propPol')
    , ("agora:proposalValidator", mkScriptInfo' propVal')
    , ("agora:treasuryValidator", mkScriptInfo' treaVal')
    , ("agora:authorityTokenPolicy", mkScriptInfo' atPol')
    , ("agora:treasuryWithdrawalValidator", mkScriptInfo' treaWithdrawalVal')
    , ("agora:mutateGovernorValidator", mkScriptInfo' mutateGovVal')
    ]
  where
    get ::
      forall (rl :: ScriptRole) (params :: [Type]).
      TypedReader rl params =>
      Text ->
      Either String (TypedScript rl params)
    get t = do
      en <- maybe (Left $ unpack t <> " not found") Right $ as !? t
      left show $ mkTypedScript en

    mkScriptInfo' ::
      forall (rl :: ScriptRole).
      TypedScript rl '[] ->
      ScriptInfo
    mkScriptInfo' = mkScriptInfo . toScript

main :: IO ()
main = do
  dir <- head <$> getArgs

  Bootstrap.writeAgoraScripts def dir

  -- parseOptions >>= runServer revision builders
  -- where
  --   -- This encodes the git revision of the server. It's useful for the caller
  --   -- to be able to ensure they are compatible with it.
  --   revision :: Text
  --   revision = $(gitBranch) <> "@" <> $(gitHash)

-- {- | Builders for Agora scripts.

--      @since 0.2.0
-- -}
-- builders :: Builders
-- builders =
--   def
--     -- Agora scripts
--     & insertBuilder "governorPolicy" ((.governorPolicyInfo) . agoraScripts)
--     & insertBuilder "governorValidator" ((.governorValidatorInfo) . agoraScripts)
--     & insertBuilder "stakePolicy" ((.stakePolicyInfo) . agoraScripts)
--     & insertBuilder "stakeValidator" ((.stakeValidatorInfo) . agoraScripts)
--     & insertBuilder "proposalPolicy" ((.proposalPolicyInfo) . agoraScripts)
--     & insertBuilder "proposalValidator" ((.proposalValidatorInfo) . agoraScripts)
--     & insertBuilder "treasuryValidator" ((.treasuryValidatorInfo) . agoraScripts)
--     & insertBuilder "authorityTokenPolicy" ((.authorityTokenPolicyInfo) . agoraScripts)
--     -- Trivial scripts. These are useful for testing, but they likely aren't useful
--     -- to you if you are actually interested in deploying to mainnet.
--     & insertBuilder
--       "alwaysSucceedsPolicy"
--       (\() -> mkPolicyInfo $ plam $ \_ _ -> popaque (pconstant ()))
--     & insertBuilder
--       "alwaysSucceedsValidator"
--       (\() -> mkValidatorInfo $ plam $ \_ _ _ -> popaque (pconstant ()))
--     & insertBuilder
--       "neverSucceedsPolicy"
--       (\() -> mkPolicyInfo $ plam $ \_ _ -> perror)
--     & insertBuilder
--       "neverSucceedsValidator"
--       (\() -> mkValidatorInfo $ plam $ \_ _ _ -> perror)
--     -- Provided Effect scripts
--     & insertBuilder "treasuryWithdrawalEffect" ((.treasuryWithdrawalEffectInfo) . agoraScripts)

-- {- | Create scripts from params.

--      @since 0.2.0
-- -}
-- agoraScripts :: ScriptParams -> AgoraScripts
-- agoraScripts params =
--   AgoraScripts
--     { governorPolicyInfo = mkPolicyInfo' scripts.compiledGovernorPolicy
--     , governorValidatorInfo = mkValidatorInfo' scripts.compiledGovernorValidator
--     , stakePolicyInfo = mkPolicyInfo' scripts.compiledStakePolicy
--     , stakeValidatorInfo = mkValidatorInfo' scripts.compiledStakeValidator
--     , proposalPolicyInfo = mkPolicyInfo' scripts.compiledProposalPolicy
--     , proposalValidatorInfo = mkValidatorInfo' scripts.compiledProposalValidator
--     , treasuryValidatorInfo = mkValidatorInfo' scripts.compiledTreasuryValidator
--     , authorityTokenPolicyInfo = mkPolicyInfo' scripts.compiledAuthorityTokenPolicy
--     , treasuryWithdrawalEffectInfo = mkValidatorInfo' scripts.compiledTreasuryWithdrawalEffect
--     }
--   where
--     governor =
--       Agora.Governor.Governor
--         params.governorInitialSpend
--         params.gtClassRef
--         params.maximumCosigners

--     scripts = Bootstrap.agoraScripts plutarchConfig governor

--     plutarchConfig :: Config
--     plutarchConfig = Config {tracingMode = if params.tracing then DoTracing else NoTracing}

-- {- | Scripts that get exported.

--      @since 0.2.0
-- -}
-- data AgoraScripts = AgoraScripts
--   { governorPolicyInfo :: ScriptInfo
--   , governorValidatorInfo :: ScriptInfo
--   , stakePolicyInfo :: ScriptInfo
--   , stakeValidatorInfo :: ScriptInfo
--   , proposalPolicyInfo :: ScriptInfo
--   , proposalValidatorInfo :: ScriptInfo
--   , treasuryValidatorInfo :: ScriptInfo
--   , authorityTokenPolicyInfo :: ScriptInfo
--   , treasuryWithdrawalEffectInfo :: ScriptInfo
--   }
--   deriving anyclass
--     ( -- | @since 0.2.0
--       Aeson.ToJSON
--     , -- | @since 0.2.0
--       Aeson.FromJSON
--     )
--   deriving stock
--     ( -- | @since 0.2.0
--       Show
--     , -- | @since 0.2.0
--       Eq
--     , -- | @since 0.2.0
--       GHC.Generic
--     )

-- {- | Turn a precompiled minting policy to a 'ScriptInfo'.

--      @since 0.2.0
-- -}
-- mkPolicyInfo' :: TypedScriptEnvelope -> ScriptInfo
-- mkPolicyInfo' (TypedScriptEnvelope _ MintingPolicyRole _ _ script) =
--   mkScriptInfo script
-- mkPolicyInfo' _ = error "Expected MintingPolicyRole"

-- {- | Turn a precompiled validator to a 'ScriptInfo'.

--      @since 0.2.0
-- -}
-- mkValidatorInfo' :: TypedScriptEnvelope -> ScriptInfo
-- mkValidatorInfo' (TypedScriptEnvelope _ ValidatorRole _ _ script) =
--   mkScriptInfo script
-- mkValidatorInfo' _ = error "Expected ValidatorRole"
