{- | Module     : Agora.Bootstrap
     Maintainer : connor@mlabs.city
     Description: Initialize a governance system

     Initialize a governance system
-}
module Agora.Bootstrap (agoraScripts) where

import Agora.AuthorityToken (AuthorityToken (AuthorityToken), authorityTokenPolicy)
import Agora.Effect.TreasuryWithdrawal (treasuryWithdrawalValidator)
import Agora.Governor (Governor, gstOutRef, gtClassRef, maximumCosigners)
import Agora.Governor.Scripts (governorPolicy, governorValidator)
import Agora.Proposal.Scripts (proposalPolicy, proposalValidator)
import Agora.Scripts (AgoraScripts (AgoraScripts), policySymbolEnvelope)
import Agora.Scripts qualified as Scripts
import Agora.Stake.Scripts (stakePolicy, stakeValidator)
import Agora.Treasury (treasuryValidator)
import PlutusLedgerApi.V1.Value (AssetClass (AssetClass))
import Ply (TypedScriptEnvelope)
import Ply.Plutarch.TypedWriter
import Data.Text (Text)
import Plutarch (Config)

mkEnvelope' :: forall (p :: S -> Type). TypedWriter p => Config -> ClosedTerm p -> Text -> TypedScriptEnvelope
mkEnvelope' conf term descr = either (error . show) id $ mkEnvelope conf term descr

{- | Parameterize and precompiled core scripts, given the
     'Agora.Governor.Governor' parameters and plutarch configurations.

     @since 0.2.0
-}
agoraScripts :: Config -> Governor -> AgoraScripts
agoraScripts conf gov =
  AgoraScripts
  { Scripts.compiledGovernorPolicy = governorPolicy'
  , Scripts.compiledGovernorValidator = governorValidator'
  , Scripts.compiledStakePolicy = stakePolicy'
  , Scripts.compiledStakeValidator = stakeValidator'
  , Scripts.compiledProposalPolicy = proposalPolicy'
  , Scripts.compiledProposalValidator = proposalValidator'
  , Scripts.compiledTreasuryValidator = treasuryValidator'
  , Scripts.compiledAuthorityTokenPolicy = authorityPolicy'
  , Scripts.compiledTreasuryWithdrawalEffect = treasuryWithdrawalEffect'
  }
  where
    governorPolicy' = mkEnvelope' conf (governorPolicy gov.gstOutRef) ""
    governorValidator' = mkEnvelope' conf (governorValidator (agoraScripts conf gov)) ""
    governorSymbol = policySymbolEnvelope governorPolicy'
    governorAssetClass = AssetClass (governorSymbol, "")

    authority = AuthorityToken governorAssetClass
    authorityPolicy' = mkEnvelope' conf (authorityTokenPolicy authority) ""
    authorityTokenSymbol = policySymbolEnvelope authorityPolicy'

    proposalPolicy' = mkEnvelope' conf (proposalPolicy governorAssetClass) ""
    proposalValidator' = mkEnvelope' conf (proposalValidator (agoraScripts conf gov) gov.maximumCosigners) ""

    stakePolicy' = mkEnvelope' conf (stakePolicy gov.gtClassRef) ""
    stakeValidator' = mkEnvelope' conf (stakeValidator (agoraScripts conf gov) gov.gtClassRef) ""

    treasuryValidator' = mkEnvelope' conf (treasuryValidator authorityTokenSymbol) ""
    treasuryWithdrawalEffect' = mkEnvelope' conf (treasuryWithdrawalValidator authorityTokenSymbol) ""
