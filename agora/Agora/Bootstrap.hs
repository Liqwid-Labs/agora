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
import Agora.Scripts (AgoraScripts (AgoraScripts))
import Agora.Scripts qualified as Scripts
import Agora.Stake.Scripts (stakePolicy, stakeValidator)
import Agora.Treasury (treasuryValidator)
import Agora.Utils (
  CompiledMintingPolicy (CompiledMintingPolicy),
  CompiledValidator (CompiledValidator),
 )
import Plutarch (Config)
import Plutarch.Api.V2 (
  mintingPolicySymbol,
  mkMintingPolicy,
  mkValidator,
 )
import PlutusLedgerApi.V1.Value (AssetClass (AssetClass))

{- | Parameterize and precompiled core scripts, given the
     'Agora.Governor.Governor' parameters and plutarch configurations.

     @since 0.2.0
-}
agoraScripts :: Config -> Governor -> AgoraScripts
agoraScripts conf gov = scripts
  where
    mkMintingPolicy' = mkMintingPolicy conf
    mkValidator' = mkValidator conf

    compiledGovernorPolicy = mkMintingPolicy' $ governorPolicy (getField @"gstOutRef" gov)
    compiledGovernorValidator = mkValidator' $ governorValidator scripts
    governorSymbol = mintingPolicySymbol compiledGovernorPolicy
    governorAssetClass = AssetClass (governorSymbol, "")

    authority = AuthorityToken governorAssetClass
    compiledAuthorityPolicy = mkMintingPolicy' $ authorityTokenPolicy authority
    authorityTokenSymbol = mintingPolicySymbol compiledAuthorityPolicy

    compiledProposalPolicy = mkMintingPolicy' $ proposalPolicy governorAssetClass
    compiledProposalValidator = mkValidator' $ proposalValidator scripts (getField @"maximumCosigners" gov)

    compiledStakePolicy = mkMintingPolicy' $ stakePolicy (getField @"gtClassRef" gov)
    compiledStakeValidator = mkValidator' $ stakeValidator scripts (getField @"gtClassRef" gov)

    compiledTreasuryValidator = mkValidator' $ treasuryValidator authorityTokenSymbol

    compiledTreasuryWithdrawalEffect = mkValidator' $ treasuryWithdrawalValidator authorityTokenSymbol

    scripts =
      AgoraScripts
        { Scripts.compiledGovernorPolicy = CompiledMintingPolicy compiledGovernorPolicy
        , Scripts.compiledGovernorValidator = CompiledValidator compiledGovernorValidator
        , Scripts.compiledStakePolicy = CompiledMintingPolicy compiledStakePolicy
        , Scripts.compiledStakeValidator = CompiledValidator compiledStakeValidator
        , Scripts.compiledProposalPolicy = CompiledMintingPolicy compiledProposalPolicy
        , Scripts.compiledProposalValidator = CompiledValidator compiledProposalValidator
        , Scripts.compiledTreasuryValidator = CompiledValidator compiledTreasuryValidator
        , Scripts.compiledAuthorityTokenPolicy = CompiledMintingPolicy compiledAuthorityPolicy
        , Scripts.compiledTreasuryWithdrawalEffect = CompiledValidator compiledTreasuryWithdrawalEffect
        }
