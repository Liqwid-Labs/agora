{- | Module     : Agora.Scripts
     Maintainer : connor@mlabs.city
     Description: Precompiled core scripts and utilities

     Precompiled core scripts and utilities
-}
module Agora.Scripts (
  AgoraScripts (..),
  governorSTSymbol,
  governorSTAssetClass,
  governorValidatorHash,
  proposalSTSymbol,
  proposalSTAssetClass,
  proposalValidatoHash,
  stakeSTSymbol,
  stakeSTAssetClass,
  stakeValidatorHash,
  authorityTokenSymbol,
  treasuryValidatorHash,
) where

import Agora.Governor (GovernorDatum, GovernorRedeemer)
import Agora.Proposal (ProposalDatum, ProposalRedeemer)
import Agora.Stake (StakeDatum, StakeRedeemer)
import Agora.Treasury (TreasuryRedeemer)
import Agora.Utils (
  CompiledMintingPolicy (getCompiledMintingPolicy),
  CompiledValidator (getCompiledValidator),
  validatorHashToTokenName,
 )
import Plutarch.Api.V2 (mintingPolicySymbol, validatorHash)
import PlutusLedgerApi.V1.Value (AssetClass (AssetClass))
import PlutusLedgerApi.V2 (CurrencySymbol, ValidatorHash)

{- | Precompiled core scripts.

     Including:

    - Governor policy
    - Governor validator
    - Proposal policy
    - Proposal validator
    - Stake policy
    - Stake validator
    - Treasury validator
    - Authority token policy

     @since 0.2.0
-}
data AgoraScripts = AgoraScripts
  { compiledGovernorPolicy :: CompiledMintingPolicy ()
  , compiledGovernorValidator :: CompiledValidator GovernorDatum GovernorRedeemer
  , compiledStakePolicy :: CompiledMintingPolicy ()
  , compiledStakeValidator :: CompiledValidator StakeDatum StakeRedeemer
  , compiledProposalPolicy :: CompiledMintingPolicy ()
  , compiledProposalValidator :: CompiledValidator ProposalDatum ProposalRedeemer
  , compiledTreasuryValidator :: CompiledValidator () TreasuryRedeemer
  , compiledAuthorityTokenPolicy :: CompiledMintingPolicy ()
  }

{- | Get the currency symbol of the governor state token.

     @since 0.2.0
-}
governorSTSymbol :: AgoraScripts -> CurrencySymbol
governorSTSymbol = mintingPolicySymbol . getCompiledMintingPolicy . compiledGovernorPolicy

{- | Get the asset class of the governor state token.

     @since 0.2.0
-}
governorSTAssetClass :: AgoraScripts -> AssetClass
governorSTAssetClass as = AssetClass (governorSTSymbol as, "")

{- | Get the script hash of the governor validator.

     @since 0.2.0
-}
governorValidatorHash :: AgoraScripts -> ValidatorHash
governorValidatorHash = validatorHash . getCompiledValidator . compiledGovernorValidator

{- | Get the currency symbol of the propsoal state token.

     @since 0.2.0
-}
proposalSTSymbol :: AgoraScripts -> CurrencySymbol
proposalSTSymbol as = mintingPolicySymbol $ getCompiledMintingPolicy as.compiledProposalPolicy

{- | Get the asset class of the governor state token.

     @since 0.2.0
-}
proposalSTAssetClass :: AgoraScripts -> AssetClass
proposalSTAssetClass as = AssetClass (proposalSTSymbol as, "")

{- | Get the script hash of the proposal validator.

     @since 0.2.0
-}
proposalValidatoHash :: AgoraScripts -> ValidatorHash
proposalValidatoHash = validatorHash . getCompiledValidator . compiledProposalValidator

{- | Get the script hash of the governor validator.

     @since 0.2.0
-}
stakeSTSymbol :: AgoraScripts -> CurrencySymbol
stakeSTSymbol = mintingPolicySymbol . getCompiledMintingPolicy . compiledStakePolicy

{- | Get the asset class of the stake state token.

     Note that this token is tagged with the hash of the stake validator.
      See 'Agora.Stake.Script.stakePolicy'.

     @since 0.2.0
-}
stakeSTAssetClass :: AgoraScripts -> AssetClass
stakeSTAssetClass as =
  let tn = validatorHashToTokenName $ stakeValidatorHash as
   in AssetClass (stakeSTSymbol as, tn)

{- | Get the script hash of the stake validator.

     @since 0.2.0
-}
stakeValidatorHash :: AgoraScripts -> ValidatorHash
stakeValidatorHash = validatorHash . getCompiledValidator . compiledStakeValidator

{- | Get the currency symbol of the authority token.

     @since 0.2.0
-}
authorityTokenSymbol :: AgoraScripts -> CurrencySymbol
authorityTokenSymbol = mintingPolicySymbol . getCompiledMintingPolicy . compiledAuthorityTokenPolicy

{- | Get the script hash of the treasury validator.

     @since 0.2.0
-}
treasuryValidatorHash :: AgoraScripts -> ValidatorHash
treasuryValidatorHash = validatorHash . getCompiledValidator . compiledTreasuryValidator
