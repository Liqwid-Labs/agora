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
  hashEnvelope,
  validatorHashEnvelope,
  policySymbolEnvelope,
) where

import Agora.Utils (validatorHashToTokenName)
import qualified PlutusLedgerApi.V1.Scripts as Plutus
import qualified PlutusLedgerApi.V1.Value as Plutus
import Plutarch.Api.V2 (scriptHash)
import PlutusLedgerApi.V1.Value (AssetClass (AssetClass))
import PlutusLedgerApi.V2 (CurrencySymbol, ValidatorHash)
import Data.Coerce(coerce)
import Data.Aeson qualified as Aeson
import GHC.Generics qualified as GHC

import Ply (
  TypedScriptEnvelope(TypedScriptEnvelope),
  ScriptRole(ValidatorRole, MintingPolicyRole)
 )

hashEnvelope :: TypedScriptEnvelope -> Plutus.ScriptHash
hashEnvelope (TypedScriptEnvelope _ _ _ _ script) = scriptHash script

validatorHashEnvelope :: TypedScriptEnvelope -> Plutus.ValidatorHash
validatorHashEnvelope (TypedScriptEnvelope _ ValidatorRole _ _ script) =
  coerce $ scriptHash script
validatorHashEnvelope _ = error "Expected ValidatorRole"

policySymbolEnvelope :: TypedScriptEnvelope -> Plutus.CurrencySymbol
policySymbolEnvelope (TypedScriptEnvelope _ MintingPolicyRole _ _ script) =
  coerce $ scriptHash script
policySymbolEnvelope _ = error "Expected MintingPolicyRole"

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
  { compiledGovernorPolicy :: TypedScriptEnvelope
  , compiledGovernorValidator :: TypedScriptEnvelope
  , compiledStakePolicy :: TypedScriptEnvelope
  , compiledStakeValidator :: TypedScriptEnvelope
  , compiledProposalPolicy :: TypedScriptEnvelope
  , compiledProposalValidator :: TypedScriptEnvelope
  , compiledTreasuryValidator :: TypedScriptEnvelope
  , compiledAuthorityTokenPolicy :: TypedScriptEnvelope
  , compiledTreasuryWithdrawalEffect :: TypedScriptEnvelope
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


{- | Get the currency symbol of the governor state token.

     @since 0.2.0
-}
governorSTSymbol :: AgoraScripts -> CurrencySymbol
governorSTSymbol = policySymbolEnvelope . (.compiledGovernorPolicy)

{- | Get the asset class of the governor state token.

     @since 0.2.0
-}
governorSTAssetClass :: AgoraScripts -> AssetClass
governorSTAssetClass as = AssetClass (governorSTSymbol as, "")

{- | Get the script hash of the governor validator.

     @since 0.2.0
-}
governorValidatorHash :: AgoraScripts -> ValidatorHash
governorValidatorHash = validatorHashEnvelope . (.compiledGovernorValidator)

{- | Get the currency symbol of the propsoal state token.

     @since 0.2.0
-}
proposalSTSymbol :: AgoraScripts -> CurrencySymbol
proposalSTSymbol = policySymbolEnvelope . (.compiledProposalPolicy)

{- | Get the asset class of the governor state token.

     @since 0.2.0
-}
proposalSTAssetClass :: AgoraScripts -> AssetClass
proposalSTAssetClass as = AssetClass (proposalSTSymbol as, "")

{- | Get the script hash of the proposal validator.

     @since 0.2.0
-}
proposalValidatoHash :: AgoraScripts -> ValidatorHash
proposalValidatoHash = validatorHashEnvelope . (.compiledProposalValidator)

{- | Get the script hash of the governor validator.

     @since 0.2.0
-}
stakeSTSymbol :: AgoraScripts -> CurrencySymbol
stakeSTSymbol = policySymbolEnvelope . (.compiledStakePolicy)

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
stakeValidatorHash = validatorHashEnvelope . (.compiledStakeValidator)

{- | Get the currency symbol of the authority token.

     @since 0.2.0
-}
authorityTokenSymbol :: AgoraScripts -> CurrencySymbol
authorityTokenSymbol = policySymbolEnvelope . (.compiledAuthorityTokenPolicy)

{- | Get the script hash of the treasury validator.

     @since 0.2.0
-}
treasuryValidatorHash :: AgoraScripts -> ValidatorHash
treasuryValidatorHash = validatorHashEnvelope . (.compiledTreasuryValidator)
