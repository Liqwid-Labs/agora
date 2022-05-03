{- |
Module     : Spec.Sample.Shared
Maintainer : emi@haskell.fyi
Description: Shared useful values for creating Samples for testing.

Shared useful values for creating Samples for testing.
-}
module Spec.Sample.Shared (
  -- * Misc
  signer,
  signer2,
  minAda,
  withMinAda,

  -- * Components

  -- ** Stake
  stake,
  stakeAssetClass,
  stakeValidatorHash,
  stakeAddress,
  stakeSymbol,

  -- ** Governor
  governor,
  govPolicy,
  govValidator,
  govSymbol,
  govAssetClass,
  govValidatorAddress,
  govValidatorHash,

  -- ** Proposal
  defaultProposalThresholds,
  proposal,
  proposalPolicySymbol,
  proposalValidatorHash,
  proposalValidatorAddress,

  -- ** Authority
  authorityToken,
  authorityTokenSymbol,
) where

import Agora.AuthorityToken
import Agora.Governor (
  Governor (Governor),
 )
import Agora.Governor.Scripts (
  authorityTokenFromGovernor,
  authorityTokenSymbolFromGovernor,
  governorPolicy,
  governorSTAssetClassFromGovernor,
  governorValidator,
  governorValidatorHash,
  proposalFromGovernor,
  proposalSTSymbolFromGovernor,
  proposalValidatorHashFromGovernor,
  stakeFromGovernor,
  stakeSTAssetClassFromGovernor,
  stakeSTSymbolFromGovernor,
  stakeValidatorHashFromGovernor,
 )
import Agora.Proposal (
  Proposal (..),
  ProposalThresholds (..),
 )
import Agora.Stake (Stake (..))
import Plutarch.Api.V1 (
  mintingPolicySymbol,
  mkMintingPolicy,
  mkValidator,
 )
import Plutarch.SafeMoney
import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Api (
  Address (Address),
  Credential (ScriptCredential),
  CurrencySymbol,
  MintingPolicy (..),
  PubKeyHash,
  TxOutRef (TxOutRef),
  Value,
 )
import Plutus.V1.Ledger.Scripts (Validator, ValidatorHash)
import Plutus.V1.Ledger.Value (AssetClass)
import Plutus.V1.Ledger.Value qualified as Value

--------------------------------------------------------------------------------

stake :: Stake
stake = stakeFromGovernor governor

stakeSymbol :: CurrencySymbol
stakeSymbol = stakeSTSymbolFromGovernor governor

stakeAssetClass :: AssetClass
stakeAssetClass = stakeSTAssetClassFromGovernor governor

stakeValidatorHash :: ValidatorHash
stakeValidatorHash = stakeValidatorHashFromGovernor governor

stakeAddress :: Address
stakeAddress = Address (ScriptCredential stakeValidatorHash) Nothing

governor :: Governor
governor = Governor oref gt mc
  where
    oref =
      TxOutRef "f28cd7145c24e66fd5bcd2796837aeb19a48a2656e7833c88c62a2d0450bd00d" 0
    gt =
      Tagged $
        Value.assetClass
          "da8c30857834c6ae7203935b89278c532b3995245295456f993e1d24"
          "LQ"
    mc = 6

govPolicy :: MintingPolicy
govPolicy = mkMintingPolicy (governorPolicy governor)

govValidator :: Validator
govValidator = mkValidator (governorValidator governor)

govSymbol :: CurrencySymbol
govSymbol = mintingPolicySymbol govPolicy

govAssetClass :: AssetClass
govAssetClass = governorSTAssetClassFromGovernor governor

govValidatorHash :: ValidatorHash
govValidatorHash = governorValidatorHash governor

govValidatorAddress :: Address
govValidatorAddress = scriptHashAddress govValidatorHash

proposal :: Proposal
proposal = proposalFromGovernor governor

proposalPolicySymbol :: CurrencySymbol
proposalPolicySymbol = proposalSTSymbolFromGovernor governor

-- | A sample 'PubKeyHash'.
signer :: PubKeyHash
signer = "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be7401214142019c"

-- | Another sample 'PubKeyHash'.
signer2 :: PubKeyHash
signer2 = "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be74012141420192"

proposalValidatorHash :: ValidatorHash
proposalValidatorHash = proposalValidatorHashFromGovernor governor

proposalValidatorAddress :: Address
proposalValidatorAddress = scriptHashAddress proposalValidatorHash

defaultProposalThresholds :: ProposalThresholds
defaultProposalThresholds =
  ProposalThresholds
    { countVoting = Tagged 1000
    , create = Tagged 1
    , startVoting = Tagged 10
    }

minAda :: Value
minAda = Value.singleton "" "" 10_000_000

withMinAda :: Value -> Value
withMinAda v = v <> minAda

authorityToken :: AuthorityToken
authorityToken = authorityTokenFromGovernor governor

authorityTokenSymbol :: CurrencySymbol
authorityTokenSymbol = authorityTokenSymbolFromGovernor governor
