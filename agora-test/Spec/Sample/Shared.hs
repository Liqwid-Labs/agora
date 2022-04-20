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

  -- * Components

  -- ** Stake
  stake,
  stakeSymbol,
  stakeValidatorHash,
  stakeAddress,

  -- ** Governor
  governor,
  govPolicy,
  govValidator,
  govSymbol,

  -- ** Proposal
  defaultProposalThresholds,
  proposal,
  proposalPolicySymbol,
  proposalValidatorHash,
  proposalValidatorAddress,
) where

import Agora.Governor (
  Governor (Governor),
  governorPolicy,
  governorValidator,
 )
import Agora.Proposal (
  Proposal (..),
  ProposalThresholds (..),
  proposalPolicy,
  proposalValidator,
 )
import Agora.Stake (Stake (..), stakePolicy, stakeValidator)
import Plutarch.Api.V1 (
  mintingPolicySymbol,
  mkMintingPolicy,
  mkValidator,
  validatorHash,
 )
import Plutarch.SafeMoney
import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Api (
  Address (Address),
  Credential (ScriptCredential),
  CurrencySymbol,
  MintingPolicy (..),
  PubKeyHash,
 )
import Plutus.V1.Ledger.Scripts (Validator, ValidatorHash)
import Plutus.V1.Ledger.Value qualified as Value

--------------------------------------------------------------------------------

stake :: Stake
stake =
  Stake
    { gtClassRef =
        Tagged $
          Value.assetClass
            "da8c30857834c6ae7203935b89278c532b3995245295456f993e1d24"
            "LQ"
    , proposalSTClass = Value.assetClass proposalPolicySymbol ""
    }

stakeSymbol :: CurrencySymbol
stakeSymbol = mintingPolicySymbol $ mkMintingPolicy $ stakePolicy stake.gtClassRef

stakeValidatorHash :: ValidatorHash
stakeValidatorHash = validatorHash $ mkValidator (stakeValidator stake)

stakeAddress :: Address
stakeAddress = Address (ScriptCredential stakeValidatorHash) Nothing

governor :: Governor
governor = Governor

govPolicy :: MintingPolicy
govPolicy = mkMintingPolicy (governorPolicy governor)

govValidator :: Validator
govValidator = mkValidator (governorValidator governor)

govSymbol :: CurrencySymbol
govSymbol = mintingPolicySymbol govPolicy

proposal :: Proposal
proposal =
  Proposal
    { governorSTAssetClass =
        -- TODO: if we had a governor here
        Value.assetClass govSymbol ""
    , stakeSTAssetClass =
        Value.assetClass stakeSymbol ""
    }

proposalPolicySymbol :: CurrencySymbol
proposalPolicySymbol = mintingPolicySymbol $ mkMintingPolicy (proposalPolicy proposal)

-- | A sample 'PubKeyHash'.
signer :: PubKeyHash
signer = "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be7401214142019c"

-- | Another sample 'PubKeyHash'.
signer2 :: PubKeyHash
signer2 = "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be74012141420192"

proposalValidatorHash :: ValidatorHash
proposalValidatorHash = validatorHash (mkValidator $ proposalValidator proposal)

proposalValidatorAddress :: Address
proposalValidatorAddress = scriptHashAddress proposalValidatorHash

defaultProposalThresholds :: ProposalThresholds
defaultProposalThresholds =
  ProposalThresholds
    { countVoting = Tagged 1000
    , create = Tagged 1
    , vote = Tagged 10
    }
