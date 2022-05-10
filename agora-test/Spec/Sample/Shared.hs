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

  -- ** Treasury
  treasuryOut,
  gatTn,
  gatCs,
  mockTrEffect,
  trCredential,
  wrongEffHash,
) where

import Agora.Effect.NoOp (noOpValidator)
import Agora.Governor (
  Governor (Governor),
  governorPolicy,
  governorValidator,
 )
import Agora.Proposal (
  Proposal (..),
  ProposalThresholds (..),
 )
import Agora.Proposal.Scripts (
  proposalPolicy,
  proposalValidator,
 )
import Agora.Stake (Stake (..))
import Agora.Stake.Scripts (stakePolicy, stakeValidator)
import Agora.Treasury (treasuryValidator)
import Agora.Utils (validatorHashToTokenName)
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
import Plutus.V1.Ledger.Contexts (
  TxOut (..),
 )
import Plutus.V1.Ledger.Scripts (Validator, ValidatorHash (..))
import Plutus.V1.Ledger.Value (TokenName, Value)
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
    { governorSTAssetClass = Value.assetClass govSymbol ""
    , stakeSTAssetClass = Value.assetClass stakeSymbol ""
    , maximumCosigners = 6
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
    , startVoting = Tagged 10
    }

------------------------------------------------------------------

treasuryOut :: TxOut
treasuryOut =
  TxOut
    { txOutAddress = Address trCredential Nothing
    , txOutValue = minAda
    , txOutDatumHash = Nothing
    }

{- | Arbitrary 'CurrencySymbol', representing the 'CurrencySymbol'
     of a valid governance authority token (GAT).
-}
gatCs :: CurrencySymbol
gatCs = "73475cb40a568e8da8a045ced110137e159f890ac4da883b6b17dc651b3a8049"

trValidator :: Validator
trValidator = mkValidator (treasuryValidator gatCs)

-- | `ScriptCredential` used for the dummy treasury validator.
trCredential :: Credential
trCredential = ScriptCredential $ validatorHash trValidator

-- | `TokenName` for GAT generated from address of `mockTrEffect`.
gatTn :: TokenName
gatTn = validatorHashToTokenName $ validatorHash mockTrEffect

-- | Mock treasury effect script, used for testing.
mockTrEffect :: Validator
mockTrEffect = mkValidator $ noOpValidator gatCs

{- | A SHA-256 hash which (in all certainty) should not match the
     hash of the dummy effect script.
-}
wrongEffHash :: ValidatorHash
wrongEffHash =
  ValidatorHash
    "a21bc4a1d95600f9fa0a00b97ed0fa49a152a72de76253cb706f90b4b40f837b"

------------------------------------------------------------------

minAda :: Value
minAda = Value.singleton "" "" 10_000_000
