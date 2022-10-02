{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module     : Sample.Shared
Maintainer : emi@haskell.fyi
Description: Shared useful values for creating Samples for testing.

Shared useful values for creating Samples for testing.
-}
module Sample.Shared (
  -- * Misc
  signer,
  signer2,
  minAda,
  deterministicTracingConfing,
  mkRedeemer,
  fromDiscrete,

  -- * Agora Scripts
  agoraScripts,

  -- * Components

  -- ** Stake
  stakeAssetClass,
  stakePolicy,
  stakeValidator,
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
  gstUTXORef,

  -- ** Proposal
  proposalPolicy,
  proposalPolicySymbol,
  proposalValidator,
  proposalValidatorHash,
  proposalValidatorAddress,
  proposalStartingTimeFromTimeRange,

  -- ** Authority
  authorityTokenPolicy,
  authorityTokenSymbol,

  -- ** Treasury
  treasuryOut,
  gatTn,
  gatCs,
  mockTrEffect,
  mockTrEffectHash,
  trValidator,
  trCredential,
  wrongEffHash,
) where

import Agora.Bootstrap qualified as Bootstrap
import Agora.Governor (Governor (Governor))
import Agora.Linker (linker)
import Agora.Proposal (ProposalThresholds (..))
import Agora.Proposal.Time (
  MaxTimeRangeWidth (..),
  ProposalStartingTime (ProposalStartingTime),
  ProposalTimingConfig (..),
 )
import Agora.Utils (
  validatorHashToTokenName,
 )
import Data.Coerce (coerce)
import Data.Default.Class (Default (..))
import Data.Map
import Data.Tagged (Tagged (..))
import Data.Text
import Plutarch (Config (..), TracingMode (DetTracing))
import Plutarch.Api.V2 (
  mintingPolicySymbol,
  validatorHash,
 )
import Plutarch.SafeMoney (Discrete (Discrete))
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Contexts (TxOut (..))
import PlutusLedgerApi.V1.Scripts (Script, Validator (Validator), ValidatorHash (..))
import PlutusLedgerApi.V1.Value (AssetClass (AssetClass), TokenName)
import PlutusLedgerApi.V1.Value qualified as Value (
  assetClass,
  singleton,
 )
import PlutusLedgerApi.V2 (
  Address (Address),
  Credential (ScriptCredential),
  CurrencySymbol,
  Extended (..),
  Interval (..),
  LowerBound (..),
  MintingPolicy (..),
  POSIXTimeRange,
  PubKeyHash,
  Redeemer (..),
  ToData (toBuiltinData),
  TxOutRef (TxOutRef),
  UpperBound (..),
  Value,
 )
import PlutusTx qualified

-- Plutarch compiler configauration.
-- TODO: add the ability to change this value. Maybe wrap everything in a
--        Reader monad?
deterministicTracingConfing :: Config
deterministicTracingConfing = Config DetTracing

governor :: Governor
governor = Governor oref gt mc
  where
    oref = gstUTXORef
    gt =
      Tagged $
        Value.assetClass
          "da8c30857834c6ae7203935b89278c532b3995245295456f993e1d24"
          "LQ"
    mc = 20

agoraScripts :: Map Text Script
agoraScripts = linker governor $ Bootstrap.agoraScripts deterministicTracingConfing

stakePolicy :: MintingPolicy
stakePolicy = MintingPolicy $ agoraScripts ! "agora:stakePolicy"

stakeSymbol :: CurrencySymbol
stakeSymbol = mintingPolicySymbol stakePolicy

stakeAssetClass :: AssetClass
stakeAssetClass = AssetClass (stakeSymbol, validatorHashToTokenName stakeValidatorHash)

stakeValidator :: Validator
stakeValidator = Validator $ agoraScripts ! "agora:stakeValidator"

stakeValidatorHash :: ValidatorHash
stakeValidatorHash = validatorHash stakeValidator

stakeAddress :: Address
stakeAddress = Address (ScriptCredential stakeValidatorHash) Nothing

gstUTXORef :: TxOutRef
gstUTXORef = TxOutRef "f28cd7145c24e66fd5bcd2796837aeb19a48a2656e7833c88c62a2d0450bd00d" 0

govPolicy :: MintingPolicy
govPolicy = MintingPolicy $ agoraScripts ! "agora:governorPolicy"

govValidator :: Validator
govValidator = Validator $ agoraScripts ! "agora:governorValidator"

govSymbol :: CurrencySymbol
govSymbol = mintingPolicySymbol govPolicy

govAssetClass :: AssetClass
govAssetClass = AssetClass (govSymbol, "")

govValidatorHash :: ValidatorHash
govValidatorHash = validatorHash govValidator

govValidatorAddress :: Address
govValidatorAddress = scriptHashAddress govValidatorHash

proposalPolicy :: MintingPolicy
proposalPolicy = MintingPolicy $ agoraScripts ! "agora:proposalPolicy"

proposalPolicySymbol :: CurrencySymbol
proposalPolicySymbol = mintingPolicySymbol proposalPolicy

-- | A sample 'PubKeyHash'.
signer :: PubKeyHash
signer = "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be7401214142019c"

-- | Another sample 'PubKeyHash'.
signer2 :: PubKeyHash
signer2 = "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be74012141420192"

proposalValidator :: Validator
proposalValidator = Validator $ agoraScripts ! "agora:proposalValidator"

proposalValidatorHash :: ValidatorHash
proposalValidatorHash = validatorHash proposalValidator

proposalValidatorAddress :: Address
proposalValidatorAddress = scriptHashAddress proposalValidatorHash

{- | Default value of 'Agora.Proposal.ProposalThresholds'.
     For testing purpose only.
-}
instance Default ProposalThresholds where
  def =
    ProposalThresholds
      { execute = Tagged 1000
      , create = Tagged 1
      , vote = Tagged 100
      }

authorityTokenPolicy :: MintingPolicy
authorityTokenPolicy = MintingPolicy $ agoraScripts ! "agora:authorityTokenPolicy"

authorityTokenSymbol :: CurrencySymbol
authorityTokenSymbol = mintingPolicySymbol authorityTokenPolicy

{- | Default value of 'Agora.Governor.GovernorDatum.proposalTimings'.
     For testing purpose only.
-}
instance Default ProposalTimingConfig where
  def =
    ProposalTimingConfig
      { draftTime = 50
      , votingTime = 1000
      , lockingTime = 2000
      , executingTime = 3000
      }

{- | Default value of 'Agora.Governor.GovernorDatum.createProposalTimeRangeMaxWidth'.
     For testing purpose only.
-}
instance Default MaxTimeRangeWidth where
  def = MaxTimeRangeWidth 10

{- | Get the starting time of a proposal, given a closed finite time range.
     Tightness of the time range is not checked. See 'Agora.Proposal.Time.createProposalStartingTime'.
-}
proposalStartingTimeFromTimeRange :: POSIXTimeRange -> ProposalStartingTime
proposalStartingTimeFromTimeRange
  (Interval (LowerBound (Finite l) True) (UpperBound (Finite u) True)) =
    ProposalStartingTime $ (l + u) `div` 2
proposalStartingTimeFromTimeRange _ = error "Given time range should be finite and closed"

mkRedeemer :: forall redeemer. PlutusTx.ToData redeemer => redeemer -> Redeemer
mkRedeemer = Redeemer . toBuiltinData

fromDiscrete :: forall tag. Discrete tag -> Integer
fromDiscrete = coerce

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
gatCs = authorityTokenSymbol

trValidator :: Validator
trValidator = Validator $ agoraScripts ! "agora:treasuryValidator"

-- | `ScriptCredential` used for the dummy treasury validator.
trCredential :: Credential
trCredential = ScriptCredential $ validatorHash trValidator

-- | `TokenName` for GAT generated from address of `mockTrEffect`.
gatTn :: TokenName
gatTn = validatorHashToTokenName $ validatorHash mockTrEffect

-- | Mock treasury effect script, used for testing.
mockTrEffect :: Validator
mockTrEffect = Validator $ agoraScripts ! "agora:noOpValidator"

-- | Mock treasury effect validator hash
mockTrEffectHash :: ValidatorHash
mockTrEffectHash = validatorHash mockTrEffect

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
