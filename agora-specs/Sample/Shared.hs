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
  mkEffect,
  mkRedeemer,
  fromDiscrete,

  -- * Agora Scripts
  agoraScripts,

  -- * Components

  -- ** Stake
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
  gstUTXORef,

  -- ** Proposal
  proposalPolicySymbol,
  proposalValidatorHash,
  proposalValidatorAddress,
  proposalStartingTimeFromTimeRange,

  -- ** Authority
  authorityTokenSymbol,

  -- ** Treasury
  treasuryOut,
  gatTn,
  gatCs,
  mockTrEffect,
  mockTrEffectHash,
  trCredential,
  wrongEffHash,
) where

import Agora.Bootstrap qualified as Bootstrap
import Agora.Effect.NoOp (noOpValidator)
import Agora.Governor (Governor (Governor))
import Agora.Proposal (ProposalThresholds (..))
import Agora.Proposal.Time (
  MaxTimeRangeWidth (..),
  ProposalStartingTime (ProposalStartingTime),
  ProposalTimingConfig (..),
 )
import Agora.Scripts qualified as Scripts
import Agora.Treasury (treasuryValidator)
import Agora.Utils (
  CompiledEffect (CompiledEffect),
  CompiledMintingPolicy (getCompiledMintingPolicy),
  CompiledValidator (getCompiledValidator),
  validatorHashToTokenName,
 )
import Data.Coerce (coerce)
import Data.Default.Class (Default (..))
import Data.Tagged (Tagged (..))
import Plutarch (Config (..), TracingMode (DetTracing))
import Plutarch.Api.V2 (
  PValidator,
  mintingPolicySymbol,
  mkValidator,
  validatorHash,
 )
import Plutarch.SafeMoney (Discrete (Discrete))
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Contexts (TxOut (..))
import PlutusLedgerApi.V1.Scripts (Validator, ValidatorHash (..))
import PlutusLedgerApi.V1.Value (AssetClass, TokenName)
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

agoraScripts :: Scripts.AgoraScripts
agoraScripts = Bootstrap.agoraScripts deterministicTracingConfing governor

stakeSymbol :: CurrencySymbol
stakeSymbol = Scripts.stakeSTSymbol agoraScripts

stakeAssetClass :: AssetClass
stakeAssetClass = Scripts.stakeSTAssetClass agoraScripts

stakeValidatorHash :: ValidatorHash
stakeValidatorHash = Scripts.stakeValidatorHash agoraScripts

stakeAddress :: Address
stakeAddress = Address (ScriptCredential stakeValidatorHash) Nothing

gstUTXORef :: TxOutRef
gstUTXORef = TxOutRef "f28cd7145c24e66fd5bcd2796837aeb19a48a2656e7833c88c62a2d0450bd00d" 0

govPolicy :: MintingPolicy
govPolicy = agoraScripts.compiledGovernorPolicy.getCompiledMintingPolicy

govValidator :: Validator
govValidator = agoraScripts.compiledGovernorValidator.getCompiledValidator

govSymbol :: CurrencySymbol
govSymbol = mintingPolicySymbol govPolicy

govAssetClass :: AssetClass
govAssetClass = Scripts.governorSTAssetClass agoraScripts

govValidatorHash :: ValidatorHash
govValidatorHash = Scripts.governorValidatorHash agoraScripts

govValidatorAddress :: Address
govValidatorAddress = scriptHashAddress govValidatorHash

proposalPolicySymbol :: CurrencySymbol
proposalPolicySymbol = Scripts.proposalSTSymbol agoraScripts

-- | A sample 'PubKeyHash'.
signer :: PubKeyHash
signer = "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be7401214142019c"

-- | Another sample 'PubKeyHash'.
signer2 :: PubKeyHash
signer2 = "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be74012141420192"

proposalValidatorHash :: ValidatorHash
proposalValidatorHash = Scripts.proposalValidatoHash agoraScripts

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

authorityTokenSymbol :: CurrencySymbol
authorityTokenSymbol = Scripts.authorityTokenSymbol agoraScripts

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

mkEffect :: (PlutusTx.ToData datum) => ClosedTerm PValidator -> CompiledEffect datum
mkEffect v = CompiledEffect $ mkValidator deterministicTracingConfing v

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
gatCs = "73475cb40a568e8da8a045ced110137e159f890ac4da883b6b17dc651b3a8049"

trValidator :: Validator
trValidator = mkValidator def (treasuryValidator gatCs)

-- | `ScriptCredential` used for the dummy treasury validator.
trCredential :: Credential
trCredential = ScriptCredential $ validatorHash trValidator

-- | `TokenName` for GAT generated from address of `mockTrEffect`.
gatTn :: TokenName
gatTn = validatorHashToTokenName $ validatorHash mockTrEffect

-- | Mock treasury effect script, used for testing.
mockTrEffect :: Validator
mockTrEffect = mkValidator def $ noOpValidator gatCs

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
