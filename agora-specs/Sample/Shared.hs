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
  deterministicTracingConfig,
  mkRedeemer,

  -- * Agora Scripts
  agoraScripts,

  -- * Components

  -- ** Stake
  stakeAssetClass,
  stakePolicy,
  stakeValidator,
  stakeScriptHash,
  stakeAddress,
  stakeSymbol,

  -- ** Governor
  governor,
  governorPolicy,
  governorValidator,
  governorSymbol,
  governorAssetClass,
  governorValidatorAddress,
  governorScriptHash,
  gstUTXORef,

  -- ** Proposal
  proposalPolicy,
  proposalPolicySymbol,
  proposalValidator,
  proposalScriptHash,
  proposalValidatorAddress,
  proposalStartingTimeFromTimeRange,
  proposalAssetClass,

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
  trScriptHash,
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
import Agora.SafeMoney (GovernorSTTag, ProposalSTTag, StakeSTTag)
import Data.Default.Class (Default (..))
import Data.Map (Map, (!))
import Data.Tagged (Tagged (..))
import Data.Text (Text)
import Optics (view)
import Plutarch (Config (..), Script, TracingMode (DetTracing))
import Plutarch.Api.V2 (scriptHash)
import Plutarch.Extra.AssetClass (AssetClass (AssetClass))
import Plutarch.Extra.ScriptContext (scriptHashToTokenName)
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (TokenName, Value)
import PlutusLedgerApi.V1.Value qualified as Value (
  singleton,
 )
import PlutusLedgerApi.V2 (
  Address (Address),
  Credential (ScriptCredential),
  CurrencySymbol (CurrencySymbol),
  Extended (..),
  Interval (..),
  LowerBound (..),
  OutputDatum (NoOutputDatum),
  POSIXTimeRange,
  PubKeyHash,
  Redeemer (..),
  ScriptHash (getScriptHash),
  ToData (toBuiltinData),
  TxOut (
    TxOut,
    txOutAddress,
    txOutDatum,
    txOutReferenceScript,
    txOutValue
  ),
  TxOutRef (TxOutRef),
  UpperBound (..),
 )
import PlutusTx qualified
import ScriptExport.ScriptInfo (runLinker)

-- Plutarch compiler configauration.
-- TODO: add the ability to change this value. Maybe wrap everything in a
--        Reader monad?
deterministicTracingConfig :: Config
deterministicTracingConfig = Config DetTracing

governor :: Governor
governor = Governor oref gt mc
  where
    oref = gstUTXORef
    gt =
      Tagged $
        AssetClass
          "da8c30857834c6ae7203935b89278c532b3995245295456f993e1d24"
          "LQ"
    mc = 20

agoraScripts :: Map Text Script
agoraScripts =
  either
    (error . show)
    (fmap (view #script) . view #scripts)
    ( runLinker
        linker
        (Bootstrap.agoraScripts deterministicTracingConfig)
        governor
    )

stakePolicy :: Script
stakePolicy = agoraScripts ! "agora:stakePolicy"

stakeSymbol :: CurrencySymbol
stakeSymbol = CurrencySymbol . getScriptHash $ scriptHash stakePolicy

stakeAssetClass :: Tagged StakeSTTag AssetClass
stakeAssetClass = Tagged $ AssetClass stakeSymbol (scriptHashToTokenName stakeScriptHash)

stakeValidator :: Script
stakeValidator = agoraScripts ! "agora:stakeValidator"

stakeScriptHash :: ScriptHash
stakeScriptHash = scriptHash stakeValidator

stakeAddress :: Address
stakeAddress = Address (ScriptCredential stakeScriptHash) Nothing

gstUTXORef :: TxOutRef
gstUTXORef = TxOutRef "f28cd7145c24e66fd5bcd2796837aeb19a48a2656e7833c88c62a2d0450bd00d" 0

governorPolicy :: Script
governorPolicy = agoraScripts ! "agora:governorPolicy"

governorValidator :: Script
governorValidator = agoraScripts ! "agora:governorValidator"

governorSymbol :: CurrencySymbol
governorSymbol = CurrencySymbol . getScriptHash $ scriptHash governorPolicy

governorAssetClass :: Tagged GovernorSTTag AssetClass
governorAssetClass = Tagged $ AssetClass governorSymbol ""

governorScriptHash :: ScriptHash
governorScriptHash = scriptHash governorValidator

governorValidatorAddress :: Address
governorValidatorAddress = scriptHashAddress governorScriptHash

proposalPolicy :: Script
proposalPolicy = agoraScripts ! "agora:proposalPolicy"

proposalPolicySymbol :: CurrencySymbol
proposalPolicySymbol = CurrencySymbol . getScriptHash $ scriptHash proposalPolicy

proposalAssetClass :: Tagged ProposalSTTag AssetClass
proposalAssetClass = Tagged $ AssetClass proposalPolicySymbol ""

-- | A sample 'PubKeyHash'.
signer :: PubKeyHash
signer = "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be7401214142019c"

-- | Another sample 'PubKeyHash'.
signer2 :: PubKeyHash
signer2 = "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be74012141420192"

proposalValidator :: Script
proposalValidator = agoraScripts ! "agora:proposalValidator"

proposalScriptHash :: ScriptHash
proposalScriptHash = scriptHash proposalValidator

proposalValidatorAddress :: Address
proposalValidatorAddress = scriptHashAddress proposalScriptHash

{- | Default value of 'Agora.Proposal.ProposalThresholds'.
     For testing purpose only.
-}
instance Default ProposalThresholds where
  def =
    ProposalThresholds
      { execute = Tagged 1000
      , create = Tagged 1
      , toVoting = Tagged 100
      , vote = Tagged 100
      , cosign = Tagged 100
      }

authorityTokenPolicy :: Script
authorityTokenPolicy = agoraScripts ! "agora:authorityTokenPolicy"

authorityTokenSymbol :: CurrencySymbol
authorityTokenSymbol = CurrencySymbol . getScriptHash $ scriptHash authorityTokenPolicy

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
      , minStakeVotingTime = 100
      , votingTimeRangeMaxWidth = 1000000
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

------------------------------------------------------------------

treasuryOut :: TxOut
treasuryOut =
  TxOut
    { txOutAddress = Address trCredential Nothing
    , txOutValue = minAda
    , txOutDatum = NoOutputDatum
    , txOutReferenceScript = Nothing
    }

{- | Arbitrary 'CurrencySymbol', representing the 'CurrencySymbol'
     of a valid governance authority token (GAT).
-}
gatCs :: CurrencySymbol
gatCs = authorityTokenSymbol

trValidator :: Script
trValidator = agoraScripts ! "agora:treasuryValidator"

trScriptHash :: ScriptHash
trScriptHash = scriptHash trValidator

-- | `ScriptCredential` used for the dummy treasury validator.
trCredential :: Credential
trCredential = ScriptCredential trScriptHash

-- | `TokenName` for GAT generated from address of `mockTrEffect`.
gatTn :: TokenName
gatTn = scriptHashToTokenName $ scriptHash mockTrEffect

-- | Mock treasury effect script, used for testing.
mockTrEffect :: Script
mockTrEffect = agoraScripts ! "agora:noOpValidator"

-- | Mock treasury effect validator hash
mockTrEffectHash :: ScriptHash
mockTrEffectHash = scriptHash mockTrEffect

{- | A SHA-256 hash which (in all certainty) should not match the
     hash of the dummy effect script.
-}
wrongEffHash :: ScriptHash
wrongEffHash = "a21bc4a1d95600f9fa0a00b97ed0fa49a152a72de76253cb706f90b4b40f837b"

------------------------------------------------------------------

minAda :: Value
minAda = Value.singleton "" "" 10_000_000
