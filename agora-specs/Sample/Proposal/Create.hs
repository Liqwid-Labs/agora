module Sample.Proposal.Create (
  Parameters (..),
  mkTestTree,
  totallyValidParameters,
  invalidOutputGovernorDatumParameters,
  useStakeOwnBySomeoneElseParameters,
  invalidOutputStakeParameters,
  addInvalidLocksParameters,
  exceedMaximumProposalsParameters,
  timeRangeNotTightParameters,
  timeRangeNotClosedParameters,
  invalidProposalStatusParameters,
) where

import Agora.Governor (
  GovernorDatum (..),
  GovernorRedeemer (CreateProposal),
 )
import Agora.Governor.Scripts (governorValidator)
import Agora.Proposal (
  Proposal (governorSTAssetClass),
  ProposalDatum (..),
  ProposalId (ProposalId),
  ProposalStatus (..),
  ResultTag (ResultTag),
  emptyVotesFor,
 )
import Agora.Proposal.Scripts (proposalPolicy)
import Agora.Proposal.Time (MaxTimeRangeWidth (MaxTimeRangeWidth), ProposalStartingTime (..))
import Agora.Stake (
  ProposalLock (..),
  Stake (gtClassRef),
  StakeDatum (..),
  StakeRedeemer (PermitVote),
 )
import Agora.Stake.Scripts (stakeValidator)
import Data.Coerce (coerce)
import Data.Default (Default (def))
import Data.Tagged (Tagged, untag)
import Plutarch.Context (
  BaseBuilder,
  buildTxInfoUnsafe,
  input,
  mint,
  output,
  script,
  signedWith,
  timeRange,
  txId,
  withDatum,
  withOutRef,
  withValue,
 )
import PlutusLedgerApi.V1 (
  DatumHash,
  POSIXTime (POSIXTime),
  POSIXTimeRange,
  PubKeyHash,
  ScriptContext (ScriptContext),
  ScriptPurpose (Minting, Spending),
  TxInfo,
  TxOutRef (TxOutRef),
  ValidatorHash,
  always,
 )
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusTx.AssocMap qualified as AssocMap
import Sample.Proposal.Shared (stakeTxRef)
import Sample.Shared (
  govValidatorHash,
  minAda,
  proposal,
  proposalPolicySymbol,
  proposalStartingTimeFromTimeRange,
  proposalValidatorHash,
  signer,
  signer2,
  stake,
  stakeAssetClass,
  stakeValidatorHash,
 )
import Sample.Shared qualified as Shared
import Test.Specification (SpecificationTree, group, testPolicy, testValidator)
import Test.Util (closedBoundedInterval, sortValue)

-- | Parameters for creating a proposal.
data Parameters = Parameters
  { advanceNextProposalId :: Bool
  -- ^ Whether to advance 'GovernorDatum.nextProposalId'.
  , createdMoreThanMaximumProposals :: Bool
  -- ^ Try creating more than maximum amount of proposals.
  , stakeOwnerSignsTheTransaction :: Bool
  -- ^ Should the stake owner sign the transaction?
  , invalidNewLocks :: Bool
  -- ^ Place invalid new locks on the output stake.
  , alterOutputStakeOwner :: Bool
  -- ^ Whether to change the 'owner' field of the output stake datum.
  , timeRangeTightEnough :: Bool
  -- ^ Is 'TxInfo.validTimeRange' tight enough?
  , timeRangeClosed :: Bool
  -- ^ Is 'TxInfo.validTimeRange' closed?
  , proposalStatus :: ProposalStatus
  -- ^ The status of the newly created proposal.
  }

--------------------------------------------------------------------------------

-- | See 'GovernorDatum.maximumProposalsPerStake'.
maxProposalPerStake :: Integer
maxProposalPerStake = 3

-- | The id of the proposal we are creating.
thisProposalId :: ProposalId
thisProposalId = ProposalId 25

-- | The arbitrary staked amount. Doesn;t really matter in this case.
stakedGTs :: Tagged _ Integer
stakedGTs = 5

-- | The owner of the stake.
stakeOwner :: PubKeyHash
stakeOwner = signer

{- | The invalid stake owner. If the 'alterOutputStakeOwner' is set to true,
      the output stake owner will be set to this.
-}
alteredStakeOwner :: PubKeyHash
alteredStakeOwner = signer2

-- | Locks the stake that the input stake already has.
defLocks :: [ProposalLock]
defLocks = [Created (ProposalId 0)]

-- | The effect of the newly created proposal.
defEffects :: AssocMap.Map ResultTag (AssocMap.Map ValidatorHash DatumHash)
defEffects =
  AssocMap.fromList
    [ (ResultTag 0, AssocMap.empty)
    , (ResultTag 1, AssocMap.empty)
    ]

--------------------------------------------------------------------------------

-- | The governor input datum.
governorInputDatum :: GovernorDatum
governorInputDatum =
  GovernorDatum
    { proposalThresholds = def
    , nextProposalId = thisProposalId
    , proposalTimings = def
    , createProposalTimeRangeMaxWidth = def
    , maximumProposalsPerStake = maxProposalPerStake
    }

-- | Create governor output datum given the parameters.
mkGovernorOutputDatum :: Parameters -> GovernorDatum
mkGovernorOutputDatum ps =
  let nextPid =
        if ps.advanceNextProposalId
          then ProposalId $ coerce thisProposalId + 1
          else thisProposalId
   in GovernorDatum
        { proposalThresholds = def
        , nextProposalId = nextPid
        , proposalTimings = def
        , createProposalTimeRangeMaxWidth = def
        , maximumProposalsPerStake = maxProposalPerStake
        }

--------------------------------------------------------------------------------

-- | Create the stake input datum given the parameters.
mkStakeInputDatum :: Parameters -> StakeDatum
mkStakeInputDatum ps =
  let locks =
        if ps.createdMoreThanMaximumProposals
          then
            Created . ProposalId
              <$> take
                (fromInteger maxProposalPerStake)
                [1 ..]
          else defLocks
   in StakeDatum
        { stakedAmount = stakedGTs
        , owner = stakeOwner
        , lockedBy = locks
        }

-- | Create the stake output datum given the parameters.
mkStakeOutputDatum :: Parameters -> StakeDatum
mkStakeOutputDatum ps =
  let inputDatum = mkStakeInputDatum ps
      newLocks =
        if ps.invalidNewLocks
          then
            [ Voted thisProposalId (ResultTag 0)
            , Voted thisProposalId (ResultTag 1)
            ]
          else [Created thisProposalId]
      locks = newLocks <> inputDatum.lockedBy
      newOwner = mkOwner ps
   in inputDatum
        { owner = newOwner
        , lockedBy = locks
        }

--------------------------------------------------------------------------------

{- | Create the proposal datum for the newly created proposal, given the
      parameters.
-}
mkProposalOutputDatum :: Parameters -> ProposalDatum
mkProposalOutputDatum ps =
  ProposalDatum
    { proposalId = thisProposalId
    , effects = defEffects
    , status = ps.proposalStatus
    , cosigners = [mkOwner ps]
    , thresholds = def
    , votes = emptyVotesFor defEffects
    , timingConfig = def
    , startingTime = mkProposalStartingTime ps
    }

--------------------------------------------------------------------------------

-- | Create time range for 'TxInfo.validTimeRange'.
mkTimeRange :: Parameters -> POSIXTimeRange
mkTimeRange ps =
  if ps.timeRangeClosed
    then
      let s = 0
          di :: POSIXTime = coerce (def @MaxTimeRangeWidth)
          o = if ps.timeRangeTightEnough then (-1) else 1
       in closedBoundedInterval s $ o + di
    else always

-- | Get the starting time of the propsoal.
mkProposalStartingTime :: Parameters -> ProposalStartingTime
mkProposalStartingTime ps =
  if ps.timeRangeClosed
    then proposalStartingTimeFromTimeRange $ mkTimeRange ps
    else ProposalStartingTime 0

-- | Who should be the 'owner' of the output stake.
mkOwner :: Parameters -> PubKeyHash
mkOwner ps =
  if ps.alterOutputStakeOwner
    then alteredStakeOwner
    else stakeOwner

--------------------------------------------------------------------------------

-- | Reference to the input stake UTXO.
stakeRef :: TxOutRef
stakeRef = TxOutRef stakeTxRef 1

-- | Reference to the input governor UTXO.
governorRef :: TxOutRef
governorRef = TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 3

--------------------------------------------------------------------------------

-- | Create a 'TxInfo' that spends a stake to create a new proposal.
createProposal :: Parameters -> TxInfo
createProposal ps = buildTxInfoUnsafe builder
  where
    pst = Value.singleton proposalPolicySymbol "" 1
    sst = Value.assetClassValue stakeAssetClass 1
    gst = Value.assetClassValue proposal.governorSTAssetClass 1

    ---

    governorValue = sortValue $ gst <> minAda
    stakeValue =
      sortValue $
        sortValue $
          sst
            <> Value.assetClassValue (untag stake.gtClassRef) (untag stakedGTs)
            <> minAda
    proposalValue = sortValue $ pst <> minAda

    ---

    withSig =
      if ps.stakeOwnerSignsTheTransaction
        then signedWith stakeOwner
        else mempty

    ---

    builder :: BaseBuilder
    builder =
      mconcat
        [ txId "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
        , ---
          withSig
        , ---
          mint pst
        , ---
          timeRange $ mkTimeRange ps
        , input $
            script govValidatorHash
              . withValue governorValue
              . withDatum governorInputDatum
              . withOutRef governorRef
        , output $
            script govValidatorHash
              . withValue governorValue
              . withDatum (mkGovernorOutputDatum ps)
        , ---
          input $
            script stakeValidatorHash
              . withValue stakeValue
              . withDatum (mkStakeInputDatum ps)
              . withOutRef stakeRef
        , output $
            script stakeValidatorHash
              . withValue stakeValue
              . withDatum (mkStakeOutputDatum ps)
        , ---
          output $
            script proposalValidatorHash
              . withValue proposalValue
              . withDatum (mkProposalOutputDatum ps)
        ]

--------------------------------------------------------------------------------

-- | Spend the stake with the 'PermitVote' redeemer.
stakeRedeemer :: StakeRedeemer
stakeRedeemer = PermitVote

-- | Spend the governor with the 'CreateProposal' redeemer.
governorRedeemer :: GovernorRedeemer
governorRedeemer = CreateProposal

-- | Mint the PST with an arbitrary redeemer. Doesn't really matter.
proposalPolicyRedeemer :: ()
proposalPolicyRedeemer = ()

--------------------------------------------------------------------------------

totallyValidParameters :: Parameters
totallyValidParameters =
  Parameters
    { advanceNextProposalId = True
    , createdMoreThanMaximumProposals = False
    , stakeOwnerSignsTheTransaction = True
    , invalidNewLocks = False
    , alterOutputStakeOwner = False
    , timeRangeTightEnough = True
    , timeRangeClosed = True
    , proposalStatus = Draft
    }

invalidOutputGovernorDatumParameters :: Parameters
invalidOutputGovernorDatumParameters =
  totallyValidParameters
    { advanceNextProposalId = False
    }

useStakeOwnBySomeoneElseParameters :: Parameters
useStakeOwnBySomeoneElseParameters =
  totallyValidParameters
    { stakeOwnerSignsTheTransaction = False
    }

invalidOutputStakeParameters :: Parameters
invalidOutputStakeParameters =
  totallyValidParameters
    { alterOutputStakeOwner = True
    }

addInvalidLocksParameters :: Parameters
addInvalidLocksParameters =
  totallyValidParameters
    { invalidNewLocks = True
    }

exceedMaximumProposalsParameters :: Parameters
exceedMaximumProposalsParameters =
  totallyValidParameters
    { createdMoreThanMaximumProposals = True
    }

timeRangeNotTightParameters :: Parameters
timeRangeNotTightParameters =
  totallyValidParameters
    { timeRangeTightEnough = False
    }

timeRangeNotClosedParameters :: Parameters
timeRangeNotClosedParameters =
  totallyValidParameters
    { timeRangeClosed = False
    }

invalidProposalStatusParameters :: [Parameters]
invalidProposalStatusParameters =
  map
    ( \st ->
        totallyValidParameters {proposalStatus = st}
    )
    [VotingReady, Locked, Finished]

--------------------------------------------------------------------------------

{- | Create a test tree that runs the propsoal minting policy, the governor
      validator and the stake validator to test the functionalities of creting
      proposals
-}
mkTestTree :: String -> Parameters -> Bool -> Bool -> Bool -> SpecificationTree
mkTestTree
  name
  ps
  validForProposalPolicy
  validForGovernorValidator
  validForStakeValidator =
    group name [proposalTest, governorTest, stakeTest]
    where
      txInfo = createProposal ps

      proposalTest =
        testPolicy
          validForProposalPolicy
          "proposal"
          (proposalPolicy Shared.proposal.governorSTAssetClass)
          proposalPolicyRedeemer
          (ScriptContext txInfo (Minting proposalPolicySymbol))

      governorTest =
        testValidator
          validForGovernorValidator
          "governor"
          (governorValidator Shared.governor)
          governorInputDatum
          governorRedeemer
          ( ScriptContext
              txInfo
              (Spending governorRef)
          )

      stakeTest =
        testValidator
          validForStakeValidator
          "stake"
          (stakeValidator Shared.stake)
          (mkStakeInputDatum ps)
          stakeRedeemer
          ( ScriptContext
              txInfo
              (Spending stakeRef)
          )
