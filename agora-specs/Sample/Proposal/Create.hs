{- |
Module     : Sample.Proposal.Create
Maintainer : connor@mlabs.city
Description: Generate sample data for testing the functionalities of creating proposals

Sample and utilities for testing the functionalities of creating proposals.
-}
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
  Governor (..),
  GovernorDatum (..),
  GovernorRedeemer (CreateProposal),
 )
import Agora.Proposal (
  ProposalDatum (..),
  ProposalEffectGroup,
  ProposalId (ProposalId),
  ProposalStatus (..),
  ResultTag (ResultTag),
  emptyVotesFor,
 )
import Agora.Proposal.Time (
  MaxTimeRangeWidth (
    MaxTimeRangeWidth
  ),
  ProposalStartingTime (..),
 )
import Agora.SafeMoney (GTTag)
import Agora.Scripts (AgoraScripts (..))
import Agora.Stake (
  ProposalLock (..),
  StakeDatum (..),
  StakeRedeemer (PermitVote),
 )
import Data.Coerce (coerce)
import Data.Default (Default (def))
import Data.Map.Strict qualified as StrictMap
import Data.Tagged (untag)
import Plutarch.Context (
  input,
  mint,
  output,
  script,
  signedWith,
  timeRange,
  txId,
  withDatum,
  withRef,
  withValue,
 )
import Plutarch.SafeMoney (Discrete)
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 (
  Credential (PubKeyCredential),
  POSIXTime (POSIXTime),
  POSIXTimeRange,
  TxOutRef (TxOutRef),
  always,
 )
import Sample.Proposal.Shared (stakeTxRef)
import Sample.Shared (
  agoraScripts,
  fromDiscrete,
  govAssetClass,
  govValidatorHash,
  governor,
  minAda,
  proposalPolicySymbol,
  proposalStartingTimeFromTimeRange,
  proposalValidatorHash,
  signer,
  signer2,
  stakeAssetClass,
  stakeValidatorHash,
 )
import Test.Specification (SpecificationTree, group, testPolicy, testValidator)
import Test.Util (
  CombinableBuilder,
  closedBoundedInterval,
  mkMinting,
  mkSpending,
  sortValue,
 )

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
stakedGTs :: Discrete GTTag
stakedGTs = 5

-- | The owner of the stake.
stakeOwner :: Credential
stakeOwner = PubKeyCredential signer

{- | The invalid stake owner. If the 'alterOutputStakeOwner' is set to true,
      the output stake owner will be set to this.
-}
alteredStakeOwner :: Credential
alteredStakeOwner = PubKeyCredential signer2

-- | Locks the stake that the input stake already has.
defLocks :: [ProposalLock]
defLocks = [Created (ProposalId 0)]

-- | The effect of the newly created proposal.
defEffects :: StrictMap.Map ResultTag ProposalEffectGroup
defEffects =
  StrictMap.fromList
    [ (ResultTag 0, StrictMap.empty)
    , (ResultTag 1, StrictMap.empty)
    , (ResultTag 3, StrictMap.empty)
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
        , delegatedTo = Nothing
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
  let effects = defEffects
      votes = emptyVotesFor defEffects
   in ProposalDatum
        { proposalId = thisProposalId
        , effects = effects
        , status = ps.proposalStatus
        , cosigners = [mkOwner ps]
        , thresholds = def
        , votes = votes
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

-- | Get the starting time of the proposal.
mkProposalStartingTime :: Parameters -> ProposalStartingTime
mkProposalStartingTime ps =
  if ps.timeRangeClosed
    then proposalStartingTimeFromTimeRange $ mkTimeRange ps
    else ProposalStartingTime 0

-- | Who should be the 'owner' of the output stake.
mkOwner :: Parameters -> Credential
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
createProposal :: forall b. CombinableBuilder b => Parameters -> b
createProposal ps = builder
  where
    pst = Value.singleton proposalPolicySymbol "" 1
    sst = Value.assetClassValue stakeAssetClass 1
    gst = Value.assetClassValue govAssetClass 1

    ---

    governorValue = sortValue $ gst <> minAda
    stakeValue =
      sortValue $
        sst
          <> Value.assetClassValue (untag governor.gtClassRef) (fromDiscrete stakedGTs)
          <> minAda
    proposalValue = sortValue $ pst <> minAda

    ---

    withSig =
      if ps.stakeOwnerSignsTheTransaction
        then case stakeOwner of
          PubKeyCredential sig -> signedWith sig
          _ -> mempty
        else mempty

    ---

    builder =
      mconcat
        [ txId "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
        , ---
          withSig
        , ---
          mint $
            sortValue $
              pst
                <>
                -- 0 Ada entry, see #174
                Value.singleton "" "" 0
        , ---
          timeRange $ mkTimeRange ps
        , input $
            mconcat
              [ script govValidatorHash
              , withValue governorValue
              , withDatum governorInputDatum
              , withRef governorRef
              ]
        , output $
            mconcat
              [ script govValidatorHash
              , withValue governorValue
              , withDatum (mkGovernorOutputDatum ps)
              ]
        , ---
          input $
            mconcat
              [ script stakeValidatorHash
              , withValue stakeValue
              , withDatum (mkStakeInputDatum ps)
              , withRef stakeRef
              ]
        , output $
            mconcat
              [ script stakeValidatorHash
              , withValue stakeValue
              , withDatum (mkStakeOutputDatum ps)
              ]
        , ---
          output $
            mconcat
              [ script proposalValidatorHash
              , withValue proposalValue
              , withDatum (mkProposalOutputDatum ps)
              ]
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

{- | Create a test tree that runs the proposal minting policy, the governor
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
      mint = mkMinting createProposal ps
      spend = mkSpending createProposal ps

      proposalTest =
        testPolicy
          validForProposalPolicy
          "proposal"
          agoraScripts.compiledProposalPolicy
          proposalPolicyRedeemer
          (mint proposalPolicySymbol)

      governorTest =
        testValidator
          validForGovernorValidator
          "governor"
          agoraScripts.compiledGovernorValidator
          governorInputDatum
          governorRedeemer
          (spend governorRef)

      stakeTest =
        testValidator
          validForStakeValidator
          "stake"
          agoraScripts.compiledStakeValidator
          (mkStakeInputDatum ps)
          stakeRedeemer
          (spend stakeRef)
