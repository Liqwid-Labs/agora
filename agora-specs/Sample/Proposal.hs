{- |
Module     : Sample.Proposal
Maintainer : emi@haskell.fyi
Description: Sample based testing for Proposal utxos

This module tests primarily the happy path for Proposal interactions
-}
module Sample.Proposal (
  -- * Script contexts
  proposalCreation,
  cosignProposal,
  proposalRef,
  stakeRef,
  voteOnProposal,
  VotingParameters (..),
  advanceProposalSuccess,
  advanceProposalFailureTimeout,
  TransitionParameters (..),
  advanceFinishedPropsoal,
  advanceProposalInsufficientVotes,
  advancePropsoalWithInvalidOutputStake,
) where

import Agora.Governor (GovernorDatum (..))
import Agora.Proposal (
  Proposal (..),
  ProposalDatum (..),
  ProposalId (..),
  ProposalStatus (..),
  ProposalThresholds (..),
  ProposalVotes (..),
  ResultTag (..),
  emptyVotesFor,
 )
import Agora.Proposal.Time (
  ProposalStartingTime (ProposalStartingTime),
  ProposalTimingConfig (..),
 )
import Agora.Stake (
  ProposalLock (ProposalLock),
  Stake (..),
  StakeDatum (..),
 )
import Data.Default.Class (Default (def))
import Data.Tagged (Tagged (..), untag)
import Plutarch.Context (
  BaseBuilder,
  MintingBuilder,
  buildMintingUnsafe,
  buildTxInfoUnsafe,
  input,
  mint,
  output,
  script,
  signedWith,
  timeRange,
  txId,
  withDatum,
  withRefIndex,
  withTxId,
  withValue,
 )
import PlutusLedgerApi.V1 (
  Datum (Datum),
  DatumHash,
  POSIXTime,
  POSIXTimeRange,
  PubKeyHash,
  ScriptContext (..),
  ToData (toBuiltinData),
  TxInInfo (TxInInfo),
  TxInfo (..),
  TxOut (TxOut, txOutAddress, txOutDatumHash, txOutValue),
  TxOutRef (..),
  ValidatorHash,
 )
import PlutusLedgerApi.V1.Value qualified as Value (
  assetClassValue,
  singleton,
 )
import PlutusTx.AssocMap qualified as AssocMap (
  Map,
  empty,
  fromList,
 )
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
  stakeAddress,
  stakeAssetClass,
  stakeValidatorHash,
 )
import Test.Util (
  closedBoundedInterval,
  datumPair,
  toDatumHash,
  updateMap,
 )

proposalCreation :: ScriptContext
proposalCreation =
  let st = Value.singleton proposalPolicySymbol "" 1 -- Proposal ST
      effects =
        AssocMap.fromList
          [ (ResultTag 0, AssocMap.empty)
          , (ResultTag 1, AssocMap.empty)
          ]
      proposalDatum :: ProposalDatum
      proposalDatum =
        ProposalDatum
          { proposalId = ProposalId 0
          , effects = effects
          , status = Draft
          , cosigners = [signer]
          , thresholds = def
          , votes = emptyVotesFor effects
          , timingConfig = def
          , startingTime = proposalStartingTimeFromTimeRange validTimeRange
          }

      govBefore :: GovernorDatum
      govBefore =
        GovernorDatum
          { proposalThresholds = def
          , nextProposalId = ProposalId 0
          , proposalTimings = def
          , createProposalTimeRangeMaxWidth = def
          }

      govAfter :: GovernorDatum
      govAfter = govBefore {nextProposalId = ProposalId 1}

      validTimeRange = closedBoundedInterval 10 15

      builder :: MintingBuilder
      builder =
        mconcat
          [ txId "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
          , signedWith signer
          , mint st
          , input $
              script govValidatorHash
                . withValue (Value.assetClassValue proposal.governorSTAssetClass 1)
                . withDatum govBefore
                . withTxId "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
          , output $
              script proposalValidatorHash
                . withValue (st <> Value.singleton "" "" 10_000_000)
                . withDatum proposalDatum
          , output $
              script govValidatorHash
                . withValue
                  ( Value.assetClassValue proposal.governorSTAssetClass 1
                      <> Value.singleton "" "" 10_000_000
                  )
                . withDatum govAfter
          ]
   in buildMintingUnsafe builder

proposalRef :: TxOutRef
proposalRef = TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 1

stakeRef :: TxOutRef
stakeRef = TxOutRef "0ca36f3a357bc69579ab2531aecd1e7d3714d993c7820f40b864be15" 0

-- | This script context should be a valid transaction.
cosignProposal :: [PubKeyHash] -> TxInfo
cosignProposal newSigners =
  let st = Value.singleton proposalPolicySymbol "" 1 -- Proposal ST
      effects =
        AssocMap.fromList
          [ (ResultTag 0, AssocMap.empty)
          , (ResultTag 1, AssocMap.empty)
          ]
      proposalBefore :: ProposalDatum
      proposalBefore =
        ProposalDatum
          { proposalId = ProposalId 0
          , effects = effects
          , status = Draft
          , cosigners = [signer]
          , thresholds = def
          , votes = emptyVotesFor effects
          , timingConfig = def
          , startingTime = ProposalStartingTime 0
          }
      stakeDatum :: StakeDatum
      stakeDatum = StakeDatum (Tagged 50_000_000) signer2 []
      proposalAfter :: ProposalDatum
      proposalAfter = proposalBefore {cosigners = newSigners <> proposalBefore.cosigners}
      validTimeRange :: POSIXTimeRange
      validTimeRange =
        closedBoundedInterval
          10
          ((def :: ProposalTimingConfig).draftTime - 10)
      builder :: BaseBuilder
      builder =
        mconcat
          [ txId "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
          , mint st
          , mconcat $ signedWith <$> newSigners
          , timeRange validTimeRange
          , input $
              script proposalValidatorHash
                . withValue (st <> Value.singleton "" "" 10_000_000)
                . withDatum proposalBefore
                . withTxId (txOutRefId proposalRef)
                . withRefIndex (txOutRefIdx proposalRef)
          , input $
              script stakeValidatorHash
                . withValue
                  ( Value.singleton "" "" 10_000_000
                      <> Value.assetClassValue (untag stake.gtClassRef) 50_000_000
                      <> Value.assetClassValue stakeAssetClass 1
                  )
                . withDatum stakeDatum
                . withTxId (txOutRefId stakeRef)
                . withRefIndex (txOutRefIdx stakeRef)
          , output $
              script proposalValidatorHash
                . withValue (st <> Value.singleton "" "" 10_000_000)
                . withDatum proposalAfter
          , output $
              script stakeValidatorHash
                . withValue
                  ( Value.singleton "" "" 10_000_000
                      <> Value.assetClassValue (untag stake.gtClassRef) 50_000_000
                      <> Value.assetClassValue stakeAssetClass 1
                  )
                . withDatum stakeDatum
          ]
   in buildTxInfoUnsafe builder

--------------------------------------------------------------------------------

-- | Parameters for creating a voting transaction.
data VotingParameters = VotingParameters
  { voteFor :: ResultTag
  -- ^ The outcome the transaction is voting for.
  , voteCount :: Integer
  -- ^ The count of votes.
  }

-- | Create a valid transaction that votes on a propsal, given the parameters.
voteOnProposal :: VotingParameters -> TxInfo
voteOnProposal params =
  let pst = Value.singleton proposalPolicySymbol "" 1
      sst = Value.assetClassValue stakeAssetClass 1

      ---

      stakeOwner = signer

      ---

      effects =
        AssocMap.fromList
          [ (ResultTag 0, AssocMap.empty)
          , (ResultTag 1, AssocMap.empty)
          ]

      ---

      initialVotes :: AssocMap.Map ResultTag Integer
      initialVotes =
        AssocMap.fromList
          [ (ResultTag 0, 42)
          , (ResultTag 1, 4242)
          ]

      ---

      proposalInputDatum :: ProposalDatum
      proposalInputDatum =
        ProposalDatum
          { proposalId = ProposalId 42
          , effects = effects
          , status = VotingReady
          , cosigners = [stakeOwner]
          , thresholds = def
          , votes = ProposalVotes initialVotes
          , timingConfig = def
          , startingTime = ProposalStartingTime 0
          }

      ---

      existingLocks :: [ProposalLock]
      existingLocks =
        [ ProposalLock (ResultTag 0) (ProposalId 0)
        , ProposalLock (ResultTag 2) (ProposalId 1)
        ]

      ---

      stakeInputDatum :: StakeDatum
      stakeInputDatum =
        StakeDatum
          { stakedAmount = Tagged params.voteCount
          , owner = stakeOwner
          , lockedBy = existingLocks
          }

      ---

      updatedVotes :: AssocMap.Map ResultTag Integer
      updatedVotes = updateMap (Just . (+ params.voteCount)) params.voteFor initialVotes

      ---

      proposalOutputDatum :: ProposalDatum
      proposalOutputDatum =
        proposalInputDatum
          { votes = ProposalVotes updatedVotes
          }

      ---

      -- Off-chain code should do exactly like this: prepend new lock toStatus the list.
      updatedLocks :: [ProposalLock]
      updatedLocks = ProposalLock params.voteFor proposalInputDatum.proposalId : existingLocks

      ---

      stakeOutputDatum :: StakeDatum
      stakeOutputDatum =
        stakeInputDatum
          { lockedBy = updatedLocks
          }

      ---

      validTimeRange =
        closedBoundedInterval
          ((def :: ProposalTimingConfig).draftTime + 1)
          ((def :: ProposalTimingConfig).votingTime - 1)

      builder :: BaseBuilder
      builder =
        mconcat
          [ txId "827598fb2d69a896bbd9e645bb14c307df907f422b39eecbe4d6329bc30b428c"
          , signedWith stakeOwner
          , timeRange validTimeRange
          , input $
              script proposalValidatorHash
                . withValue pst
                . withDatum proposalInputDatum
                . withTxId (txOutRefId proposalRef)
                . withRefIndex (txOutRefIdx proposalRef)
          , input $
              script stakeValidatorHash
                . withValue
                  ( sst
                      <> Value.assetClassValue (untag stake.gtClassRef) params.voteCount
                      <> minAda
                  )
                . withDatum stakeInputDatum
                . withTxId (txOutRefId stakeRef)
                . withRefIndex (txOutRefIdx stakeRef)
          , output $
              script proposalValidatorHash
                . withValue pst
                . withDatum proposalOutputDatum
          , output $
              script stakeValidatorHash
                . withValue
                  ( sst
                      <> Value.assetClassValue (untag stake.gtClassRef) params.voteCount
                      <> minAda
                  )
                . withDatum stakeOutputDatum
          ]
   in buildTxInfoUnsafe builder

--------------------------------------------------------------------------------

-- | Parameters for state transition of proposals.
data TransitionParameters = TransitionParameters
  { -- The initial status of the propsoal.
    initialProposalStatus :: ProposalStatus
  , -- The starting time of the propsoal.
    proposalStartingTime :: ProposalStartingTime
  }

-- | Create a 'TxInfo' that update the status of a proposal.
mkTransitionTxInfo ::
  -- | Initial state of the proposal.
  ProposalStatus ->
  -- | Next state of the proposal.
  ProposalStatus ->
  -- | Effects.
  AssocMap.Map ResultTag (AssocMap.Map ValidatorHash DatumHash) ->
  -- | Votes.
  ProposalVotes ->
  -- | Starting time of the proposal.
  ProposalStartingTime ->
  -- | Valid time range of the transaction.
  POSIXTimeRange ->
  -- | Add a unchanged stake or not.
  Bool ->
  TxInfo
mkTransitionTxInfo from to effects votes startingTime validTime shouldAddUnchangedStake =
  let pst = Value.singleton proposalPolicySymbol "" 1
      sst = Value.assetClassValue stakeAssetClass 1

      proposalInputDatum :: ProposalDatum
      proposalInputDatum =
        ProposalDatum
          { proposalId = ProposalId 0
          , effects = effects
          , status = from
          , cosigners = [signer]
          , thresholds = def
          , votes = votes
          , timingConfig = def
          , startingTime = startingTime
          }

      proposalOutputDatum :: ProposalDatum
      proposalOutputDatum =
        proposalInputDatum
          { status = to
          }

      stakeOwner = signer
      stakedAmount = 200

      existingLocks :: [ProposalLock]
      existingLocks =
        [ ProposalLock (ResultTag 0) (ProposalId 0)
        , ProposalLock (ResultTag 2) (ProposalId 1)
        ]

      stakeInputDatum :: StakeDatum
      stakeInputDatum =
        StakeDatum
          { stakedAmount = Tagged stakedAmount
          , owner = stakeOwner
          , lockedBy = existingLocks
          }

      stakeOutputDatum :: StakeDatum
      stakeOutputDatum = stakeInputDatum

      stakeBuilder :: BaseBuilder
      stakeBuilder =
        if shouldAddUnchangedStake
          then
            mconcat
              [ input $
                  script stakeValidatorHash
                    . withValue sst
                    . withDatum stakeInputDatum
                    . withTxId (txOutRefId stakeRef)
              , output $
                  script stakeValidatorHash
                    . withValue (sst <> minAda)
                    . withDatum stakeOutputDatum
              ]
          else mempty

      builder :: BaseBuilder
      builder =
        mconcat
          [ txId "95ba4015e30aef16a3461ea97a779f814aeea6b8009d99a94add4b8293be737a"
          , signedWith stakeOwner
          , timeRange validTime
          , input $
              script proposalValidatorHash
                . withValue pst
                . withDatum proposalInputDatum
                . withTxId (txOutRefId proposalRef)
          , output $
              script proposalValidatorHash
                . withValue (pst <> minAda)
                . withDatum proposalOutputDatum
          ]
   in buildTxInfoUnsafe $ builder <> stakeBuilder

-- | Wrapper around 'advanceProposalSuccess'', with valid stake.
advanceProposalSuccess :: TransitionParameters -> TxInfo
advanceProposalSuccess ps = advanceProposalSuccess' ps True

{- | Create a valid 'TxInfo' that advances a proposal, given the parameters.
     The second parameter determines wherther valid stake should be included.

     Note that 'TransitionParameters.initialProposalStatus' should not be 'Finished'.
-}
advanceProposalSuccess' :: TransitionParameters -> Bool -> TxInfo
advanceProposalSuccess' params =
  let -- Status of the output proposal.
      toStatus :: ProposalStatus
      toStatus = case params.initialProposalStatus of
        Draft -> VotingReady
        VotingReady -> Locked
        Locked -> Finished
        Finished -> error "Cannot advance 'Finished' proposal"

      effects =
        AssocMap.fromList
          [ (ResultTag 0, AssocMap.empty)
          , (ResultTag 1, AssocMap.empty)
          ]

      emptyVotes@(ProposalVotes emptyVotes') = emptyVotesFor effects

      -- Set the vote count of outcome 0 to @def.countingVoting + 1@,
      --   meaning that outcome 0 will be the winner.
      outcome0WinningVotes =
        ProposalVotes $
          updateMap
            (\_ -> Just $ untag (def :: ProposalThresholds).vote + 1)
            (ResultTag 0)
            emptyVotes'

      votes :: ProposalVotes
      votes = case params.initialProposalStatus of
        Draft -> emptyVotes
        -- With sufficient votes
        _ -> outcome0WinningVotes

      proposalStartingTime :: POSIXTime
      proposalStartingTime =
        let (ProposalStartingTime startingTime) = params.proposalStartingTime
         in startingTime

      timeRange :: POSIXTimeRange
      timeRange = case params.initialProposalStatus of
        -- [S + 1, S + D - 1]
        Draft ->
          closedBoundedInterval
            (proposalStartingTime + 1)
            (proposalStartingTime + (def :: ProposalTimingConfig).draftTime - 1)
        -- [S + D + V + 1, S + D + V + L - 1]
        VotingReady ->
          closedBoundedInterval
            ( proposalStartingTime
                + (def :: ProposalTimingConfig).draftTime
                + (def :: ProposalTimingConfig).votingTime
                + 1
            )
            ( proposalStartingTime
                + (def :: ProposalTimingConfig).draftTime
                + (def :: ProposalTimingConfig).votingTime
                + (def :: ProposalTimingConfig).lockingTime
                - 1
            )
        -- [S + D + V + L + 1, S + + D + V + L + E - 1]
        Locked ->
          closedBoundedInterval
            ( proposalStartingTime
                + (def :: ProposalTimingConfig).draftTime
                + (def :: ProposalTimingConfig).votingTime
                + (def :: ProposalTimingConfig).lockingTime
                + 1
            )
            ( proposalStartingTime
                + (def :: ProposalTimingConfig).draftTime
                + (def :: ProposalTimingConfig).votingTime
                + (def :: ProposalTimingConfig).lockingTime
                + (def :: ProposalTimingConfig).executingTime - 1
            )
        Finished -> error "Cannot advance 'Finished' proposal"
   in mkTransitionTxInfo
        params.initialProposalStatus
        toStatus
        effects
        votes
        params.proposalStartingTime
        timeRange

{- | Create a valid 'TxInfo' that advances a proposal to failed state, given the parameters.
     The reason why the proposal fails is the proposal has ran out of time.
     Note that 'TransitionParameters.initialProposalStatus' should not be 'Finished'.
-}
advanceProposalFailureTimeout :: TransitionParameters -> TxInfo
advanceProposalFailureTimeout params =
  let effects =
        AssocMap.fromList
          [ (ResultTag 0, AssocMap.empty)
          , (ResultTag 1, AssocMap.empty)
          ]

      emptyVotes@(ProposalVotes emptyVotes') = emptyVotesFor effects

      -- Set the vote count of outcome 0 to @def.countingVoting + 1@,
      --   meaning that outcome 0 will be the winner.
      outcome0WinningVotes =
        ProposalVotes $
          updateMap
            (\_ -> Just $ untag (def :: ProposalThresholds).vote + 1)
            (ResultTag 0)
            emptyVotes'

      votes :: ProposalVotes
      votes = case params.initialProposalStatus of
        Draft -> emptyVotes
        -- With sufficient votes
        _ -> outcome0WinningVotes

      proposalStartingTime :: POSIXTime
      proposalStartingTime =
        let (ProposalStartingTime startingTime) = params.proposalStartingTime
         in startingTime

      timeRange :: POSIXTimeRange
      timeRange = case params.initialProposalStatus of
        -- [S + D + 1, S + D + V - 1]
        Draft ->
          closedBoundedInterval
            (proposalStartingTime + (def :: ProposalTimingConfig).draftTime + 1)
            ( proposalStartingTime
                + (def :: ProposalTimingConfig).draftTime
                + (def :: ProposalTimingConfig).votingTime - 1
            )
        -- [S + D + V + L + 1, S + D + V + L + E -1]
        VotingReady ->
          closedBoundedInterval
            ( proposalStartingTime
                + (def :: ProposalTimingConfig).draftTime
                + (def :: ProposalTimingConfig).votingTime
                + (def :: ProposalTimingConfig).lockingTime
                + 1
            )
            ( proposalStartingTime
                + (def :: ProposalTimingConfig).draftTime
                + (def :: ProposalTimingConfig).votingTime
                + (def :: ProposalTimingConfig).lockingTime
                + (def :: ProposalTimingConfig).executingTime
                - 1
            )
        -- [S + D + V + L + E + 1, S + D + V + L + E + 100]
        Locked ->
          closedBoundedInterval
            ( proposalStartingTime
                + (def :: ProposalTimingConfig).draftTime
                + (def :: ProposalTimingConfig).votingTime
                + (def :: ProposalTimingConfig).lockingTime
                + (def :: ProposalTimingConfig).executingTime
                + 1
            )
            ( proposalStartingTime
                + (def :: ProposalTimingConfig).draftTime
                + (def :: ProposalTimingConfig).votingTime
                + (def :: ProposalTimingConfig).lockingTime
                + (def :: ProposalTimingConfig).executingTime
                + 100
            )
        Finished -> error "Cannot advance 'Finished' proposal"
   in mkTransitionTxInfo
        params.initialProposalStatus
        Finished
        effects
        votes
        params.proposalStartingTime
        timeRange
        True

-- | An invalid 'TxInfo' that tries to advance a 'VotingReady' proposal without sufficient votes.
advanceProposalInsufficientVotes :: TxInfo
advanceProposalInsufficientVotes =
  let effects =
        AssocMap.fromList
          [ (ResultTag 0, AssocMap.empty)
          , (ResultTag 1, AssocMap.empty)
          ]

      -- Insufficient votes.
      votes = emptyVotesFor effects

      proposalStartingTime = 0

      -- Valid time range.
      -- [S + D + 1, S + V - 1]
      timeRange =
        closedBoundedInterval
          (proposalStartingTime + (def :: ProposalTimingConfig).draftTime + 1)
          (proposalStartingTime + (def :: ProposalTimingConfig).votingTime - 1)
   in mkTransitionTxInfo
        VotingReady
        Locked
        effects
        votes
        (ProposalStartingTime proposalStartingTime)
        timeRange
        True

-- | An invalid 'TxInfo' that tries to advance a 'Finished' proposal.
advanceFinishedPropsoal :: TxInfo
advanceFinishedPropsoal =
  let effects =
        AssocMap.fromList
          [ (ResultTag 0, AssocMap.empty)
          , (ResultTag 1, AssocMap.empty)
          ]

      -- Set the vote count of outcome 0 to @def.countingVoting + 1@,
      --   meaning that outcome 0 will be the winner.
      outcome0WinningVotes =
        ProposalVotes $
          AssocMap.fromList
            [ (ResultTag 0, untag (def :: ProposalThresholds).vote + 1)
            , (ResultTag 1, 0)
            ]

      ---

      timeRange =
        closedBoundedInterval
          ((def :: ProposalTimingConfig).lockingTime + 1)
          ((def :: ProposalTimingConfig).executingTime - 1)
   in mkTransitionTxInfo
        Finished
        Finished
        effects
        outcome0WinningVotes
        (ProposalStartingTime 0)
        timeRange
        True

{- | An illegal 'TxInfo' that tries to output a changed stake with 'AdvanceProposal'.
     From the perspective of stake validator, the transition is totally valid,
       so the proposal validator should reject this.
-}
advancePropsoalWithInvalidOutputStake :: TxInfo
advancePropsoalWithInvalidOutputStake =
  let templateTxInfo =
        advanceProposalSuccess'
          TransitionParameters
            { initialProposalStatus = VotingReady
            , proposalStartingTime = ProposalStartingTime 0
            }
          False

      ---
      -- Now we create a new lock on an arbitrary stake

      sst = Value.assetClassValue stakeAssetClass 1

      ---

      stakeOwner = signer
      stakedAmount = 200

      ---

      existingLocks :: [ProposalLock]
      existingLocks =
        [ ProposalLock (ResultTag 0) (ProposalId 0)
        , ProposalLock (ResultTag 2) (ProposalId 1)
        ]

      ---

      stakeInputDatum' :: StakeDatum
      stakeInputDatum' =
        StakeDatum
          { stakedAmount = Tagged stakedAmount
          , owner = stakeOwner
          , lockedBy = existingLocks
          }
      stakeInputDatum :: Datum
      stakeInputDatum = Datum $ toBuiltinData stakeInputDatum'
      stakeInput :: TxOut
      stakeInput =
        TxOut
          { txOutAddress = stakeAddress
          , txOutValue =
              mconcat
                [ sst
                , Value.assetClassValue (untag stake.gtClassRef) stakedAmount
                , minAda
                ]
          , txOutDatumHash = Just $ toDatumHash stakeInputDatum
          }

      ---

      updatedLocks :: [ProposalLock]
      updatedLocks = ProposalLock (ResultTag 42) (ProposalId 27) : existingLocks

      ---

      stakeOutputDatum' :: StakeDatum
      stakeOutputDatum' =
        stakeInputDatum'
          { lockedBy = updatedLocks
          }
      stakeOutputDatum :: Datum
      stakeOutputDatum = Datum $ toBuiltinData stakeOutputDatum'
      stakeOutput :: TxOut
      stakeOutput =
        stakeInput
          { txOutDatumHash = Just $ toDatumHash stakeOutputDatum
          }
   in templateTxInfo
        { txInfoInputs = TxInInfo stakeRef stakeInput : templateTxInfo.txInfoInputs
        , txInfoOutputs = stakeOutput : templateTxInfo.txInfoOutputs
        , txInfoData =
            (datumPair <$> [stakeInputDatum, stakeOutputDatum])
              <> templateTxInfo.txInfoData
        , txInfoSignatories = [stakeOwner]
        }
