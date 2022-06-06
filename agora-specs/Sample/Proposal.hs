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
  voterUnlockStakeAndRetractVotesWhile,
  voterUnlockStakeWhile,
  creatorRetractVotesWhile,
  creatorUnlockStakeWhile,
  unlockStakeAndRetractVotesUsingIrrelevantStakeWhile,
  unlockStakeUsingIrrelevantStakeWhile,
  unlockStakeProposalId,
  unlockStake,
) where

--------------------------------------------------------------------------------

import Plutarch.Api.V1 (
  validatorHash,
 )

--------------------------------------------------------------------------------

import PlutusLedgerApi.V1 (
  Address (Address),
  Credential (ScriptCredential),
  Datum (Datum),
  DatumHash,
  POSIXTime,
  POSIXTimeRange,
  PubKeyHash,
  ScriptContext (..),
  ScriptPurpose (..),
  ToData (toBuiltinData),
  TxInInfo (TxInInfo),
  TxInfo (..),
  TxOut (TxOut, txOutAddress, txOutDatumHash, txOutValue),
  TxOutRef (TxOutRef),
  ValidatorHash,
 )
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusTx.AssocMap qualified as AssocMap

--------------------------------------------------------------------------------

import Agora.Governor (
  GovernorDatum (..),
 )
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
import Agora.Proposal.Time (ProposalStartingTime (ProposalStartingTime), ProposalTimingConfig (..))
import Agora.Stake (ProposalLock (ProposalLock), Stake (..), StakeDatum (..))
import Data.Tagged (Tagged (..), untag)
import Sample.Shared
import Test.Util (closedBoundedInterval, datumPair, toDatumHash, updateMap)

--------------------------------------------------------------------------------

import Data.Default.Class (Default (def))

--------------------------------------------------------------------------------

-- | This script context should be a valid transaction.
proposalCreation :: ScriptContext
proposalCreation =
  let st = Value.singleton proposalPolicySymbol "" 1 -- Proposal ST
      effects =
        AssocMap.fromList
          [ (ResultTag 0, AssocMap.empty)
          , (ResultTag 1, AssocMap.empty)
          ]
      proposalDatum :: Datum
      proposalDatum =
        Datum
          ( toBuiltinData $
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
          )

      govBefore :: Datum
      govBefore =
        Datum
          ( toBuiltinData $
              GovernorDatum
                { proposalThresholds = def
                , nextProposalId = ProposalId 0
                , proposalTimings = def
                , createProposalTimeRangeMaxWidth = def
                }
          )
      govAfter :: Datum
      govAfter =
        Datum
          ( toBuiltinData $
              GovernorDatum
                { proposalThresholds = def
                , nextProposalId = ProposalId 1
                , proposalTimings = def
                , createProposalTimeRangeMaxWidth = def
                }
          )

      validTimeRange = closedBoundedInterval 10 15
   in ScriptContext
        { scriptContextTxInfo =
            TxInfo
              { txInfoInputs =
                  [ TxInInfo
                      (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 1)
                      TxOut
                        { txOutAddress = Address (ScriptCredential $ validatorHash govValidator) Nothing
                        , txOutValue = Value.assetClassValue proposal.governorSTAssetClass 1
                        , txOutDatumHash = Just (toDatumHash govBefore)
                        }
                  ]
              , txInfoOutputs =
                  [ TxOut
                      { txOutAddress = Address (ScriptCredential proposalValidatorHash) Nothing
                      , txOutValue =
                          mconcat
                            [ st
                            , Value.singleton "" "" 10_000_000
                            ]
                      , txOutDatumHash = Just (toDatumHash proposalDatum)
                      }
                  , TxOut
                      { txOutAddress = Address (ScriptCredential $ validatorHash govValidator) Nothing
                      , txOutValue =
                          mconcat
                            [ Value.assetClassValue proposal.governorSTAssetClass 1
                            , Value.singleton "" "" 10_000_000
                            ]
                      , txOutDatumHash = Just (toDatumHash govAfter)
                      }
                  ]
              , txInfoFee = Value.singleton "" "" 2
              , txInfoMint = st
              , txInfoDCert = []
              , txInfoWdrl = []
              , txInfoValidRange = validTimeRange
              , txInfoSignatories = [signer]
              , txInfoData =
                  [ datumPair proposalDatum
                  , datumPair govBefore
                  , datumPair govAfter
                  ]
              , txInfoId = "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
              }
        , scriptContextPurpose = Minting proposalPolicySymbol
        }

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
   in TxInfo
        { txInfoInputs =
            [ TxInInfo
                proposalRef
                TxOut
                  { txOutAddress = proposalValidatorAddress
                  , txOutValue =
                      mconcat
                        [ st
                        , Value.singleton "" "" 10_000_000
                        ]
                  , txOutDatumHash = Just (toDatumHash proposalBefore)
                  }
            , TxInInfo
                stakeRef
                TxOut
                  { txOutAddress = stakeAddress
                  , txOutValue =
                      mconcat
                        [ Value.singleton "" "" 10_000_000
                        , Value.assetClassValue (untag stake.gtClassRef) 50_000_000
                        , Value.assetClassValue stakeAssetClass 1
                        ]
                  , txOutDatumHash = Just (toDatumHash stakeDatum)
                  }
            ]
        , txInfoOutputs =
            [ TxOut
                { txOutAddress = Address (ScriptCredential proposalValidatorHash) Nothing
                , txOutValue =
                    mconcat
                      [ st
                      , Value.singleton "" "" 10_000_000
                      ]
                , txOutDatumHash = Just (toDatumHash . Datum $ toBuiltinData proposalAfter)
                }
            , TxOut
                { txOutAddress = stakeAddress
                , txOutValue =
                    mconcat
                      [ Value.singleton "" "" 10_000_000
                      , Value.assetClassValue (untag stake.gtClassRef) 50_000_000
                      , Value.assetClassValue stakeAssetClass 1
                      ]
                , txOutDatumHash = Just (toDatumHash stakeDatum)
                }
            ]
        , txInfoFee = Value.singleton "" "" 2
        , txInfoMint = st
        , txInfoDCert = []
        , txInfoWdrl = []
        , txInfoValidRange = validTimeRange
        , txInfoSignatories = newSigners
        , txInfoData =
            [ datumPair . Datum $ toBuiltinData proposalBefore
            , datumPair . Datum $ toBuiltinData proposalAfter
            , datumPair . Datum $ toBuiltinData stakeDatum
            ]
        , txInfoId = "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
        }

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

      proposalInputDatum' :: ProposalDatum
      proposalInputDatum' =
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
      proposalInputDatum :: Datum
      proposalInputDatum = Datum $ toBuiltinData proposalInputDatum'
      proposalInput :: TxOut
      proposalInput =
        TxOut
          { txOutAddress = proposalValidatorAddress
          , txOutValue = pst
          , txOutDatumHash = Just $ toDatumHash proposalInputDatum
          }

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
          { stakedAmount = Tagged params.voteCount
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
                , Value.assetClassValue (untag stake.gtClassRef) params.voteCount
                , minAda
                ]
          , txOutDatumHash = Just $ toDatumHash stakeInputDatum
          }

      ---

      updatedVotes :: AssocMap.Map ResultTag Integer
      updatedVotes = updateMap (Just . (+ params.voteCount)) params.voteFor initialVotes

      ---

      proposalOutputDatum' :: ProposalDatum
      proposalOutputDatum' =
        proposalInputDatum'
          { votes = ProposalVotes updatedVotes
          }
      proposalOutputDatum :: Datum
      proposalOutputDatum = Datum $ toBuiltinData proposalOutputDatum'
      proposalOutput :: TxOut
      proposalOutput =
        proposalInput
          { txOutDatumHash = Just $ toDatumHash proposalOutputDatum
          }

      ---

      -- Off-chain code should do exactly like this: prepend new lock toStatus the list.
      updatedLocks :: [ProposalLock]
      updatedLocks = ProposalLock params.voteFor proposalInputDatum'.proposalId : existingLocks

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

      ---

      validTimeRange =
        closedBoundedInterval
          ((def :: ProposalTimingConfig).draftTime + 1)
          ((def :: ProposalTimingConfig).votingTime - 1)
   in TxInfo
        { txInfoInputs =
            [ TxInInfo proposalRef proposalInput
            , TxInInfo stakeRef stakeInput
            ]
        , txInfoOutputs = [proposalOutput, stakeOutput]
        , txInfoFee = Value.singleton "" "" 2
        , txInfoMint = mempty
        , txInfoDCert = []
        , txInfoWdrl = []
        , txInfoValidRange = validTimeRange
        , txInfoSignatories = [stakeOwner]
        , txInfoData = datumPair <$> [proposalInputDatum, proposalOutputDatum, stakeInputDatum, stakeOutputDatum]
        , txInfoId = "827598fb2d69a896bbd9e645bb14c307df907f422b39eecbe4d6329bc30b428c"
        }

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
  -- | Whether to add an unchanged stake or not.
  Bool ->
  TxInfo
mkTransitionTxInfo from to effects votes startingTime timeRange shouldAddUnchangedStake =
  let pst = Value.singleton proposalPolicySymbol "" 1

      ---

      proposalInputDatum' :: ProposalDatum
      proposalInputDatum' =
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
      proposalInputDatum :: Datum
      proposalInputDatum = Datum $ toBuiltinData proposalInputDatum'
      proposalInput :: TxOut
      proposalInput =
        TxOut
          { txOutAddress = proposalValidatorAddress
          , txOutValue = pst
          , txOutDatumHash = Just $ toDatumHash proposalInputDatum
          }

      ---

      proposalOutputDatum' :: ProposalDatum
      proposalOutputDatum' =
        proposalInputDatum'
          { status = to
          }
      proposalOutputDatum :: Datum
      proposalOutputDatum = Datum $ toBuiltinData proposalOutputDatum'
      proposalOutput :: TxOut
      proposalOutput =
        proposalInput
          { txOutValue = proposalInput.txOutValue <> minAda
          , txOutDatumHash = Just $ toDatumHash proposalOutputDatum
          }

      -- Add a pair of unchanged stake input/output to the txinfo.
      addUnchangedStake :: TxInfo -> TxInfo
      addUnchangedStake input =
        let sst = Value.assetClassValue stakeAssetClass 1

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

            stakeOutput = stakeInput
         in input
              { txInfoInputs = TxInInfo stakeRef stakeInput : input.txInfoInputs
              , txInfoOutputs = stakeOutput : input.txInfoOutputs
              , txInfoData = datumPair stakeInputDatum : input.txInfoData
              }

      template =
        TxInfo
          { txInfoInputs = [TxInInfo proposalRef proposalInput]
          , txInfoOutputs = [proposalOutput]
          , txInfoFee = Value.singleton "" "" 2
          , txInfoMint = mempty
          , txInfoDCert = []
          , txInfoWdrl = []
          , txInfoValidRange = timeRange
          , txInfoSignatories = [signer]
          , txInfoData = datumPair <$> [proposalInputDatum, proposalOutputDatum]
          , txInfoId = "95ba4015e30aef16a3461ea97a779f814aeea6b8009d99a94add4b8293be737a"
          }
   in if shouldAddUnchangedStake
        then addUnchangedStake template
        else template

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

--------------------------------------------------------------------------------

-- | Create empty effects for every result tag given the votes.
emptyEffectFor ::
  ProposalVotes ->
  AssocMap.Map ResultTag (AssocMap.Map ValidatorHash DatumHash)
emptyEffectFor (ProposalVotes vs) =
  AssocMap.fromList $
    map (,AssocMap.empty) (AssocMap.keys vs)

-- | The proposal id shared by all the samples relate to unlocking stake.
unlockStakeProposalId :: ProposalId
unlockStakeProposalId = ProposalId 0

-- | A 'ProposalVotes' that has only two options, serves as a template for unlokcing stake samples.
unlockStakePropsoalVotesTemplate :: ProposalVotes
unlockStakePropsoalVotesTemplate =
  ProposalVotes $
    AssocMap.fromList
      [ (ResultTag 0, 0)
      , (ResultTag 1, 0)
      ]

-- | Create a 'TxInfo' that unlocks a stake from a proposal. For internal use only.
mkUnlockStakeTxInfo ::
  -- | The current state of the proposal.
  ProposalStatus ->
  -- | The votes of the input propsoal
  ProposalVotes ->
  -- | The votes of the output proposal.
  ProposalVotes ->
  -- | Stake amount.
  Integer ->
  -- | Retract from option.
  [ProposalLock] ->
  -- | The locks of output stake.
  [ProposalLock] ->
  TxInfo
mkUnlockStakeTxInfo
  status
  votesBefore
  votesAfter
  stakedAmount
  locksBefore
  locksAfter =
    let stakeOwner = signer

        stakeInputDatum' :: StakeDatum
        stakeInputDatum' =
          StakeDatum
            { stakedAmount = Tagged stakedAmount
            , owner = stakeOwner
            , lockedBy = locksBefore
            }

        stakeOutputDatum' :: StakeDatum
        stakeOutputDatum' =
          stakeInputDatum'
            { lockedBy = locksAfter
            }

        ---

        effects = emptyEffectFor votesBefore

        proposalInputDatum' :: ProposalDatum
        proposalInputDatum' =
          ProposalDatum
            { proposalId = unlockStakeProposalId
            , effects = effects
            , status = status
            , cosigners = [signer]
            , thresholds = def
            , votes = votesBefore
            , timingConfig = def
            , startingTime = ProposalStartingTime 0
            }

        proposalOutputDatum' :: ProposalDatum
        proposalOutputDatum' =
          proposalInputDatum'
            { votes = votesAfter
            }

        ---

        sst = Value.assetClassValue stakeAssetClass 1

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

        stakeOutputDatum :: Datum
        stakeOutputDatum = Datum $ toBuiltinData stakeOutputDatum'
        stakeOutput :: TxOut
        stakeOutput =
          stakeInput
            { txOutDatumHash = Just $ toDatumHash stakeOutputDatum
            }

        ---

        pst = Value.singleton proposalPolicySymbol "" 1

        proposalInputDatum :: Datum
        proposalInputDatum = Datum $ toBuiltinData proposalInputDatum'
        proposalInput :: TxOut
        proposalInput =
          TxOut
            { txOutAddress = proposalValidatorAddress
            , txOutValue = pst
            , txOutDatumHash = Just $ toDatumHash proposalInputDatum
            }

        ---

        proposalOutputDatum :: Datum
        proposalOutputDatum = Datum $ toBuiltinData proposalOutputDatum'
        proposalOutput :: TxOut
        proposalOutput =
          proposalInput
            { txOutValue = proposalInput.txOutValue <> minAda
            , txOutDatumHash = Just $ toDatumHash proposalOutputDatum
            }
     in TxInfo
          { txInfoInputs = [TxInInfo proposalRef proposalInput, TxInInfo stakeRef stakeInput]
          , txInfoOutputs = [proposalOutput, stakeOutput]
          , txInfoFee = Value.singleton "" "" 2
          , txInfoMint = mempty
          , txInfoDCert = []
          , txInfoWdrl = []
          , -- Time doesn't matter int this case.
            txInfoValidRange = closedBoundedInterval 0 100
          , txInfoSignatories = [signer]
          , txInfoData = datumPair <$> [proposalInputDatum, proposalOutputDatum, stakeInputDatum, stakeOutputDatum]
          , txInfoId = "95ba4015e30aef16a3461ea97a779f814aeea6b8009d99a94add4b8293be737a"
          }

-- | How a stake has been used on a particular proposal.
data StakeUsage
  = -- | The stake was spent to vote for a paraticular option.
    VotedFor ResultTag
  | -- | The stake was used to created the proposal.
    Created
  | -- | The stake has nothing to do with the proposal.
    DidNothing

-- | Create a bunch of 'ProposalLock' given the 'StakeUsgae'.
mkStakeLocks :: StakeUsage -> [ProposalLock]
mkStakeLocks (VotedFor rt) = [ProposalLock rt unlockStakeProposalId]
mkStakeLocks Created =
  map (`ProposalLock` unlockStakeProposalId) $
    AssocMap.keys $ getProposalVotes unlockStakePropsoalVotesTemplate
mkStakeLocks _ = []

-- | Assemble the votes of the input propsoal based on 'unlockStakePropsoalVotesTemplate'.
mkVotesBefore ::
  StakeUsage ->
  -- | The staked amount/votes.
  Integer ->
  ProposalVotes
mkVotesBefore (VotedFor rt) vc =
  ProposalVotes $
    updateMap (Just . const vc) rt $
      getProposalVotes unlockStakePropsoalVotesTemplate
mkVotesBefore _ vc = mkVotesBefore (VotedFor $ ResultTag 0) vc

{- | Create a 'TxInfo' that unlocks the stake from the proposal.
     The last parameter controls whether votes should be retracted or not.
-}
unlockStake ::
  -- | The status of both the input and output propsoals.
  ProposalStatus ->
  StakeUsage ->
  -- | Staked amount/vote count.
  Integer ->
  -- | Should we retract votes?
  Bool ->
  TxInfo
unlockStake ps su staked shouldRetract =
  let votesBefore = mkVotesBefore su staked
      votesAfter =
        if shouldRetract
          then unlockStakePropsoalVotesTemplate
          else votesBefore

      locksBefore = mkStakeLocks su
      locksAfter = []
   in mkUnlockStakeTxInfo
        ps
        votesBefore
        votesAfter
        staked
        locksBefore
        locksAfter

{- | Create a 'TxInfo' that unlocks a stake which is used to vote on the proposal.
     Correct count of votes is also retracted. The 'TxInfo' is valid only if the given
      proposal status is 'VotingReady'.
-}
voterUnlockStakeAndRetractVotesWhile :: ProposalStatus -> TxInfo
voterUnlockStakeAndRetractVotesWhile ps =
  unlockStake
    ps
    (VotedFor $ ResultTag 0)
    42
    True

{- | Create a 'TxInfo' that unlocks a stake which is used to vote on the proposal
      without retracting the votes, given the status of the proposal.

     The 'TxInfo' is valid only if the status of the propsoal is either 'Locked'
      or 'Finished'.
-}
voterUnlockStakeWhile :: ProposalStatus -> TxInfo
voterUnlockStakeWhile ps =
  unlockStake
    ps
    (VotedFor $ ResultTag 0)
    42
    False

{- | Create an invalid 'TxInfo' that retracts votes using the stake
      that is used to create the proposal.
-}
creatorRetractVotesWhile :: ProposalStatus -> TxInfo
creatorRetractVotesWhile ps =
  unlockStake
    ps
    Created
    42
    True

{- | Create a 'TxInfo' to unlock the stake that is used to create the propsoal.
      The 'TxInfo' is valid only if the given proposal status is 'Finished'.
-}
creatorUnlockStakeWhile :: ProposalStatus -> TxInfo
creatorUnlockStakeWhile ps =
  unlockStake
    ps
    Created
    42
    False

{- | Create an invalid 'TxInfo' that tries to retract votes and also unlock a stake
      which is not locked by the proposal, given the status of the proposal.
-}
unlockStakeAndRetractVotesUsingIrrelevantStakeWhile :: ProposalStatus -> TxInfo
unlockStakeAndRetractVotesUsingIrrelevantStakeWhile ps =
  unlockStake
    ps
    DidNothing
    42
    True

{- | Create an invalid 'TxInfo' that tries to unlock a stake which is not locked by the proposal,
      given the status of the proposal.
-}
unlockStakeUsingIrrelevantStakeWhile :: ProposalStatus -> TxInfo
unlockStakeUsingIrrelevantStakeWhile ps =
  unlockStake
    ps
    DidNothing
    42
    False
