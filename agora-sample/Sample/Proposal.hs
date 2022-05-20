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
) where

--------------------------------------------------------------------------------
import Plutarch.Api.V1 (
  validatorHash,
 )
import Plutus.V1.Ledger.Api (
  Address (Address),
  Credential (ScriptCredential),
  Datum (Datum),
  POSIXTimeRange,
  PubKeyHash,
  ScriptContext (..),
  ScriptPurpose (..),
  ToData (toBuiltinData),
  TxInInfo (TxInInfo),
  TxInfo (..),
  TxOut (TxOut, txOutAddress, txOutDatumHash, txOutValue),
  TxOutRef (TxOutRef),
 )
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Value qualified as Value

--------------------------------------------------------------------------------

import Agora.Governor (
  GovernorDatum (..),
 )
import Agora.Proposal (
  Proposal (..),
  ProposalDatum (..),
  ProposalId (..),
  ProposalStatus (..),
  ProposalVotes (..),
  ResultTag (..),
  emptyVotesFor,
 )
import Agora.Proposal.Time (ProposalTimingConfig (..))
import Agora.Stake (ProposalLock (ProposalLock), Stake (..), StakeDatum (..))
import Plutarch.SafeMoney (Tagged (Tagged), untag)
import PlutusTx.AssocMap qualified as AssocMap
import Sample.Shared
import Test.Util (closedBoundedInterval, datumPair, toDatumHash, updateMap)

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
                , thresholds = defaultProposalThresholds
                , votes = emptyVotesFor effects
                , timingConfig = defaultProposalTimingConfig
                , startingTime = tmpProposalStartingTime
                }
          )

      govBefore :: Datum
      govBefore =
        Datum
          ( toBuiltinData $
              GovernorDatum
                { proposalThresholds = defaultProposalThresholds
                , nextProposalId = ProposalId 0
                , proposalTimings = defaultProposalTimingConfig
                }
          )
      govAfter :: Datum
      govAfter =
        Datum
          ( toBuiltinData $
              GovernorDatum
                { proposalThresholds = defaultProposalThresholds
                , nextProposalId = ProposalId 1
                , proposalTimings = defaultProposalTimingConfig
                }
          )
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
              , txInfoValidRange = Interval.always
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
          , thresholds = defaultProposalThresholds
          , votes = emptyVotesFor effects
          , timingConfig = defaultProposalTimingConfig
          , startingTime = tmpProposalStartingTime
          }
      stakeDatum :: StakeDatum
      stakeDatum = StakeDatum (Tagged 50_000_000) signer2 []
      proposalAfter :: ProposalDatum
      proposalAfter = proposalBefore {cosigners = newSigners <> proposalBefore.cosigners}
      validTimeRange :: POSIXTimeRange
      validTimeRange =
        closedBoundedInterval
          10
          (defaultProposalTimingConfig.draftTime - 10)
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
          , thresholds = defaultProposalThresholds
          , votes = ProposalVotes initialVotes
          , timingConfig = defaultProposalTimingConfig
          , startingTime = tmpProposalStartingTime
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

      -- Off-chain code should do exactly like this: prepend new lock to the list.
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
        closedBoundedInterval (defaultProposalTimingConfig.draftTime + 1) (defaultProposalTimingConfig.votingTime - 1)
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
