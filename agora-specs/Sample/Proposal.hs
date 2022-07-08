{- |
Module     : Sample.Proposal
Maintainer : emi@haskell.fyi
Description: Sample based testing for Proposal utxos

This module tests primarily the happy path for Proposal interactions
-}
module Sample.Proposal (
  -- * Script contexts
  proposalCreation,
) where

import Agora.Governor (GovernorDatum (..))
import Agora.Proposal (
  Proposal (..),
  ProposalDatum (..),
  ProposalId (..),
  ProposalStatus (..),
  ResultTag (..),
  emptyVotesFor,
 )
import Data.Default.Class (Default (def))
import Plutarch.Context (
  MintingBuilder,
  buildMintingUnsafe,
  input,
  mint,
  output,
  script,
  signedWith,
  txId,
  withDatum,
  withTxId,
  withValue,
 )
import PlutusLedgerApi.V1 (
  ScriptContext (..),
 )
import PlutusLedgerApi.V1.Value qualified as Value (
  assetClassValue,
  singleton,
 )
import PlutusTx.AssocMap qualified as AssocMap
import Sample.Shared (
  govValidatorHash,
  proposal,
  proposalPolicySymbol,
  proposalStartingTimeFromTimeRange,
  proposalValidatorHash,
  signer,
 )
import Test.Util (
  closedBoundedInterval,
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
          , maximumProposalsPerStake = 3
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
