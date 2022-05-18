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
) where

--------------------------------------------------------------------------------
import Plutarch.Api.V1 (
  validatorHash,
 )
import Plutus.V1.Ledger.Api (
  Address (Address),
  Credential (ScriptCredential),
  Datum (Datum),
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
  GovernorDatum (GovernorDatum, nextProposalId, proposalThresholds),
 )
import Agora.Proposal (
  Proposal (..),
  ProposalDatum (..),
  ProposalId (..),
  ProposalStatus (..),
  ResultTag (..),
  emptyVotesFor,
 )
import Agora.Stake (Stake (..), StakeDatum (StakeDatum))
import Plutarch.SafeMoney (Tagged (Tagged), untag)
import PlutusTx.AssocMap qualified as AssocMap
import Sample.Shared
import Test.Util (datumPair, toDatumHash)

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
                , timingConfig = proposalTimingConfig
                }
          )

      govBefore :: Datum
      govBefore =
        Datum
          ( toBuiltinData $
              GovernorDatum
                { proposalThresholds = defaultProposalThresholds
                , nextProposalId = ProposalId 0
                }
          )
      govAfter :: Datum
      govAfter =
        Datum
          ( toBuiltinData $
              GovernorDatum
                { proposalThresholds = defaultProposalThresholds
                , nextProposalId = ProposalId 1
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
          , timingConfig = proposalTimingConfig
          }
      stakeDatum :: StakeDatum
      stakeDatum = StakeDatum (Tagged 50_000_000) signer2 []
      proposalAfter :: ProposalDatum
      proposalAfter = proposalBefore {cosigners = newSigners <> proposalBefore.cosigners}
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
        , txInfoValidRange = Interval.always
        , txInfoSignatories = newSigners
        , txInfoData =
            [ datumPair . Datum $ toBuiltinData proposalBefore
            , datumPair . Datum $ toBuiltinData proposalAfter
            , datumPair . Datum $ toBuiltinData stakeDatum
            ]
        , txInfoId = "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
        }
