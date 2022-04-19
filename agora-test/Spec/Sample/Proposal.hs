{- |
Module     : Spec.Sample.Proposal
Maintainer : emi@haskell.fyi
Description: Sample based testing for Proposal utxos

This module tests primarily the happy path for Proposal interactions
-}
module Spec.Sample.Proposal (
  proposal,
  propPolicy,
  propPolicySymbol,
  propThresholds,
  signer,
  signer2,

  -- * Script contexts
  proposalCreation,
  cosignProposal,
) where

--------------------------------------------------------------------------------
import Plutarch.Api.V1 (
  mintingPolicySymbol,
  mkMintingPolicy,
  mkValidator,
  validatorHash,
 )
import Plutus.V1.Ledger.Api (
  Address (Address),
  Credential (ScriptCredential),
  CurrencySymbol,
  Datum (Datum),
  MintingPolicy (..),
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
import Plutus.V1.Ledger.Scripts (Validator, ValidatorHash)
import Plutus.V1.Ledger.Value qualified as Value

--------------------------------------------------------------------------------

import Agora.Governor (
  Governor (Governor),
  GovernorDatum (GovernorDatum, nextProposalId, proposalThresholds),
  governorPolicy,
  governorValidator,
 )
import Agora.Proposal (
  Proposal (..),
  ProposalDatum (..),
  ProposalId (..),
  ProposalStatus (..),
  ProposalThresholds (..),
  ProposalVotes (..),
  ResultTag (..),
  proposalPolicy,
  proposalValidator,
 )
import Agora.Stake (Stake (..), stakePolicy)
import Plutarch.SafeMoney
import Plutus.V1.Ledger.Address (scriptHashAddress)
import PlutusTx.AssocMap qualified as AssocMap
import Spec.Util (datumPair, toDatumHash)

--------------------------------------------------------------------------------

stake :: Stake
stake =
  Stake
    { gtClassRef = Tagged $ Value.assetClass govSymbol ""
    , proposalSTClass = Value.assetClass propPolicySymbol ""
    }

stakeSymbol :: CurrencySymbol
stakeSymbol = mintingPolicySymbol $ mkMintingPolicy $ stakePolicy stake.gtClassRef

governor :: Governor
governor = Governor

govPolicy :: MintingPolicy
govPolicy = mkMintingPolicy (governorPolicy governor)

govValidator :: Validator
govValidator = mkValidator (governorValidator governor)

govSymbol :: CurrencySymbol
govSymbol = mintingPolicySymbol govPolicy

proposal :: Proposal
proposal =
  Proposal
    { governorSTAssetClass =
        -- TODO: if we had a governor here
        Value.assetClass govSymbol ""
    , stakeSTAssetClass =
        Value.assetClass stakeSymbol ""
    }

-- | 'Proposal' policy instance.
propPolicy :: MintingPolicy
propPolicy = mkMintingPolicy (proposalPolicy proposal)

propPolicySymbol :: CurrencySymbol
propPolicySymbol = mintingPolicySymbol propPolicy

-- | A sample 'PubKeyHash'.
signer :: PubKeyHash
signer = "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be7401214142019c"

-- | Another sample 'PubKeyHash'.
signer2 :: PubKeyHash
signer2 = "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be74012141420192"

-- | 'Proposal' validator instance.
propValidator :: Validator
propValidator = mkValidator (proposalValidator proposal)

propValidatorHash :: ValidatorHash
propValidatorHash = validatorHash propValidator

propValidatorAddress :: Address
propValidatorAddress = scriptHashAddress propValidatorHash

propThresholds :: ProposalThresholds
propThresholds =
  ProposalThresholds
    { countVoting = Tagged 1000
    , create = Tagged 1
    , vote = Tagged 10
    }

-- | This script context should be a valid transaction.
proposalCreation :: ScriptContext
proposalCreation =
  let st = Value.singleton propPolicySymbol "" 1 -- Proposal ST
      proposalDatum :: Datum
      proposalDatum =
        Datum
          ( toBuiltinData $
              ProposalDatum
                { proposalId = ProposalId 0
                , effects =
                    AssocMap.fromList
                      [ (ResultTag 0, [])
                      , (ResultTag 1, [])
                      ]
                , status = Draft
                , cosigners = [signer]
                , thresholds = propThresholds
                , votes = ProposalVotes AssocMap.empty
                }
          )

      govBefore :: Datum
      govBefore =
        Datum
          ( toBuiltinData $
              GovernorDatum
                { proposalThresholds = propThresholds
                , nextProposalId = ProposalId 0
                }
          )
      govAfter :: Datum
      govAfter =
        Datum
          ( toBuiltinData $
              GovernorDatum
                { proposalThresholds = propThresholds
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
                      { txOutAddress = Address (ScriptCredential $ validatorHash propValidator) Nothing
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
        , scriptContextPurpose = Minting propPolicySymbol
        }

-- | This script context should be a valid transaction.
cosignProposal :: [PubKeyHash] -> ScriptContext
cosignProposal newSigners =
  let st = Value.singleton propPolicySymbol "" 1 -- Proposal ST
      proposalBefore :: ProposalDatum
      proposalBefore =
        ProposalDatum
          { proposalId = ProposalId 0
          , effects =
              AssocMap.fromList
                [ (ResultTag 0, [])
                , (ResultTag 1, [])
                ]
          , status = Draft
          , cosigners = [signer]
          , thresholds = propThresholds
          , votes = ProposalVotes AssocMap.empty
          }
      proposalAfter :: ProposalDatum
      proposalAfter = proposalBefore {cosigners = newSigners <> proposalBefore.cosigners}
      proposalRef = (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 1)
   in ScriptContext
        { scriptContextTxInfo =
            TxInfo
              { txInfoInputs =
                  [ TxInInfo
                      proposalRef
                      TxOut
                        { txOutAddress = propValidatorAddress
                        , txOutValue =
                            mconcat
                              [ st
                              , Value.singleton "" "" 10_000_000
                              ]
                        , txOutDatumHash = Just (toDatumHash proposalBefore)
                        }
                  ]
              , txInfoOutputs =
                  [ TxOut
                      { txOutAddress = Address (ScriptCredential $ validatorHash propValidator) Nothing
                      , txOutValue =
                          mconcat
                            [ st
                            , Value.singleton "" "" 10_000_000
                            ]
                      , txOutDatumHash = Just (toDatumHash . Datum $ toBuiltinData proposalAfter)
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
                  ]
              , txInfoId = "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be"
              }
        , scriptContextPurpose = Spending proposalRef
        }
