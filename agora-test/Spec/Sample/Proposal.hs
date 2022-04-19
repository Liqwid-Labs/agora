{- |
Module     : Spec.Sample.Proposal
Maintainer : emi@haskell.fyi
Description: Sample based testing for Proposal utxos

This module tests primarily the happy path for Proposal interactions
-}
module Spec.Sample.Proposal (
  proposal,
  policy,
  policySymbol,
  validatorHashTN,
  signer,

  -- * Script contexts
  proposalCreation,
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
  ValidatorHash (ValidatorHash),
 )
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Scripts (Validator)
import Plutus.V1.Ledger.Value (AssetClass (AssetClass), TokenName (TokenName))
import Plutus.V1.Ledger.Value qualified as Value

--------------------------------------------------------------------------------

import Agora.Governor (
  Governor (Governor),
  GovernorDatum (GovernorDatum, nextProposalId, proposalThresholds),
  governorPolicy,
  governorValidator,
 )
import Agora.Proposal
import Plutarch.SafeMoney
import PlutusTx.AssocMap qualified as AssocMap
import Spec.Util (datumPair, toDatumHash)

--------------------------------------------------------------------------------

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
        AssetClass
          ( govSymbol
          , ""
          )
    }

-- | 'Proposal' policy instance.
policy :: MintingPolicy
policy = mkMintingPolicy (proposalPolicy proposal)

policySymbol :: CurrencySymbol
policySymbol = mintingPolicySymbol policy

-- | A sample 'PubKeyHash'.
signer :: PubKeyHash
signer = "8a30896c4fd5e79843e4ca1bd2cdbaa36f8c0bc3be7401214142019c"

-- | 'Proposal' validator instance.
validator :: Validator
validator = mkValidator (proposalValidator proposal)

-- | 'TokenName' that represents the hash of the 'Proposal' validator.
validatorHashTN :: TokenName
validatorHashTN = let ValidatorHash vh = validatorHash validator in TokenName vh

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
  let st = Value.singleton policySymbol "" 1 -- Proposal ST
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
                      { txOutAddress = Address (ScriptCredential $ validatorHash validator) Nothing
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
        , scriptContextPurpose = Minting policySymbol
        }
