{- |
Module     : Sample.Proposal.Vote
Maintainer : connor@mlabs.city
Description: Generate sample data for testing the functionalities of voting on proposals.

Sample and utilities for testing the functionalities of voting on proposals.
-}
module Sample.Proposal.Vote (
  validVoteParameters,
  mkTestTree,
  validVoteAsDelegateParameters,
) where

import Agora.Governor (Governor (..))
import Agora.Proposal (
  ProposalDatum (..),
  ProposalId (ProposalId),
  ProposalRedeemer (Vote),
  ProposalStatus (VotingReady),
  ProposalVotes (ProposalVotes),
  ResultTag (ResultTag),
 )
import Agora.Proposal.Time (
  ProposalStartingTime (ProposalStartingTime),
  ProposalTimingConfig (draftTime, votingTime),
 )
import Agora.Scripts (AgoraScripts (..))
import Agora.Stake (
  ProposalLock (..),
  StakeDatum (..),
  StakeRedeemer (PermitVote),
 )
import Data.Default (Default (def))
import Data.Map.Strict qualified as StrictMap
import Data.Tagged (untag)
import Plutarch.Context (
  input,
  output,
  script,
  signedWith,
  timeRange,
  txId,
  withDatum,
  withRedeemer,
  withRef,
  withValue,
 )
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 (
  Credential (PubKeyCredential),
  PubKeyHash,
  TxOutRef (TxOutRef),
 )
import Sample.Proposal.Shared (proposalTxRef, stakeTxRef)
import Sample.Shared (
  agoraScripts,
  governor,
  minAda,
  proposalPolicySymbol,
  proposalValidatorHash,
  signer,
  stakeAssetClass,
  stakeValidatorHash,
 )
import Test.Specification (
  SpecificationTree,
  group,
  testValidator,
  validatorSucceedsWith,
 )
import Test.Util (
  CombinableBuilder,
  closedBoundedInterval,
  mkSpending,
  pubKeyHashes,
  sortValue,
 )

-- | Reference to the proposal UTXO.
proposalRef :: TxOutRef
proposalRef = TxOutRef proposalTxRef 0

-- | Reference to the stake UTXO.
stakeRef :: TxOutRef
stakeRef = TxOutRef stakeTxRef 1

-- | Parameters for creating a voting transaction.
data Parameters = Parameters
  { voteFor :: ResultTag
  -- ^ The outcome the transaction is voting for.
  , voteCount :: Integer
  -- ^ The count of votes.
  , voteAsDelegate :: Bool
  -- ^ Delegate the stake and use it to vote.
  }

-- | The public key hash of the stake owner.
stakeOwner :: PubKeyHash
stakeOwner = signer

-- | The votes of the input proposals.
initialVotes :: StrictMap.Map ResultTag Integer
initialVotes =
  StrictMap.fromList
    [ (ResultTag 0, 42)
    , (ResultTag 1, 4242)
    ]

-- | The input proposal datum.
proposalInputDatum :: ProposalDatum
proposalInputDatum =
  ProposalDatum
    { proposalId = ProposalId 42
    , effects =
        StrictMap.fromList
          [ (ResultTag 0, StrictMap.empty)
          , (ResultTag 1, StrictMap.empty)
          ]
    , status = VotingReady
    , cosigners = [PubKeyCredential stakeOwner]
    , thresholds = def
    , votes = ProposalVotes initialVotes
    , timingConfig = def
    , startingTime = ProposalStartingTime 0
    }

-- | The locks of the input stake.
existingLocks :: [ProposalLock]
existingLocks =
  [ Voted (ProposalId 0) (ResultTag 0)
  , Voted (ProposalId 1) (ResultTag 2)
  ]

delegate :: PubKeyHash
delegate = head pubKeyHashes

{- | Set the 'StakeDatum.stakedAmount' according to the number of votes being
      casted.
-}
mkStakeInputDatum :: Parameters -> StakeDatum
mkStakeInputDatum params =
  StakeDatum
    { stakedAmount = fromInteger params.voteCount
    , owner = PubKeyCredential stakeOwner
    , delegatedTo =
        if params.voteAsDelegate
          then Just (PubKeyCredential delegate)
          else Nothing
    , lockedBy = existingLocks
    }

-- | Create the proposal redeemer. In this case @'Vote' _@ will always be used.
mkProposalRedeemer :: Parameters -> ProposalRedeemer
mkProposalRedeemer params = Vote params.voteFor

-- | Place new proposal locks on the stake.
mkNewLock :: Parameters -> ProposalLock
mkNewLock params = Voted proposalInputDatum.proposalId params.voteFor

{- | The stake redeemer that is used in 'mkTestTree'. In this case it'll always be
      'PermitVote'.
-}
stakeRedeemer :: StakeRedeemer
stakeRedeemer = PermitVote

-- | Create a valid transaction that votes on a propsal, given the parameters.
vote :: forall b. CombinableBuilder b => Parameters -> b
vote params =
  let pst = Value.singleton proposalPolicySymbol "" 1
      sst = Value.assetClassValue stakeAssetClass 1

      ---

      stakeInputDatum = mkStakeInputDatum params

      ---

      updatedVotes :: StrictMap.Map ResultTag Integer
      updatedVotes = StrictMap.adjust (+ params.voteCount) params.voteFor initialVotes

      ---

      proposalOutputDatum :: ProposalDatum
      proposalOutputDatum =
        proposalInputDatum
          { votes = ProposalVotes updatedVotes
          }

      ---

      -- Off-chain code should do exactly like this: prepend new lock toStatus the list.
      updatedLocks :: [ProposalLock]
      updatedLocks = mkNewLock params : existingLocks

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

      ---

      stakeValue =
        sortValue $
          sst
            <> Value.assetClassValue (untag governor.gtClassRef) params.voteCount
            <> minAda

      signer =
        if params.voteAsDelegate
          then delegate
          else stakeOwner

      builder =
        mconcat
          [ txId "827598fb2d69a896bbd9e645bb14c307df907f422b39eecbe4d6329bc30b428c"
          , signedWith signer
          , timeRange validTimeRange
          , input $
              mconcat
                [ script proposalValidatorHash
                , withValue pst
                , withDatum proposalInputDatum
                , withRef proposalRef
                , withRedeemer $ mkProposalRedeemer params
                ]
          , input $
              mconcat
                [ script stakeValidatorHash
                , withValue stakeValue
                , withDatum stakeInputDatum
                , withRef stakeRef
                ]
          , output $
              mconcat
                [ script proposalValidatorHash
                , withValue pst
                , withDatum proposalOutputDatum
                ]
          , output $
              mconcat
                [ script stakeValidatorHash
                , withValue stakeValue
                , withDatum stakeOutputDatum
                ]
          ]
   in builder

---

-- | Valida parameters that vote on the proposal.
validVoteParameters :: Parameters
validVoteParameters =
  Parameters
    { voteFor = ResultTag 0
    , voteCount = 27
    , voteAsDelegate = False
    }

validVoteAsDelegateParameters :: Parameters
validVoteAsDelegateParameters =
  validVoteParameters
    { voteAsDelegate = True
    }

---

{- | Create a test tree that runs the stake validator and proposal validator to
      test the voting functionalities.
-}
mkTestTree :: String -> Parameters -> Bool -> SpecificationTree
mkTestTree name ps isValid = group name [proposal, stake]
  where
    spend = mkSpending vote ps

    proposal =
      testValidator
        isValid
        "proposal"
        agoraScripts.compiledProposalValidator
        proposalInputDatum
        (mkProposalRedeemer ps)
        (spend proposalRef)

    stake =
      let stakeInputDatum = mkStakeInputDatum ps
       in validatorSucceedsWith
            "stake"
            agoraScripts.compiledStakeValidator
            stakeInputDatum
            stakeRedeemer
            (spend stakeRef)
