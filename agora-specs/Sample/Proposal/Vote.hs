module Sample.Proposal.Vote (
  validVoteParameters,
  mkTestTree,
) where

import Agora.Proposal (
  ProposalDatum (..),
  ProposalId (ProposalId),
  ProposalRedeemer (Vote),
  ProposalStatus (VotingReady),
  ProposalVotes (ProposalVotes),
  ResultTag (ResultTag),
 )
import Agora.Proposal.Scripts (proposalValidator)
import Agora.Proposal.Time (
  ProposalStartingTime (ProposalStartingTime),
  ProposalTimingConfig (draftTime, votingTime),
 )
import Agora.Stake (
  ProposalLock (ProposalLock),
  Stake (gtClassRef),
  StakeDatum (..),
  StakeRedeemer (PermitVote),
 )
import Agora.Stake.Scripts (stakeValidator)
import Data.Default (Default (def))
import Data.Tagged (Tagged (Tagged), untag)
import Plutarch.Context (
  BaseBuilder,
  buildTxInfoUnsafe,
  input,
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
  PubKeyHash,
  ScriptContext (..),
  ScriptPurpose (Spending),
  TxInfo,
  TxOutRef (TxOutRef),
 )
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusTx.AssocMap qualified as AssocMap
import Sample.Proposal.Shared (proposalTxRef, stakeTxRef, testFunc)
import Sample.Shared (
  minAda,
  proposalPolicySymbol,
  proposalValidatorHash,
  signer,
  stake,
  stakeAssetClass,
  stakeValidatorHash,
 )
import Sample.Shared qualified as Shared
import Test.Specification (
  SpecificationTree,
  group,
  validatorSucceedsWith,
 )
import Test.Util (closedBoundedInterval, sortValue, updateMap)

proposalRef :: TxOutRef
proposalRef = TxOutRef proposalTxRef 0

stakeRef :: TxOutRef
stakeRef = TxOutRef stakeTxRef 1

-- | Parameters for creating a voting transaction.
data Parameters = Parameters
  { voteFor :: ResultTag
  -- ^ The outcome the transaction is voting for.
  , voteCount :: Integer
  -- ^ The count of votes.
  }

stakeOwner :: PubKeyHash
stakeOwner = signer

initialVotes :: AssocMap.Map ResultTag Integer
initialVotes =
  AssocMap.fromList
    [ (ResultTag 0, 42)
    , (ResultTag 1, 4242)
    ]

proposalInputDatum :: ProposalDatum
proposalInputDatum =
  ProposalDatum
    { proposalId = ProposalId 42
    , effects =
        AssocMap.fromList
          [ (ResultTag 0, AssocMap.empty)
          , (ResultTag 1, AssocMap.empty)
          ]
    , status = VotingReady
    , cosigners = [stakeOwner]
    , thresholds = def
    , votes = ProposalVotes initialVotes
    , timingConfig = def
    , startingTime = ProposalStartingTime 0
    }

existingLocks :: [ProposalLock]
existingLocks =
  [ ProposalLock (ResultTag 0) (ProposalId 0)
  , ProposalLock (ResultTag 2) (ProposalId 1)
  ]

mkStakeInputDatum :: Parameters -> StakeDatum
mkStakeInputDatum params =
  StakeDatum
    { stakedAmount = Tagged params.voteCount
    , owner = stakeOwner
    , lockedBy = existingLocks
    }

mkProposalRedeemer :: Parameters -> ProposalRedeemer
mkProposalRedeemer = Vote . voteFor

mkNewLock :: Parameters -> ProposalLock
mkNewLock ps = ProposalLock ps.voteFor proposalInputDatum.proposalId

mkStakeRedeemer :: Parameters -> StakeRedeemer
mkStakeRedeemer = PermitVote . mkNewLock

-- | Create a valid transaction that votes on a propsal, given the parameters.
vote :: Parameters -> TxInfo
vote params =
  let pst = Value.singleton proposalPolicySymbol "" 1
      sst = Value.assetClassValue stakeAssetClass 1

      ---

      stakeInputDatum = mkStakeInputDatum params

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
            <> Value.assetClassValue (untag stake.gtClassRef) params.voteCount
            <> minAda

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
                . withOutRef proposalRef
          , input $
              script stakeValidatorHash
                . withValue stakeValue
                . withDatum stakeInputDatum
                . withOutRef stakeRef
          , output $
              script proposalValidatorHash
                . withValue pst
                . withDatum proposalOutputDatum
          , output $
              script stakeValidatorHash
                . withValue stakeValue
                . withDatum stakeOutputDatum
          ]
   in buildTxInfoUnsafe builder

---

validVoteParameters :: Parameters
validVoteParameters =
  Parameters
    { voteFor = ResultTag 0
    , voteCount = 27
    }

---

mkTestTree :: String -> Parameters -> Bool -> SpecificationTree
mkTestTree name ps isValid = group name [proposal, stake]
  where
    txInfo = vote ps

    proposal =
      testFunc
        isValid
        "propsoal"
        (proposalValidator Shared.proposal)
        proposalInputDatum
        (mkProposalRedeemer ps)
        ( ScriptContext
            txInfo
            (Spending proposalRef)
        )

    stake =
      let stakeInputDatum = mkStakeInputDatum ps
       in validatorSucceedsWith
            "stake"
            (stakeValidator Shared.stake)
            stakeInputDatum
            (mkStakeRedeemer ps)
            ( ScriptContext
                txInfo
                (Spending stakeRef)
            )
