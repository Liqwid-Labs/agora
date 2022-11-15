{- |
Module     : Sample.Proposal.UnlockStake
Maintainer : connor@mlabs.city
Description: Generate sample data for testing the functionalities of unlocking stake and retracting votes

Sample and utilities for testing the functionalities of unlocking stake and retracting votes
-}
module Sample.Proposal.Unlock (
  ParameterBundle (..),
  StakeRole (..),
  TimeRange (..),
  SignedBy (..),
  TransactionParameters (..),
  ProposalParameters (..),
  SSTOwner (..),
  StakeParameters (..),
  Validity (..),
  unlock,
  mkTestTree,
  mkValidVoterRetractVotes,
  mkValidDelegateeRetractVotes,
  mkValidVoterCreatorRetractVotes,
  mkValidCreatorRemoveLock,
  mkValidVoterRemoveLockAfterVoting,
  mkRetractVotesWhileNotVoting,
  mkUnockIrrelevantStakes,
  mkRemoveCreatorLockBeforeFinished,
  mkCreatorRetractVotes,
  mkChangeOutputStakeValue,
  mkUseFakeStakes,
) where

--------------------------------------------------------------------------------

import Agora.Governor (Governor (..))
import Agora.Proposal (
  ProposalDatum (..),
  ProposalEffectGroup,
  ProposalId (..),
  ProposalRedeemer (UnlockStake),
  ProposalStatus (..),
  ProposalVotes (..),
  ResultTag (..),
 )
import Agora.Proposal.Time (ProposalStartingTime (ProposalStartingTime), ProposalTimingConfig (..))
import Agora.SafeMoney (GTTag)
import Agora.Stake (
  ProposalLock (..),
  StakeDatum (..),
  StakeRedeemer (RetractVotes),
 )
import Data.Default.Class (Default (def))
import Data.Map.Strict qualified as StrictMap
import Data.Tagged (Tagged, untag)
import Plutarch.Context (
  input,
  normalizeValue,
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
import Plutarch.Extra.AssetClass (assetClassValue)
import Plutarch.Extra.ScriptContext (validatorHashToTokenName)
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 (
  Credential (PubKeyCredential),
  PubKeyHash,
  TxOutRef (..),
 )
import Sample.Proposal.Shared (stakeTxRef)
import Sample.Shared (
  governor,
  minAda,
  proposalAssetClass,
  proposalValidator,
  proposalValidatorHash,
  stakeSymbol,
  stakeValidator,
  stakeValidatorHash,
 )
import Test.Specification (SpecificationTree, group, testValidator)
import Test.Util (CombinableBuilder, closedBoundedInterval, mkSpending, pubKeyHashes)

--------------------------------------------------------------------------------

votesTemplate :: ProposalVotes
votesTemplate =
  ProposalVotes $
    StrictMap.fromList
      [ (ResultTag 0, 0)
      , (ResultTag 1, 0)
      ]

-- | Create empty effects for every result tag given the votes.
emptyEffectFor ::
  ProposalVotes ->
  StrictMap.Map ResultTag ProposalEffectGroup
emptyEffectFor (ProposalVotes vs) =
  StrictMap.fromList $
    map (,StrictMap.empty) (StrictMap.keys vs)

-- | The default vote option that will be used by functions in this module.
defVoteFor :: ResultTag
defVoteFor = ResultTag 0

-- | The default number of GTs the stake will have.
defStakedGTs :: Tagged GTTag Integer
defStakedGTs = 100000

alteredStakedGTs :: Tagged GTTag Integer
alteredStakedGTs = 100

-- | Default owner of the stakes.
defOwner :: PubKeyHash
defOwner = pubKeyHashes !! 1

defDelegatee :: PubKeyHash
defDelegatee = pubKeyHashes !! 2

defUnknown :: PubKeyHash
defUnknown = pubKeyHashes !! 3

defProposalId :: ProposalId
defProposalId = ProposalId 0

defStartingTime :: ProposalStartingTime
defStartingTime = ProposalStartingTime 0

--------------------------------------------------------------------------------

data ParameterBundle = ParameterBundle
  { proposalParameters :: ProposalParameters
  , stakeParameters :: StakeParameters
  , transactionParameters :: TransactionParameters
  }

data SignedBy = Owner | Delegatee | Unknown

data TimeRange = WhileVoting | AfterVoting

data TransactionParameters = TransactionParameters
  { signedBy :: SignedBy
  , timeRange :: TimeRange
  }

data ProposalParameters = ProposalParameters
  { proposalStatus :: ProposalStatus
  , retractVotes :: Bool
  }

-- | How a stake has been used on a particular proposal.
data StakeRole
  = -- | The stake was spent to vote for a paraticular option.
    Voter
  | -- | The stake was used to create the proposal.
    Creator
  | -- | The stake was used to both create and vote for the proposal.
    Both
  | -- | The stake has nothing to do with the proposal.
    Irrelevant
  deriving stock (Bounded, Enum, Show)

data SSTOwner
  = StakeValidator
  | Attacker

data StakeParameters = StakeParameters
  { numStakes :: Integer
  , stakeRole :: StakeRole
  , removeVoterLock :: Bool
  , removeCreatorLock :: Bool
  , alterOutputValue :: Bool
  , sstOwner :: SSTOwner
  }

data Validity = Validity
  { forProposalValidator :: Bool
  , forStakeValidator :: Bool
  }

--------------------------------------------------------------------------------

mkStakeRef :: Integer -> TxOutRef
mkStakeRef = TxOutRef stakeTxRef

stakeRedeemer :: StakeRedeemer
stakeRedeemer = RetractVotes

mkStakeInputDatum :: StakeParameters -> StakeDatum
mkStakeInputDatum ps =
  StakeDatum
    { stakedAmount = defStakedGTs
    , owner = PubKeyCredential defOwner
    , delegatedTo = Just $ PubKeyCredential defDelegatee
    , lockedBy = stakeLocks
    }
  where
    stakeLocks = mkStakeLocks' ps.stakeRole

    mkStakeLocks' Voter = [Voted defProposalId defVoteFor]
    mkStakeLocks' Creator = [Created defProposalId]
    mkStakeLocks' Both = mkStakeLocks' Voter <> mkStakeLocks' Creator
    mkStakeLocks' Irrelevant =
      let ProposalId pid = defProposalId
          ResultTag vid = defVoteFor
       in [ Voted (ProposalId $ pid + 1) (ResultTag $ vid + 1)
          , Created (ProposalId $ pid + 1)
          ]

--------------------------------------------------------------------------------

proposalRef :: TxOutRef
proposalRef = TxOutRef stakeTxRef 0

proposalRedeemer :: ProposalRedeemer
proposalRedeemer = UnlockStake

mkProposalInputDatum ::
  StakeParameters ->
  ProposalParameters ->
  ProposalDatum
mkProposalInputDatum sps pps =
  ProposalDatum
    { proposalId = defProposalId
    , effects = emptyEffectFor votesTemplate
    , status = pps.proposalStatus
    , cosigners = [PubKeyCredential $ head pubKeyHashes]
    , thresholds = def
    , votes = updatVotes votesTemplate
    , timingConfig = def
    , startingTime = defStartingTime
    }
  where
    updatVotes (ProposalVotes vt) =
      ProposalVotes $
        StrictMap.adjust
          (+ sps.numStakes * untag defStakedGTs)
          defVoteFor
          vt

--------------------------------------------------------------------------------

unlock :: forall b. CombinableBuilder b => ParameterBundle -> b
unlock ps = builder
  where
    pst = assetClassValue proposalAssetClass 1

    proposalInputDatum =
      mkProposalInputDatum
        ps.stakeParameters
        ps.proposalParameters

    proposalOutputDatum =
      if ps.proposalParameters.retractVotes
        then proposalInputDatum {votes = votesTemplate}
        else proposalInputDatum

    proposalValue = normalizeValue $ pst <> minAda

    proposalBuilder :: b
    proposalBuilder =
      mconcat
        [ input $
            mconcat
              [ script proposalValidatorHash
              , withValue proposalValue
              , withDatum proposalInputDatum
              , withRef proposalRef
              , withRedeemer proposalRedeemer
              ]
        , output $
            mconcat
              [ script proposalValidatorHash
              , withValue proposalValue
              , withDatum proposalOutputDatum
              ]
        ]

    ---

    sstName = case ps.stakeParameters.sstOwner of
      StakeValidator -> validatorHashToTokenName stakeValidatorHash
      _ -> ""

    sst = Value.singleton stakeSymbol sstName 1

    stakeInputDatum = mkStakeInputDatum ps.stakeParameters

    removeLocks v c =
      filter $
        not
          . ( \case
                Created pid -> c && pid == defProposalId
                Cosigned pid -> c && pid == defProposalId
                Voted pid _ -> v && pid == defProposalId
            )

    stakeOutputDatum =
      stakeInputDatum
        { lockedBy =
            removeLocks
              ps.stakeParameters.removeVoterLock
              ps.stakeParameters.removeCreatorLock
              stakeInputDatum.lockedBy
        }

    mkStakeValue gt =
      normalizeValue $
        mconcat
          [ minAda
          , sst
          , assetClassValue
              governor.gtClassRef
              gt
          ]

    stakeInputValue = mkStakeValue defStakedGTs

    stakeOutputValue =
      mkStakeValue $
        if ps.stakeParameters.alterOutputValue
          then alteredStakedGTs
          else defStakedGTs

    stakeBuilder :: b
    stakeBuilder =
      foldMap
        ( \i ->
            mconcat
              [ input $
                  mconcat
                    [ script stakeValidatorHash
                    , withValue stakeInputValue
                    , withDatum stakeInputDatum
                    , withRef $ mkStakeRef i
                    ]
              , output $
                  mconcat
                    [ script stakeValidatorHash
                    , withValue stakeOutputValue
                    , withDatum stakeOutputDatum
                    ]
              ]
        )
        [1 .. ps.stakeParameters.numStakes]

    ---

    ProposalStartingTime s = defStartingTime

    time = case ps.transactionParameters.timeRange of
      WhileVoting ->
        let lb = s + (def :: ProposalTimingConfig).draftTime
            ub = lb + (def :: ProposalTimingConfig).votingTime
         in closedBoundedInterval (lb + 1) (ub - 1)
      AfterVoting ->
        let lb =
              s
                + (def :: ProposalTimingConfig).draftTime
                + (def :: ProposalTimingConfig).votingTime
            ub = lb + (def :: ProposalTimingConfig).lockingTime
         in closedBoundedInterval (lb + 1) (ub - 1)

    sig = case ps.transactionParameters.signedBy of
      Unknown -> defUnknown
      Owner -> defOwner
      Delegatee -> defDelegatee

    ---

    builder =
      mconcat
        [ txId "388bc0b897b3dadcd479da4c88291de4113a50b72ddbed001faf7fc03f11bc52"
        , proposalBuilder
        , stakeBuilder
        , signedWith sig
        , timeRange time
        ]

--------------------------------------------------------------------------------

{- | Create a test tree that runs both the stake validator and the proposal
     validator.
-}
mkTestTree :: String -> ParameterBundle -> Validity -> SpecificationTree
mkTestTree name ps val = group name [stake, proposal]
  where
    spend = mkSpending unlock ps

    stake =
      testValidator
        val.forStakeValidator
        "stake"
        stakeValidator
        (mkStakeInputDatum ps.stakeParameters)
        stakeRedeemer
        (spend $ mkStakeRef 1)

    proposal =
      testValidator
        val.forProposalValidator
        "proposal"
        proposalValidator
        (mkProposalInputDatum ps.stakeParameters ps.proposalParameters)
        proposalRedeemer
        (spend proposalRef)

--------------------------------------------------------------------------------

mkValidVoterRetractVotes :: Integer -> ParameterBundle
mkValidVoterRetractVotes i =
  ParameterBundle
    { proposalParameters =
        ProposalParameters
          { proposalStatus = VotingReady
          , retractVotes = True
          }
    , stakeParameters =
        StakeParameters
          { numStakes = i
          , stakeRole = Voter
          , removeVoterLock = True
          , removeCreatorLock = False
          , alterOutputValue = False
          , sstOwner = StakeValidator
          }
    , transactionParameters =
        TransactionParameters
          { signedBy = Owner
          , timeRange =
              WhileVoting
          }
    }

mkValidDelegateeRetractVotes :: Integer -> ParameterBundle
mkValidDelegateeRetractVotes i =
  let template = mkValidVoterRetractVotes i
   in template
        { transactionParameters =
            template.transactionParameters
              { signedBy = Delegatee
              }
        }

mkValidVoterCreatorRetractVotes :: Integer -> ParameterBundle
mkValidVoterCreatorRetractVotes i =
  let template = mkValidVoterRetractVotes i
   in template
        { stakeParameters =
            template.stakeParameters
              { stakeRole = Both
              }
        }

mkValidCreatorRemoveLock :: Integer -> ParameterBundle
mkValidCreatorRemoveLock i =
  let template = mkValidVoterRetractVotes i
   in template
        { proposalParameters =
            template.proposalParameters
              { proposalStatus = Finished
              , retractVotes = False
              }
        , stakeParameters =
            template.stakeParameters
              { stakeRole = Creator
              , removeCreatorLock = True
              }
        , transactionParameters =
            template.transactionParameters
              { timeRange = AfterVoting
              }
        }

mkValidVoterRemoveLockAfterVoting :: Integer -> ParameterBundle
mkValidVoterRemoveLockAfterVoting i =
  let template = mkValidVoterRetractVotes i
   in template
        { proposalParameters =
            template.proposalParameters
              { proposalStatus = Finished
              , retractVotes = False
              }
        , transactionParameters =
            template.transactionParameters
              { timeRange = AfterVoting
              }
        }

mkRetractVotesWhileNotVoting :: Integer -> [ParameterBundle]
mkRetractVotesWhileNotVoting i =
  let template = mkValidVoterRetractVotes i
   in map
        ( \s ->
            template
              { proposalParameters =
                  template.proposalParameters
                    { proposalStatus = s
                    }
              }
        )
        [Draft, Locked, Finished]

mkUnockIrrelevantStakes :: Integer -> ParameterBundle
mkUnockIrrelevantStakes i =
  let template = mkValidVoterRetractVotes i
   in template
        { stakeParameters =
            template.stakeParameters
              { stakeRole = Irrelevant
              , removeCreatorLock = True
              }
        }

mkRemoveCreatorLockBeforeFinished :: Integer -> [ParameterBundle]
mkRemoveCreatorLockBeforeFinished i =
  let template = mkValidCreatorRemoveLock i
   in map
        ( \s ->
            template
              { proposalParameters =
                  template.proposalParameters
                    { proposalStatus = s
                    }
              }
        )
        [Draft, VotingReady, Locked]

mkCreatorRetractVotes :: Integer -> ParameterBundle
mkCreatorRetractVotes i =
  let template = mkValidVoterRetractVotes i
   in template
        { proposalParameters =
            template.proposalParameters
              { proposalStatus = VotingReady
              }
        , stakeParameters =
            template.stakeParameters
              { stakeRole = Creator
              }
        , transactionParameters =
            template.transactionParameters
              { timeRange = WhileVoting
              }
        }

mkChangeOutputStakeValue :: Integer -> ParameterBundle
mkChangeOutputStakeValue i =
  let template = mkValidVoterRetractVotes i
   in template
        { stakeParameters =
            template.stakeParameters
              { alterOutputValue = True
              }
        }

mkUseFakeStakes :: Integer -> ParameterBundle
mkUseFakeStakes i =
  let template = mkValidVoterCreatorRetractVotes i
   in template
        { stakeParameters =
            template.stakeParameters
              { sstOwner = Attacker
              }
        }
