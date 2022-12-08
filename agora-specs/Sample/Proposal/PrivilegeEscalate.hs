module Sample.Proposal.PrivilegeEscalate (
  Operation (..),
  privilegeEscalate,
  Validity (..),
  mkTestTree,
) where

import Agora.Proposal (
  ProposalDatum (..),
  ProposalId (ProposalId),
  ProposalRedeemer (UnlockStake, Vote),
  ProposalStatus (VotingReady),
  ProposalVotes (ProposalVotes),
  ResultTag (ResultTag),
  emptyVotesFor,
 )
import Agora.Proposal.Time (
  ProposalStartingTime (ProposalStartingTime),
  ProposalTimingConfig (draftTime, votingTime),
 )
import Agora.SafeMoney (GTTag)
import Agora.Stake (
  ProposalAction (
    Voted
  ),
  ProposalLock (ProposalLock),
  StakeDatum (..),
  StakeRedeemer (PermitVote, RetractVotes),
 )
import Data.Default (Default (def))
import Data.Map.Strict qualified as StrictMap
import Data.Tagged (Tagged, untag)
import Plutarch.Context (
  input,
  normalizeValue,
  output,
  script,
  signedWith,
  timeRange,
  withDatum,
  withRedeemer,
  withRef,
  withValue,
 )
import Plutarch.Extra.AssetClass (assetClassValue)
import PlutusLedgerApi.V1 (Credential (PubKeyCredential))
import PlutusLedgerApi.V2 (PubKeyHash, TxOutRef (TxOutRef))
import Sample.Proposal.Shared (proposalTxRef, stakeTxRef)
import Sample.Shared (
  minAda,
  proposalAssetClass,
  proposalScriptHash,
  proposalValidator,
  stakeAssetClass,
  stakeScriptHash,
  stakeValidator,
 )
import Test.Specification (SpecificationTree, group, testValidator)
import Test.Util (CombinableBuilder, closedBoundedInterval, mkSpending, pubKeyHashes)

data Operation = Voting | RetractingVotes

data Validity = Validity
  { forStakeValidator :: Bool
  , forProposalValidator :: Bool
  }

wrap :: forall x y. Operation -> (x -> x -> y) -> x -> x -> y
wrap Voting = id
wrap RetractingVotes = flip

defStakeAmount :: Tagged GTTag Integer
defStakeAmount = 100000

defResultTag :: ResultTag
defResultTag = ResultTag 0

defProposalId :: ProposalId
defProposalId = ProposalId 0

mkProposalInputOutputDatum :: Operation -> (ProposalDatum, ProposalDatum)
mkProposalInputOutputDatum op =
  let effects = StrictMap.singleton defResultTag StrictMap.empty

      proposal =
        ProposalDatum
          { proposalId = defProposalId
          , effects = effects
          , status = VotingReady
          , cosigners = [] -- doesn't matter
          , thresholds = def
          , votes = emptyVotesFor effects
          , timingConfig = def
          , startingTime = ProposalStartingTime 0
          }

      proposalWithVotes =
        proposal
          { votes =
              ProposalVotes $
                StrictMap.singleton defResultTag (untag defStakeAmount)
          }
   in wrap op (,) proposal proposalWithVotes

mkProposalRedeemer :: Operation -> ProposalRedeemer
mkProposalRedeemer op = wrap op const (Vote defResultTag) UnlockStake

proposalRef :: TxOutRef
proposalRef = TxOutRef proposalTxRef 1

attacker :: PubKeyHash
attacker = head pubKeyHashes

mkStakeInputOutputDatums :: Operation -> ([StakeDatum], [StakeDatum])
mkStakeInputOutputDatums op =
  let delegatee = pubKeyHashes !! 1

      firstStake =
        StakeDatum
          { stakedAmount = defStakeAmount
          , owner = PubKeyCredential attacker
          , delegatedTo = Just $ PubKeyCredential delegatee
          , lockedBy = []
          }

      otherStakes =
        (\pkh -> firstStake {owner = PubKeyCredential pkh})
          <$> drop 2 pubKeyHashes

      allStakes = take 10 $ firstStake : otherStakes

      createdAt = (def :: ProposalTimingConfig).votingTime - 1

      stakeWithLock =
        ( \stake ->
            stake
              { lockedBy =
                  [ ProposalLock defProposalId $
                      Voted
                        defResultTag
                        createdAt
                  ]
              }
        )
          <$> allStakes
   in wrap op (,) allStakes stakeWithLock

mkStakeRedeemer :: Operation -> StakeRedeemer
mkStakeRedeemer op = wrap op const PermitVote RetractVotes

mkStakeRef :: Integer -> TxOutRef
mkStakeRef o = TxOutRef stakeTxRef $ 1 + o

privilegeEscalate :: forall b. CombinableBuilder b => Operation -> b
privilegeEscalate op =
  let sst = assetClassValue stakeAssetClass 1

      stakeValue = normalizeValue $ minAda <> sst

      (stakeInputDatums, stakeOutputDatums) = mkStakeInputOutputDatums op

      stakeBuilder =
        mconcat $
          zipWith3
            ( \index stakeInput stakeOutput ->
                mconcat @b
                  [ input $
                      mconcat
                        [ script stakeScriptHash
                        , withDatum stakeInput
                        , withValue stakeValue
                        , withRef $ mkStakeRef index
                        , withRedeemer $ mkStakeRedeemer op
                        ]
                  , output $
                      mconcat
                        [ script stakeScriptHash
                        , withDatum stakeOutput
                        , withValue stakeValue
                        ]
                  ]
            )
            [1 ..]
            stakeInputDatums
            stakeOutputDatums

      ---

      pst = assetClassValue proposalAssetClass 1

      proposalValue = normalizeValue $ minAda <> pst

      (proposalInput, proposalOutput) = mkProposalInputOutputDatum op

      proposalBuilder =
        mconcat @b
          [ input $
              mconcat
                [ script proposalScriptHash
                , withDatum proposalInput
                , withRedeemer $ mkProposalRedeemer op
                , withValue proposalValue
                , withRef proposalRef
                ]
          , output $
              mconcat
                [ script proposalScriptHash
                , withDatum proposalOutput
                , withValue proposalValue
                ]
          ]

      ---

      validTimeRange =
        closedBoundedInterval
          ((def :: ProposalTimingConfig).draftTime + 1)
          ((def :: ProposalTimingConfig).votingTime - 1)

      miscBuilder =
        mconcat @b
          [ signedWith attacker
          , timeRange validTimeRange
          ]
   in mconcat
        [ miscBuilder
        , stakeBuilder
        , proposalBuilder
        ]

mkTestTree :: String -> Operation -> Validity -> SpecificationTree
mkTestTree name op val = group name [proposal, stake]
  where
    spend = mkSpending privilegeEscalate op

    proposal =
      testValidator
        val.forProposalValidator
        "proposal"
        proposalValidator
        (fst $ mkProposalInputOutputDatum op)
        (mkProposalRedeemer op)
        (spend proposalRef)

    stakeInputdDatum = head $ fst $ mkStakeInputOutputDatums op

    stake =
      testValidator
        val.forStakeValidator
        "stake"
        stakeValidator
        stakeInputdDatum
        (mkStakeRedeemer op)
        (spend $ mkStakeRef 1)
