{- |
Module     : Sample.Proposal.Vote
Maintainer : connor@mlabs.city
Description: Generate sample data for testing the functionalities of voting on proposals.
Sample and utilities for testing the functionalities of voting on proposals.
-}
module Sample.Proposal.Vote (
  ParameterBundle (..),
  VoteParameters (..),
  StakeParameters (..),
  StakeInputParameters (..),
  StakeOutputParameters (..),
  NumProposals (..),
  ProposalParameters (..),
  TransactionParameters (..),
  Validity (..),
  vote,
  mkTestTree,
  mkValidOwnerVoteBundle,
  mkValidDelegateeVoteBundle,
  delegateeVoteWithOwnAndDelegatedStakeBundle,
  transparentAssets,
  transactionNotAuthorized,
  voteForNonexistentOutcome,
  noProposal,
  moreThanOneProposals,
  invalidLocks,
  destroyStakes,
  insufficientAmount,
  insufficientAmount1,
) where

import Agora.Governor (Governor (..))
import Agora.Proposal (
  ProposalDatum (..),
  ProposalId (ProposalId),
  ProposalRedeemer (Vote),
  ProposalStatus (VotingReady),
  ProposalThresholds (vote),
  ProposalVotes (ProposalVotes),
  ResultTag (ResultTag),
 )
import Agora.Proposal.Time (
  ProposalStartingTime (ProposalStartingTime),
  ProposalTimingConfig (draftTime, votingTime),
 )
import Agora.SafeMoney (GTTag)
import Agora.Stake (
  ProposalAction (Voted),
  ProposalLock (ProposalLock),
  StakeDatum (..),
  StakeRedeemer (Destroy, PermitVote),
 )
import Data.Default (Default (def))
import Data.Map.Strict qualified as StrictMap
import Data.Maybe (catMaybes)
import Data.Tagged (Tagged, untag)
import Plutarch.Context (
  input,
  mint,
  normalizeValue,
  output,
  script,
  signedWith,
  timeRange,
  withInlineDatum,
  withRedeemer,
  withRef,
  withValue,
 )
import Plutarch.Extra.AssetClass (adaClass, assetClassValue)
import PlutusLedgerApi.V2 (Credential (PubKeyCredential), Interval, POSIXTime, PubKeyHash)
import PlutusLedgerApi.V2.Contexts (TxOutRef (TxOutRef))
import Sample.Proposal.Shared (proposalTxRef)
import Sample.Shared (
  governor,
  minAda,
  proposalAssetClass,
  proposalScriptHash,
  proposalValidator,
  stakeAssetClass,
  stakeScriptHash,
  stakeValidator,
 )
import Test.Specification (SpecificationTree, group, testValidator)
import Test.Util (
  CombinableBuilder,
  closedBoundedInterval,
  mkSpending,
  pubKeyHashes,
 )

data ParameterBundle = ParamerterBundle
  { voteParameters :: VoteParameters
  , stakeParameters :: StakeParameters
  , proposalParameters :: ProposalParameters
  , transactionParameters :: TransactionParameters
  }

newtype VoteParameters = VoteParameters {voteFor :: ResultTag}

data StakeParameters = StakeParameters
  { numStakes :: Integer
  , mixInDelegateeAsOwner :: Bool
  , stakeInputParameters :: StakeInputParameters
  , stakeOutputParameters :: StakeOutputParameters
  }

newtype StakeInputParameters = StakeInputParameters
  { perStakeGTs :: Tagged GTTag Integer
  }

data StakeOutputParameters = StakeOutputParameters
  { burnStakes :: Bool
  , dontAddNewLock :: Bool
  , changeGTAmount :: Bool
  , changeAdaAmount :: Bool
  }

data NumProposals = NoProposal | OneProposal | MoreThanOneProposals

data ProposalParameters = ProposalParameters
  { wrongAddedVotes :: Bool
  , numProposals :: NumProposals
  }

data SignedBy = Owner | Delegatee | Unknown

newtype TransactionParameters = TransactionParameters
  { signedBy :: SignedBy
  }

data Validity = Validity
  { forProposalValidator :: Bool
  , forStakeValidator :: Bool
  }

--------------------------------------------------------------------------------

stakeOwner :: PubKeyHash
stakeOwner = head pubKeyHashes

delegatee :: PubKeyHash
delegatee = pubKeyHashes !! 1

unknownSig :: PubKeyHash
unknownSig = pubKeyHashes !! 2

validTimeRangeLowerBound :: POSIXTime
validTimeRangeLowerBound =
  0
    + (def :: ProposalTimingConfig).draftTime
    + 1

validTimeRangeUpperBound :: POSIXTime
validTimeRangeUpperBound =
  validTimeRangeLowerBound
    + (def :: ProposalTimingConfig).votingTime
    - 2

validTimeRange :: Interval POSIXTime
validTimeRange =
  closedBoundedInterval
    validTimeRangeLowerBound
    validTimeRangeUpperBound

--------------------------------------------------------------------------------

initialVotes :: StrictMap.Map ResultTag Integer
initialVotes =
  StrictMap.fromList
    [ (ResultTag 0, 114)
    , (ResultTag 1, 514)
    ]

proposalInputDatum :: ProposalDatum
proposalInputDatum =
  ProposalDatum
    { proposalId = ProposalId 22
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

mkProposalRedeemer :: VoteParameters -> ProposalRedeemer
mkProposalRedeemer v = Vote v.voteFor

mkProposalRef :: Integer -> TxOutRef
mkProposalRef = TxOutRef proposalTxRef

numProposals :: NumProposals -> Integer
numProposals NoProposal = 0
numProposals OneProposal = 1
numProposals MoreThanOneProposals = 2

--------------------------------------------------------------------------------

mkStakeRedeemer :: StakeOutputParameters -> StakeRedeemer
mkStakeRedeemer params =
  if params.burnStakes
    then Destroy
    else PermitVote

mkStakeInputDatum :: StakeInputParameters -> StakeDatum
mkStakeInputDatum params =
  StakeDatum
    { stakedAmount = params.perStakeGTs
    , owner = PubKeyCredential stakeOwner
    , delegatedTo = Just (PubKeyCredential delegatee)
    , lockedBy =
        [ ProposalLock (ProposalId 0) $ Voted (ResultTag 0) 100
        , ProposalLock (ProposalId 1) $ Voted (ResultTag 2) 200
        ]
    }

mkStakeRef :: Integer -> Integer -> TxOutRef
mkStakeRef o i = TxOutRef proposalTxRef $ o + i

--------------------------------------------------------------------------------

vote :: forall b. CombinableBuilder b => ParameterBundle -> b
vote params =
  let pst = assetClassValue proposalAssetClass 1
      sst = assetClassValue stakeAssetClass 1

      ---

      stakeInputDatum =
        mkStakeInputDatum
          params.stakeParameters.stakeInputParameters

      stakeInputValue =
        normalizeValue $
          sst
            <> assetClassValue
              governor.gtClassRef
              params.stakeParameters.stakeInputParameters.perStakeGTs
            <> minAda

      newLock =
        ProposalLock
          proposalInputDatum.proposalId
          $ Voted
            params.voteParameters.voteFor
            validTimeRangeUpperBound

      updatedLocks =
        if params.stakeParameters.stakeOutputParameters.dontAddNewLock
          then stakeInputDatum.lockedBy
          else newLock : stakeInputDatum.lockedBy

      stakeOutputDatum = stakeInputDatum {lockedBy = updatedLocks}

      stakeOutputValue =
        let changeAmount cond = if cond then (* 100) else id
            gtAmount =
              changeAmount
                params.stakeParameters.stakeOutputParameters.changeGTAmount
                params.stakeParameters.stakeInputParameters.perStakeGTs
            adaAmount =
              changeAmount
                params.stakeParameters.stakeOutputParameters.changeAdaAmount
                10_000_000
         in normalizeValue $
              sst
                <> assetClassValue
                  governor.gtClassRef
                  gtAmount
                <> minAda
                <> assetClassValue adaClass adaAmount

      stakeRedeemer =
        mkStakeRedeemer params.stakeParameters.stakeOutputParameters

      mixOwner i datum =
        if params.stakeParameters.mixInDelegateeAsOwner
          && i == 2
          then
            datum
              { owner = PubKeyCredential delegatee
              , delegatedTo = Nothing
              }
          else datum

      stakeBuilder :: b
      stakeBuilder =
        foldMap
          ( \i ->
              mconcat
                [ input $
                    mconcat
                      [ script stakeScriptHash
                      , withValue stakeInputValue
                      , withInlineDatum $ mixOwner i stakeInputDatum
                      , withRedeemer stakeRedeemer
                      , withRef $ mkStakeRef numProposals' i
                      ]
                , if params.stakeParameters.stakeOutputParameters.burnStakes
                    then mint $ assetClassValue stakeAssetClass (-1)
                    else
                      output $
                        mconcat
                          [ script stakeScriptHash
                          , withValue stakeOutputValue
                          , withInlineDatum $ mixOwner i stakeOutputDatum
                          ]
                ]
          )
          [1 .. params.stakeParameters.numStakes]

      --------------------------------------------------------------------------

      numProposals' = numProposals params.proposalParameters.numProposals

      updatedVotes =
        StrictMap.adjust
          ( ( if params.proposalParameters.wrongAddedVotes
                then (* 10)
                else id
            )
              . ( +
                    untag params.stakeParameters.stakeInputParameters.perStakeGTs
                      * params.stakeParameters.numStakes
                )
          )
          params.voteParameters.voteFor
          initialVotes

      proposalOutputDatum =
        proposalInputDatum
          { votes = ProposalVotes updatedVotes
          }

      proposalRedeemer = mkProposalRedeemer params.voteParameters

      proposalValue =
        normalizeValue $
          pst
            <> minAda

      proposalBuidler :: b
      proposalBuidler =
        foldMap
          ( \i ->
              mconcat
                [ input $
                    mconcat
                      [ script proposalScriptHash
                      , withValue proposalValue
                      , withRedeemer proposalRedeemer
                      , withInlineDatum proposalInputDatum
                      , withRef $ mkProposalRef i
                      ]
                , output $
                    mconcat
                      [ script proposalScriptHash
                      , withValue proposalValue
                      , withInlineDatum proposalOutputDatum
                      ]
                ]
          )
          [1 .. numProposals']

      --------------------------------------------------------------------------

      sig = case params.transactionParameters.signedBy of
        Owner -> stakeOwner
        Delegatee -> delegatee
        Unknown -> unknownSig

      --------------------------------------------------------------------------

      miscBuilder :: b
      miscBuilder =
        mconcat
          [ signedWith sig
          , timeRange validTimeRange
          ]

      --------------------------------------------------------------------------

      builder :: b
      builder =
        mconcat
          [ stakeBuilder
          , proposalBuidler
          , miscBuilder
          ]
   in builder

--------------------------------------------------------------------------------

mkTestTree :: String -> ParameterBundle -> Validity -> SpecificationTree
mkTestTree name ps val = group name $ catMaybes [proposal, stake]
  where
    spend = mkSpending vote ps

    numProposals' = numProposals ps.proposalParameters.numProposals

    proposal =
      case ps.proposalParameters.numProposals of
        NoProposal -> Nothing
        _ ->
          Just $
            testValidator
              val.forProposalValidator
              "proposal"
              proposalValidator
              proposalInputDatum
              (mkProposalRedeemer ps.voteParameters)
              (spend $ mkProposalRef 1)

    stake =
      case ps.stakeParameters.numStakes of
        0 -> error "At least one stake"
        _ ->
          let stakeRef = mkStakeRef numProposals' 1
           in Just $
                testValidator
                  val.forStakeValidator
                  "stake"
                  stakeValidator
                  (mkStakeInputDatum ps.stakeParameters.stakeInputParameters)
                  (mkStakeRedeemer ps.stakeParameters.stakeOutputParameters)
                  (spend stakeRef)

--------------------------------------------------------------------------------

-- TODO(Connor) Use optics

mkValidOwnerVoteBundle :: Integer -> ParameterBundle
mkValidOwnerVoteBundle stakes =
  ParamerterBundle
    { voteParameters =
        VoteParameters
          { voteFor = ResultTag 0
          }
    , stakeParameters =
        StakeParameters
          { numStakes = stakes
          , mixInDelegateeAsOwner = False
          , stakeInputParameters =
              StakeInputParameters
                { perStakeGTs = (def :: ProposalThresholds).vote
                }
          , stakeOutputParameters =
              StakeOutputParameters
                { burnStakes = False
                , dontAddNewLock = False
                , changeGTAmount = False
                , changeAdaAmount = False
                }
          }
    , proposalParameters =
        ProposalParameters
          { wrongAddedVotes = False
          , numProposals = OneProposal
          }
    , transactionParameters =
        TransactionParameters
          { signedBy = Owner
          }
    }

mkValidDelegateeVoteBundle :: Integer -> ParameterBundle
mkValidDelegateeVoteBundle stakes =
  let template = mkValidOwnerVoteBundle stakes
   in template
        { transactionParameters =
            template.transactionParameters
              { signedBy = Delegatee
              }
        }

delegateeVoteWithOwnAndDelegatedStakeBundle :: ParameterBundle
delegateeVoteWithOwnAndDelegatedStakeBundle =
  let template = mkValidDelegateeVoteBundle 5
   in template
        { stakeParameters =
            template.stakeParameters
              { mixInDelegateeAsOwner = True
              }
        }

ownerVoteWithSignleStake :: ParameterBundle
ownerVoteWithSignleStake = mkValidOwnerVoteBundle 1

transparentAssets :: ParameterBundle
transparentAssets =
  ownerVoteWithSignleStake
    { stakeParameters =
        ownerVoteWithSignleStake.stakeParameters
          { stakeOutputParameters =
              ownerVoteWithSignleStake.stakeParameters.stakeOutputParameters
                { changeAdaAmount = True
                }
          }
    }

transactionNotAuthorized :: ParameterBundle
transactionNotAuthorized =
  ownerVoteWithSignleStake
    { transactionParameters =
        ownerVoteWithSignleStake.transactionParameters
          { signedBy = Unknown
          }
    }

voteForNonexistentOutcome :: ParameterBundle
voteForNonexistentOutcome =
  ownerVoteWithSignleStake
    { voteParameters =
        ownerVoteWithSignleStake.voteParameters
          { voteFor = ResultTag 1919810
          }
    }

noProposal :: ParameterBundle
noProposal =
  ownerVoteWithSignleStake
    { proposalParameters =
        ownerVoteWithSignleStake.proposalParameters
          { numProposals = NoProposal
          }
    }

moreThanOneProposals :: ParameterBundle
moreThanOneProposals =
  ownerVoteWithSignleStake
    { proposalParameters =
        ownerVoteWithSignleStake.proposalParameters
          { numProposals = MoreThanOneProposals
          }
    }

ownerVoteWithMultipleStakes :: ParameterBundle
ownerVoteWithMultipleStakes = mkValidOwnerVoteBundle 5

invalidLocks :: ParameterBundle
invalidLocks =
  ownerVoteWithMultipleStakes
    { stakeParameters =
        ownerVoteWithMultipleStakes.stakeParameters
          { stakeOutputParameters =
              ownerVoteWithMultipleStakes.stakeParameters.stakeOutputParameters
                { dontAddNewLock = True
                }
          }
    }

destroyStakes :: ParameterBundle
destroyStakes =
  ownerVoteWithMultipleStakes
    { stakeParameters =
        ownerVoteWithMultipleStakes.stakeParameters
          { stakeOutputParameters =
              ownerVoteWithMultipleStakes.stakeParameters.stakeOutputParameters
                { burnStakes = True
                }
          }
    }

insufficientAmount :: ParameterBundle
insufficientAmount =
  ownerVoteWithSignleStake
    { stakeParameters =
        ownerVoteWithSignleStake.stakeParameters
          { stakeInputParameters =
              ownerVoteWithSignleStake.stakeParameters.stakeInputParameters
                { perStakeGTs = 1
                }
          }
    }

insufficientAmount1 :: ParameterBundle
insufficientAmount1 =
  ownerVoteWithMultipleStakes
    { stakeParameters =
        ownerVoteWithMultipleStakes.stakeParameters
          { stakeInputParameters =
              ownerVoteWithMultipleStakes.stakeParameters.stakeInputParameters
                { perStakeGTs = 1
                }
          }
    }
