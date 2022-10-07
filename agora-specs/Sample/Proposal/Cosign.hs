{- |
Module     : Sample.Proposal.Cosign
Maintainer : connor@mlabs.city
Description: Generate sample data for testing the functionalities of cosigning proposals

Sample and utilities for testing the functionalities of cosigning proposals.
-}
module Sample.Proposal.Cosign (
  StakedAmount (..),
  StakeOwner (..),
  StakeParameters (..),
  SignedBy (..),
  TransactionParameters (..),
  ProposalParameters (..),
  ParameterBundle (..),
  Validity (..),
  cosign,
  mkTestTree,
  totallyValid,
  insufficientStakedAmount,
  duplicateCosigners,
  locksNotUpdated,
  cosignersNotUpdated,
  cosignAfterDraft,
) where

import Agora.Governor (Governor (..))
import Agora.Proposal (
  ProposalDatum (..),
  ProposalId (ProposalId),
  ProposalRedeemer (Cosign),
  ProposalStatus (..),
  ProposalThresholds (..),
  ResultTag (ResultTag),
  emptyVotesFor,
 )
import Agora.Proposal.Time (
  ProposalStartingTime (ProposalStartingTime),
  ProposalTimingConfig (draftTime),
 )
import Agora.SafeMoney (GTTag)
import Agora.Scripts (AgoraScripts (..))
import Agora.Stake (
  ProposalLock (Cosigned, Created),
  StakeDatum (..),
  StakeRedeemer (PermitVote),
 )
import Data.Coerce (coerce)
import Data.Default (def)
import Data.List (sort)
import Data.Map.Strict qualified as StrictMap
import Data.Tagged (untag)
import Plutarch.Context (
  input,
  normalizeValue,
  output,
  script,
  signedWith,
  timeRange,
  txId,
  withDatum,
  withInlineDatum,
  withRedeemer,
  withRef,
  withValue,
 )
import Plutarch.SafeMoney (Discrete (Discrete))
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 (
  Credential (PubKeyCredential),
  POSIXTime (POSIXTime),
  PubKeyHash,
  TxOutRef (TxOutRef),
 )
import Sample.Proposal.Shared (proposalTxRef, stakeTxRef)
import Sample.Shared (
  agoraScripts,
  fromDiscrete,
  governor,
  minAda,
  proposalPolicySymbol,
  proposalValidatorHash,
  stakeAssetClass,
  stakeValidatorHash,
 )
import Test.Specification (
  SpecificationTree,
  group,
  testValidator,
 )
import Test.Util (
  CombinableBuilder,
  closedBoundedInterval,
  mkSpending,
  pubKeyHashes,
 )

data StakedAmount = Sufficient | Insufficient

data StakeOwner = Creator | Other

data StakeParameters = StakeParameters
  { gtAmount :: StakedAmount
  , stakeOwner :: StakeOwner
  , dontUpdateLocks :: Bool
  }

data SignedBy = Owner | Delegatee | Unknown

newtype TransactionParameters = TransactionParameters
  { signedBy :: SignedBy
  }

data ProposalParameters = ProposalParameters
  { proposalStatus :: ProposalStatus
  , dontUpdateCosigners :: Bool
  }

-- | Parameters for cosigning a proposal.
data ParameterBundle = ParameterBundle
  { stakeParameters :: StakeParameters
  , proposalParameters :: ProposalParameters
  , transactionParameters :: TransactionParameters
  }

data Validity = Validity
  { forProposalValidator :: Bool
  , forStakeValidator :: Bool
  }

--------------------------------------------------------------------------------

mkStakeAmount :: StakedAmount -> Discrete GTTag
mkStakeAmount Sufficient = Discrete $ (def @ProposalThresholds).cosign
mkStakeAmount Insufficient = mkStakeAmount Sufficient - 1

mkStakeOwner :: StakeOwner -> PubKeyHash
mkStakeOwner Creator = creator
mkStakeOwner Other = pubKeyHashes !! 2

mkSigner :: StakeOwner -> SignedBy -> PubKeyHash
mkSigner so Owner = mkStakeOwner so
mkSigner _ Delegatee = delegatee
mkSigner _ Unknown = pubKeyHashes !! 4

creator :: PubKeyHash
creator = pubKeyHashes !! 1

delegatee :: PubKeyHash
delegatee = pubKeyHashes !! 3

--------------------------------------------------------------------------------

defProposalId :: ProposalId
defProposalId = ProposalId 0

mkProposalInputDatum :: ParameterBundle -> ProposalDatum
mkProposalInputDatum ps =
  let effects =
        StrictMap.fromList
          [ (ResultTag 0, StrictMap.empty)
          , (ResultTag 1, StrictMap.empty)
          ]
   in ProposalDatum
        { proposalId = ProposalId 0
        , effects = effects
        , status = ps.proposalParameters.proposalStatus
        , cosigners = [PubKeyCredential creator]
        , thresholds = def
        , votes = emptyVotesFor effects
        , timingConfig = def
        , startingTime = ProposalStartingTime 0
        }

mkProposalOutputDatum :: ParameterBundle -> ProposalDatum
mkProposalOutputDatum ps =
  let inputDatum = mkProposalInputDatum ps
      stakeOwner =
        PubKeyCredential $
          mkStakeOwner ps.stakeParameters.stakeOwner
      newCosigners =
        if ps.proposalParameters.dontUpdateCosigners
          then inputDatum.cosigners
          else sort $ stakeOwner : inputDatum.cosigners
   in inputDatum {cosigners = newCosigners}

proposalRedeemer :: ProposalRedeemer
proposalRedeemer = Cosign

proposalRef :: TxOutRef
proposalRef = TxOutRef proposalTxRef 1

--------------------------------------------------------------------------------

mkStakeInputDatum :: ParameterBundle -> StakeDatum
mkStakeInputDatum ps =
  let sps = ps.stakeParameters
      amount = mkStakeAmount sps.gtAmount
      owner = mkStakeOwner sps.stakeOwner
      locks = case sps.stakeOwner of
        Creator -> [Created defProposalId]
        _ -> []
   in StakeDatum
        { stakedAmount = amount
        , owner = PubKeyCredential owner
        , delegatedTo = Just $ PubKeyCredential delegatee
        , lockedBy = locks
        }

mkStakeOuputDatum :: ParameterBundle -> StakeDatum
mkStakeOuputDatum ps =
  let sps = ps.stakeParameters
      inpDatum = mkStakeInputDatum ps
      locks =
        if sps.dontUpdateLocks
          then inpDatum.lockedBy
          else Cosigned defProposalId : inpDatum.lockedBy
   in inpDatum {lockedBy = locks}

stakeRedeemer :: StakeRedeemer
stakeRedeemer = PermitVote

stakeRef :: TxOutRef
stakeRef = TxOutRef stakeTxRef 0

--------------------------------------------------------------------------------

-- | Create a 'TxInfo' that tries to cosign a proposal with new cosigners.
cosign :: forall b. CombinableBuilder b => ParameterBundle -> b
cosign ps = builder
  where
    pst = Value.singleton proposalPolicySymbol "" 1
    sst = Value.assetClassValue stakeAssetClass 1

    ----------------------------------------------------------------------------

    stakeInputDatum = mkStakeInputDatum ps
    stakeOutputDatum = mkStakeOuputDatum ps

    stakeValue =
      normalizeValue $
        minAda
          <> Value.assetClassValue
            (untag governor.gtClassRef)
            ( fromDiscrete $
                mkStakeAmount ps.stakeParameters.gtAmount
            )
          <> sst

    stakeBuilder =
      mconcat
        [ input $
            mconcat
              [ script stakeValidatorHash
              , withValue stakeValue
              , withInlineDatum stakeInputDatum
              , withRef stakeRef
              , withRedeemer stakeRedeemer
              ]
        , output $
            mconcat
              [ script stakeValidatorHash
              , withValue stakeValue
              , withInlineDatum stakeOutputDatum
              ]
        ]

    ----------------------------------------------------------------------------

    proposalInputDatum = mkProposalInputDatum ps
    proposalOutputDatum = mkProposalOutputDatum ps

    proposalValue =
      normalizeValue $
        pst <> minAda

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

    ----------------------------------------------------------------------------

    validTimeRange =
      closedBoundedInterval
        (coerce proposalInputDatum.startingTime + 1)
        ( coerce proposalInputDatum.startingTime
            + proposalInputDatum.timingConfig.draftTime - 1
        )

    sig =
      mkSigner
        ps.stakeParameters.stakeOwner
        ps.transactionParameters.signedBy

    ----------------------------------------------------------------------------

    builder =
      mconcat
        [ txId "05c67819fc3381a2052b929ab439244b7b5fe3b3bd07f2134055bbbb21bd9e52"
        , timeRange validTimeRange
        , proposalBuilder
        , stakeBuilder
        , signedWith sig
        ]

--------------------------------------------------------------------------------

mkTestTree ::
  String ->
  ParameterBundle ->
  Validity ->
  SpecificationTree
mkTestTree name ps val =
  group name [proposal, stake]
  where
    spend = mkSpending cosign ps

    proposal =
      testValidator
        val.forProposalValidator
        "proposal"
        agoraScripts.compiledProposalValidator
        (mkProposalInputDatum ps)
        proposalRedeemer
        (spend proposalRef)

    stake =
      testValidator
        val.forStakeValidator
        "stake"
        agoraScripts.compiledStakeValidator
        (mkStakeInputDatum ps)
        stakeRedeemer
        (spend stakeRef)

--------------------------------------------------------------------------------

totallyValid :: ParameterBundle
totallyValid =
  ParameterBundle
    { stakeParameters =
        StakeParameters
          { gtAmount = Sufficient
          , stakeOwner = Other
          , dontUpdateLocks = False
          }
    , proposalParameters =
        ProposalParameters
          { proposalStatus = Draft
          , dontUpdateCosigners = False
          }
    , transactionParameters =
        TransactionParameters
          { signedBy =
              Owner
          }
    }

insufficientStakedAmount :: ParameterBundle
insufficientStakedAmount =
  totallyValid
    { stakeParameters =
        totallyValid.stakeParameters
          { gtAmount = Insufficient
          }
    }

locksNotUpdated :: ParameterBundle
locksNotUpdated =
  totallyValid
    { stakeParameters =
        totallyValid.stakeParameters
          { dontUpdateLocks = True
          }
    }

duplicateCosigners :: ParameterBundle
duplicateCosigners =
  totallyValid
    { stakeParameters =
        totallyValid.stakeParameters
          { stakeOwner = Creator
          }
    }

cosignersNotUpdated :: ParameterBundle
cosignersNotUpdated =
  totallyValid
    { proposalParameters =
        totallyValid.proposalParameters
          { dontUpdateCosigners = True
          }
    }

cosignAfterDraft :: [ParameterBundle]
cosignAfterDraft =
  map
    ( \s ->
        totallyValid
          { proposalParameters =
              totallyValid.proposalParameters
                { proposalStatus = s
                }
          }
    )
    [VotingReady, Locked, Finished]
