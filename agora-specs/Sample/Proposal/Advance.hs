{- |
Module     : Sample.Proposal.Advance
Maintainer : connor@mlabs.city
Description: Generate sample data for testing the functionalities of advancing proposals

Sample and utilities for testing the functionalities of advancing proposals.
-}
module Sample.Proposal.Advance (
  advanceToNextStateInTimeParameters,
  advanceToFailedStateDueToTimeoutParameters,
  insufficientVotesParameters,
  insufficientCosignsParameters,
  advanceFromFinishedParameters,
  invalidOutputStakeParameters,
  mkTestTree,
  Parameters (..),
) where

import Agora.Proposal (
  ProposalDatum (..),
  ProposalId (ProposalId),
  ProposalRedeemer (AdvanceProposal),
  ProposalStatus (..),
  ProposalThresholds (..),
  ProposalVotes (ProposalVotes),
  ResultTag (ResultTag),
  emptyVotesFor,
 )
import Agora.Proposal.Scripts (proposalValidator)
import Agora.Proposal.Time (
  ProposalStartingTime (ProposalStartingTime),
  ProposalTimingConfig (
    draftTime,
    executingTime,
    lockingTime,
    votingTime
  ),
 )
import Agora.SafeMoney (GTTag)
import Agora.Stake (
  ProposalLock (..),
  Stake (gtClassRef),
  StakeDatum (..),
  StakeRedeemer (WitnessStake),
 )
import Agora.Stake.Scripts (stakeValidator)
import Data.Coerce (coerce)
import Data.Default (def)
import Data.List (sort)
import Data.Tagged (Tagged (..), untag)
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
  withTxId,
  withValue,
 )
import PlutusLedgerApi.V1 (
  DatumHash,
  POSIXTime,
  POSIXTimeRange,
  PubKeyHash,
  ScriptContext (ScriptContext),
  ScriptPurpose (Spending),
  TxInfo,
  TxOutRef (TxOutRef),
  ValidatorHash,
  always,
 )
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusTx.AssocMap qualified as AssocMap
import Sample.Proposal.Shared (proposalTxRef, stakeTxRef)
import Sample.Shared (
  minAda,
  proposalPolicySymbol,
  proposalValidatorHash,
  stake,
  stakeAssetClass,
  stakeValidatorHash,
 )
import Sample.Shared qualified as Shared
import Test.Specification (SpecificationTree, group, testValidator)
import Test.Util (closedBoundedInterval, pubKeyHashes, sortValue, updateMap)

-- | Parameters for state transition of proposals.
data Parameters = Parameters
  { fromStatus :: ProposalStatus
  -- ^ Initial state of the proposal.
  , toStatus :: ProposalStatus
  -- ^ Next state of the proposal.
  , votes :: ProposalVotes
  -- ^ Votes.
  , includeAllStakes :: Bool
  -- ^ Whether to add an extra cosigner without stake or not.
  , validTimeRange :: POSIXTimeRange
  -- ^ Valid time range of the transaction.
  , alterOutputStakes :: Bool
  -- ^ Whether to alter th output stakes or not.
  , stakeCount :: Integer
  -- ^ The number of stakes.
  , signByAllCosigners :: Bool
  -- ^ Whether the transaction is signed by all the cosigners.
  , perStakeGTs :: Tagged GTTag Integer
  -- ^ The staked amount of each stake.
  }

---

-- | Reference to the proposal UTXO.
proposalRef :: TxOutRef
proposalRef = TxOutRef proposalTxRef 1

-- | Create the reference to a particular stake UTXO.
mkStakeRef :: Int -> TxOutRef
mkStakeRef = TxOutRef stakeTxRef . (+ 2) . fromIntegral

---

-- | Default effects of the propsoal.
defEffects :: AssocMap.Map ResultTag (AssocMap.Map ValidatorHash DatumHash)
defEffects =
  AssocMap.fromList
    [ (ResultTag 0, AssocMap.empty)
    , (ResultTag 1, AssocMap.empty)
    ]

-- | Empty votes for the default effects.
emptyVotes :: ProposalVotes
emptyVotes = emptyVotesFor defEffects

{- | The default proposal statring time, which doesn't really matter in this
     case.
-}
proposalStartingTime :: POSIXTime
proposalStartingTime = 0

---

-- | Create the input proposal datum given the parameters.
mkProposalInputDatum :: Parameters -> ProposalDatum
mkProposalInputDatum ps =
  ProposalDatum
    { proposalId = ProposalId 0
    , effects = defEffects
    , status = ps.fromStatus
    , cosigners = mkStakeOwners ps
    , thresholds = def
    , votes = ps.votes
    , timingConfig = def
    , startingTime = ProposalStartingTime proposalStartingTime
    }

-- | Create the input stake datums given the parameters.
mkStakeInputDatums :: Parameters -> [StakeDatum]
mkStakeInputDatums ps =
  map
    ( \pk ->
        StakeDatum
          { stakedAmount = ps.perStakeGTs
          , owner = pk
          , lockedBy = existingLocks
          }
    )
    $ mkStakeOwners ps
  where
    existingLocks :: [ProposalLock]
    existingLocks =
      [ Voted (ProposalId 0) (ResultTag 0)
      , Voted (ProposalId 1) (ResultTag 2)
      ]

---

-- | Script purpose of the proposal validator.
proposalScriptPurpose :: ScriptPurpose
proposalScriptPurpose = Spending proposalRef

-- | Script purpose of the stake validator, given which stake we want to spend.
mkStakeScriptPurpose :: Int -> ScriptPurpose
mkStakeScriptPurpose = Spending . mkStakeRef

---

{- | The propsoal redeemer used to spend the proposal UTXO, which is always
      'AdvanceProposal' in this case.
-}
proposalRedeemer :: ProposalRedeemer
proposalRedeemer = AdvanceProposal

{- | The propsoal redeemer used to spend the stake UTXO, which is always
      'WitnessStake' in this case.
-}
stakeRedeemer :: StakeRedeemer
stakeRedeemer = WitnessStake

---

-- | Create some valid stake owners.
mkStakeOwners :: Parameters -> [PubKeyHash]
mkStakeOwners ps =
  sort $
    take
      (fromIntegral ps.stakeCount)
      pubKeyHashes

---

-- | Create a 'TxInfo' that update the status of a proposal.
advance ::
  Parameters ->
  TxInfo
advance ps =
  let pst = Value.singleton proposalPolicySymbol "" 1
      sst = Value.assetClassValue stakeAssetClass 1

      proposalInputDatum :: ProposalDatum
      proposalInputDatum =
        mkProposalInputDatum ps

      proposalOutputDatum :: ProposalDatum
      proposalOutputDatum =
        proposalInputDatum
          { status = ps.toStatus
          }

      stakeInputDatums :: [StakeDatum]
      stakeInputDatums = mkStakeInputDatums ps

      mkStakeOutputDatum :: StakeDatum -> StakeDatum
      mkStakeOutputDatum si =
        if ps.alterOutputStakes
          then
            si
              { stakedAmount = ps.perStakeGTs + 1
              }
          else si

      stakeValue =
        let gts =
              if ps.perStakeGTs == 0
                then mempty
                else
                  Value.assetClassValue
                    (untag stake.gtClassRef)
                    (untag ps.perStakeGTs)
         in sortValue $
              sst <> minAda
                <> gts

      stakeBuilder :: BaseBuilder
      stakeBuilder =
        foldMap
          ( \(si, idx) ->
              let so = mkStakeOutputDatum si
               in mconcat @BaseBuilder
                    [ input $
                        script stakeValidatorHash
                          . withValue stakeValue
                          . withDatum si
                          . withOutRef (mkStakeRef idx)
                    , output $
                        script stakeValidatorHash
                          . withValue stakeValue
                          . withDatum so
                    ]
          )
          $ let withIds = zip stakeInputDatums [0 ..]
             in if ps.includeAllStakes
                  then withIds
                  else [head withIds]

      signBuilder :: BaseBuilder
      signBuilder =
        let sos = mkStakeOwners ps
         in if ps.signByAllCosigners
              then foldMap signedWith sos
              else signedWith $ head sos

      builder :: BaseBuilder
      builder =
        mconcat
          [ txId "95ba4015e30aef16a3461ea97a779f814aeea6b8009d99a94add4b8293be737a"
          , signBuilder
          , timeRange ps.validTimeRange
          , input $
              script proposalValidatorHash
                . withValue pst
                . withDatum proposalInputDatum
                . withTxId proposalTxRef
          , output $
              script proposalValidatorHash
                . withValue (pst <> minAda)
                . withDatum proposalOutputDatum
          ]
   in buildTxInfoUnsafe $ builder <> stakeBuilder

---

{- | Given the proposal status, create a time range that is in time for
      advacing to the next state.
-}
mkInTimeTimeRange :: ProposalStatus -> POSIXTimeRange
mkInTimeTimeRange advanceFrom =
  case advanceFrom of
    -- [S + 1, S + D - 1]
    Draft ->
      closedBoundedInterval
        (proposalStartingTime + 1)
        (proposalStartingTime + (def :: ProposalTimingConfig).draftTime - 1)
    -- [S + D + V + 1, S + D + V + L - 1]
    VotingReady ->
      closedBoundedInterval
        ( proposalStartingTime
            + (def :: ProposalTimingConfig).draftTime
            + (def :: ProposalTimingConfig).votingTime
            + 1
        )
        ( proposalStartingTime
            + (def :: ProposalTimingConfig).draftTime
            + (def :: ProposalTimingConfig).votingTime
            + (def :: ProposalTimingConfig).lockingTime
            - 1
        )
    -- [S + D + V + L + 1, S + + D + V + L + E - 1]
    Locked ->
      closedBoundedInterval
        ( proposalStartingTime
            + (def :: ProposalTimingConfig).draftTime
            + (def :: ProposalTimingConfig).votingTime
            + (def :: ProposalTimingConfig).lockingTime
            + 1
        )
        ( proposalStartingTime
            + (def :: ProposalTimingConfig).draftTime
            + (def :: ProposalTimingConfig).votingTime
            + (def :: ProposalTimingConfig).lockingTime
            + (def :: ProposalTimingConfig).executingTime - 1
        )
    Finished -> error "Cannot advance 'Finished' proposal"

{- | Given the proposal status, create a time range that is too time for
      advacing to the next state.
-}
mkTooLateTimeRange :: ProposalStatus -> POSIXTimeRange
mkTooLateTimeRange advanceFrom =
  case advanceFrom of
    -- [S + D + 1, S + D + V - 1]
    Draft ->
      closedBoundedInterval
        (proposalStartingTime + (def :: ProposalTimingConfig).draftTime + 1)
        ( proposalStartingTime
            + (def :: ProposalTimingConfig).draftTime
            + (def :: ProposalTimingConfig).votingTime - 1
        )
    -- [S + D + V + L + 1, S + D + V + L + E -1]
    VotingReady ->
      closedBoundedInterval
        ( proposalStartingTime
            + (def :: ProposalTimingConfig).draftTime
            + (def :: ProposalTimingConfig).votingTime
            + (def :: ProposalTimingConfig).lockingTime
            + 1
        )
        ( proposalStartingTime
            + (def :: ProposalTimingConfig).draftTime
            + (def :: ProposalTimingConfig).votingTime
            + (def :: ProposalTimingConfig).lockingTime
            + (def :: ProposalTimingConfig).executingTime
            - 1
        )
    -- [S + D + V + L + E + 1, S + D + V + L + E + 100]
    Locked ->
      closedBoundedInterval
        ( proposalStartingTime
            + (def :: ProposalTimingConfig).draftTime
            + (def :: ProposalTimingConfig).votingTime
            + (def :: ProposalTimingConfig).lockingTime
            + (def :: ProposalTimingConfig).executingTime
            + 1
        )
        ( proposalStartingTime
            + (def :: ProposalTimingConfig).draftTime
            + (def :: ProposalTimingConfig).votingTime
            + (def :: ProposalTimingConfig).lockingTime
            + (def :: ProposalTimingConfig).executingTime
            + 100
        )
    Finished -> error "Cannot advance 'Finished' proposal"

---

-- | Next state of the given proposal status.
getNextState :: ProposalStatus -> ProposalStatus
getNextState = \case
  Draft -> VotingReady
  VotingReady -> Locked
  Locked -> Finished
  Finished -> error "Cannot advance 'Finished' proposal"

---

advanceToNextStateInTimeParameters :: Int -> [Parameters]
advanceToNextStateInTimeParameters nCosigners =
  map
    ( \from ->
        let -- Set the vote count of outcome 0 to @def.countingVoting + 1@,
            --   meaning that outcome 0 will be the winner.
            outcome0WinningVotes =
              ProposalVotes $
                updateMap
                  (\_ -> Just $ untag (def :: ProposalThresholds).execute + 1)
                  (ResultTag 0)
                  (coerce emptyVotes)

            votes = case from of
              Draft -> emptyVotes
              -- With sufficient votes
              _ -> outcome0WinningVotes

            includeAllStakes = case from of
              Draft -> True
              _ -> False

            signByAllCosigners = case from of
              Draft -> True
              _ -> False
         in Parameters
              { fromStatus = from
              , toStatus = getNextState from
              , votes = votes
              , includeAllStakes = includeAllStakes
              , validTimeRange = mkInTimeTimeRange from
              , alterOutputStakes = False
              , stakeCount = fromIntegral nCosigners
              , signByAllCosigners = signByAllCosigners
              , perStakeGTs =
                  (def :: ProposalThresholds).vote
                    `div` fromIntegral nCosigners + 1
              }
    )
    [Draft, VotingReady, Locked]

advanceToFailedStateDueToTimeoutParameters :: Int -> [Parameters]
advanceToFailedStateDueToTimeoutParameters nCosigners =
  map
    ( \from ->
        Parameters
          { fromStatus = from
          , toStatus = Finished
          , votes = emptyVotes
          , includeAllStakes = False
          , validTimeRange = mkTooLateTimeRange from
          , alterOutputStakes = False
          , stakeCount = fromIntegral nCosigners
          , signByAllCosigners = False
          , perStakeGTs = 1
          }
    )
    [Draft, VotingReady, Locked]

insufficientVotesParameters :: Parameters
insufficientVotesParameters =
  let votes = emptyVotes
      from = VotingReady
      to = getNextState from
   in Parameters
        { fromStatus = from
        , toStatus = to
        , votes = votes
        , includeAllStakes = False
        , validTimeRange = mkInTimeTimeRange from
        , alterOutputStakes = False
        , stakeCount = 1
        , signByAllCosigners = True
        , perStakeGTs = 20
        }

insufficientCosignsParameters :: Int -> Parameters
insufficientCosignsParameters nCosigners =
  (\ps -> ps {perStakeGTs = 0}) $
    head $
      advanceToNextStateInTimeParameters nCosigners

advanceFromFinishedParameters :: Parameters
advanceFromFinishedParameters =
  Parameters
    { fromStatus = Finished
    , toStatus = Finished
    , votes = emptyVotes
    , includeAllStakes = False
    , validTimeRange = always
    , alterOutputStakes = False
    , stakeCount = 1
    , signByAllCosigners = True
    , perStakeGTs = 20
    }

invalidOutputStakeParameters :: Int -> [Parameters]
invalidOutputStakeParameters nCosigners =
  (\ps -> ps {alterOutputStakes = True})
    <$> advanceToNextStateInTimeParameters nCosigners

---

{- | Create a test tree that runs the stake validator and proposal validator to
      test the advancing functionalities.
-}
mkTestTree :: String -> Parameters -> Bool -> SpecificationTree
mkTestTree name ps isValidForProposalValidator = group name [proposal, stake]
  where
    txInfo = advance ps

    proposal =
      let proposalInputDatum = mkProposalInputDatum ps
       in testValidator
            isValidForProposalValidator
            "propsoal"
            (proposalValidator Shared.proposal)
            proposalInputDatum
            proposalRedeemer
            ( ScriptContext
                txInfo
                proposalScriptPurpose
            )

    stake =
      let idx = 0
          stakeInputDatum = mkStakeInputDatums ps !! idx
          isValid = not $ ps.alterOutputStakes
       in testValidator
            isValid
            "stake"
            (stakeValidator Shared.stake)
            stakeInputDatum
            stakeRedeemer
            ( ScriptContext
                txInfo
                (mkStakeScriptPurpose idx)
            )
