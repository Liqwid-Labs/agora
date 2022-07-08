module Sample.Proposal.Cosign (
  Parameters (..),
  validCosignNParameters,
  duplicateCosignersParameters,
  statusNotDraftCosignNParameters,
  invalidStakeOutputParameters,
  mkTestTree,
) where

import Agora.Proposal (
  ProposalDatum (..),
  ProposalId (ProposalId),
  ProposalRedeemer (Cosign),
  ProposalStatus (..),
  ResultTag (ResultTag),
  emptyVotesFor,
 )
import Agora.Proposal.Scripts (proposalValidator)
import Agora.Proposal.Time (
  ProposalStartingTime (ProposalStartingTime),
  ProposalTimingConfig (draftTime),
 )
import Agora.SafeMoney (GTTag)
import Agora.Stake (
  Stake (gtClassRef),
  StakeDatum (StakeDatum, owner),
  StakeRedeemer (WitnessStake),
  stakedAmount,
 )
import Agora.Stake.Scripts (stakeValidator)
import Data.Coerce (coerce)
import Data.Default (def)
import Data.List (sort)
import Data.Tagged (Tagged, untag)
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
  withRefIndex,
  withTxId,
  withValue,
 )
import PlutusLedgerApi.V1 (
  POSIXTimeRange,
  PubKeyHash,
  ScriptContext (ScriptContext),
  ScriptPurpose (Spending),
  TxInfo,
  TxOutRef (..),
  Value,
 )
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusTx.AssocMap qualified as AssocMap
import Sample.Proposal.Shared (proposalTxRef, stakeTxRef)
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
  testValidator,
 )
import Test.Util (closedBoundedInterval, pubKeyHashes, sortValue)

-- | Parameters for cosigning a proposal.
data Parameters = Parameters
  { newCosigners :: [PubKeyHash]
  -- ^ New cosigners to be added, and the owners of the generated stakes.
  , proposalStatus :: ProposalStatus
  -- ^ Current state of the proposal.
  , alterOutputStakes :: Bool
  -- ^ Whether to generate invalid stake outputs.
  --   In particular, the 'stakedAmount' of all the stake datums will be set to zero.
  }

-- | Owner of the creator stake, doesn't really matter in this case.
proposalCreator :: PubKeyHash
proposalCreator = signer

-- | The amount of GTs every generated stake has, doesn't really matter in this case.
perStakedGTs :: Tagged GTTag Integer
perStakedGTs = 5

{- | Create input proposal datum given the parameters.
   In particular, 'status' is set to 'proposalStstus'.
-}
mkProposalInputDatum :: Parameters -> ProposalDatum
mkProposalInputDatum ps =
  let effects =
        AssocMap.fromList
          [ (ResultTag 0, AssocMap.empty)
          , (ResultTag 1, AssocMap.empty)
          ]
   in ProposalDatum
        { proposalId = ProposalId 0
        , effects = effects
        , status = ps.proposalStatus
        , cosigners = [proposalCreator]
        , thresholds = def
        , votes = emptyVotesFor effects
        , timingConfig = def
        , startingTime = ProposalStartingTime 0
        }

{- | Create the output proposal datum given the parameters.
   The 'newCosigners' is added to the exisiting list of cosigners, note the said list should be sorted in
     ascending order.
-}
mkProposalOutputDatum :: Parameters -> ProposalDatum
mkProposalOutputDatum ps =
  let inputDatum = mkProposalInputDatum ps
   in inputDatum
        { cosigners = sort $ inputDatum.cosigners <> ps.newCosigners
        }

-- | Create all the input stakes given the parameters.
mkStakeInputDatums :: Parameters -> [StakeDatum]
mkStakeInputDatums = fmap (\pk -> StakeDatum perStakedGTs pk []) . newCosigners

-- | Create a 'TxInfo' that tries to cosign a proposal with new cosigners.
cosign :: Parameters -> TxInfo
cosign ps = buildTxInfoUnsafe builder
  where
    pst = Value.singleton proposalPolicySymbol "" 1
    sst = Value.assetClassValue stakeAssetClass 1

    ---

    stakeInputDatums :: [StakeDatum]
    stakeInputDatums = mkStakeInputDatums ps

    stakeValue :: Value
    stakeValue =
      sortValue $
        minAda
          <> Value.assetClassValue
            (untag stake.gtClassRef)
            (untag perStakedGTs)
          <> sst

    stakeBuilder :: BaseBuilder
    stakeBuilder =
      foldMap
        ( \(stakeDatum, refIdx) ->
            let stakeOutputDatum =
                  if ps.alterOutputStakes
                    then stakeDatum {stakedAmount = 0}
                    else stakeDatum
             in mconcat @BaseBuilder
                  [ input $
                      script stakeValidatorHash
                        . withValue stakeValue
                        . withDatum stakeDatum
                        . withTxId stakeTxRef
                        . withRefIndex refIdx
                  , output $
                      script stakeValidatorHash
                        . withValue stakeValue
                        . withDatum stakeOutputDatum
                  , signedWith stakeDatum.owner
                  ]
        )
        $ zip
          stakeInputDatums
          [2 ..]

    ---

    proposalInputDatum :: ProposalDatum
    proposalInputDatum = mkProposalInputDatum ps

    proposalOutputDatum :: ProposalDatum
    proposalOutputDatum = mkProposalOutputDatum ps

    proposalBuilder :: BaseBuilder
    proposalBuilder =
      mconcat
        [ input $
            script proposalValidatorHash
              . withValue pst
              . withDatum proposalInputDatum
              . withTxId proposalTxRef
              . withRefIndex proposalRefIdx
        , output $
            script proposalValidatorHash
              . withValue (sortValue (pst <> minAda))
              . withDatum proposalOutputDatum
        ]

    validTimeRange :: POSIXTimeRange
    validTimeRange =
      closedBoundedInterval
        (coerce proposalInputDatum.startingTime + 1)
        ( coerce proposalInputDatum.startingTime
            + proposalInputDatum.timingConfig.draftTime - 1
        )

    ---

    builder :: BaseBuilder
    builder =
      mconcat
        [ txId "05c67819fc3381a2052b929ab439244b7b5fe3b3bd07f2134055bbbb21bd9e52"
        , timeRange validTimeRange
        , proposalBuilder
        , stakeBuilder
        ]

-- | Reference index of the proposal UTXO.
proposalRefIdx :: Integer
proposalRefIdx = 1

-- | Spend the proposal ST.
proposalScriptPurpose :: ScriptPurpose
proposalScriptPurpose =
  Spending
    ( TxOutRef
        proposalTxRef
        proposalRefIdx
    )

-- | Consume the given stake.
mkStakeScriptPurpose :: Int -> ScriptPurpose
mkStakeScriptPurpose idx =
  Spending $
    TxOutRef
      stakeTxRef
      $ proposalRefIdx + 1 + fromIntegral idx

-- | Create a proposal redeemer which cosigns with the new cosginers.
mkProposalRedeemer :: Parameters -> ProposalRedeemer
mkProposalRedeemer (sort . newCosigners -> cs) = Cosign cs

-- | Stake redeemer for cosuming all the stakes generated in the module.
stakeRedeemer :: StakeRedeemer
stakeRedeemer = WitnessStake

---

-- | Create a valid parameters that cosign the proposal with a given number of cosigners.
validCosignNParameters :: Int -> Parameters
validCosignNParameters n
  | n > 0 =
      Parameters
        { newCosigners = take n pubKeyHashes
        , proposalStatus = Draft
        , alterOutputStakes = False
        }
  | otherwise = error "Number of cosigners should be positive"

---

{- | Parameters that make 'cosign' yield duplicate cosigners.
   Invalid for the ptoposal validator, perfectly valid for stake validator.
-}
duplicateCosignersParameters :: Parameters
duplicateCosignersParameters =
  Parameters
    { newCosigners = [proposalCreator]
    , proposalStatus = Draft
    , alterOutputStakes = False
    }

---

{- | Generate a list of parameters that sets proposal status to something other than 'Draft'.
   Invalid for the ptoposal validator, perfectly valid for stake validator.
-}
statusNotDraftCosignNParameters :: Int -> [Parameters]
statusNotDraftCosignNParameters n =
  map
    ( \st ->
        Parameters
          { newCosigners = take n pubKeyHashes
          , proposalStatus = st
          , alterOutputStakes = False
          }
    )
    [VotingReady, Locked, Finished]

---

{- | Parameters thet change the output stake datums.
   Invalid for both proposal validator and stake validator.
-}
invalidStakeOutputParameters :: Parameters
invalidStakeOutputParameters =
  (validCosignNParameters 2)
    { alterOutputStakes = True
    }

---

-- | Create a test tree given the parameters. Both the proposal validator and stake validator will be run.
mkTestTree ::
  -- | The name of the test group.
  String ->
  Parameters ->
  -- | Are the parameters valid for the proposal validator?
  Bool ->
  SpecificationTree
mkTestTree name ps isValid = group name [proposal, stake]
  where
    txInfo = cosign ps

    proposal =
      let proposalInputDatum = mkProposalInputDatum ps
       in testValidator
            isValid
            "propsoal"
            (proposalValidator Shared.proposal)
            proposalInputDatum
            (mkProposalRedeemer ps)
            ( ScriptContext
                txInfo
                proposalScriptPurpose
            )

    stake =
      let idx = 0
          stakeInputDatum = mkStakeInputDatums ps !! idx
          isValid = not ps.alterOutputStakes
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
