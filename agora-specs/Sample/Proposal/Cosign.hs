{- |
Module     : Sample.Proposal.Cosign
Maintainer : connor@mlabs.city
Description: Generate sample data for testing the functionalities of cosigning proposals

Sample and utilities for testing the functionalities of cosigning proposals.
-}
module Sample.Proposal.Cosign (
  Parameters (..),
  validCosignNParameters,
  duplicateCosignersParameters,
  statusNotDraftCosignNParameters,
  mkTestTree,
) where

import Agora.Governor (Governor (..))
import Agora.Proposal (
  ProposalDatum (..),
  ProposalId (ProposalId),
  ProposalRedeemer (Cosign),
  ProposalStatus (..),
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
  StakeDatum (StakeDatum, owner),
 )
import Data.Coerce (coerce)
import Data.Default (def)
import Data.List (sort)
import Data.Map.Strict qualified as StrictMap
import Data.Tagged (untag)
import Plutarch.Context (
  input,
  output,
  referenceInput,
  script,
  signedWith,
  timeRange,
  txId,
  withDatum,
  withInlineDatum,
  withRef,
  withValue,
 )
import Plutarch.SafeMoney (Discrete)
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 (
  Credential (PubKeyCredential),
  POSIXTimeRange,
  PubKeyHash,
  TxOutRef (..),
  Value,
 )
import Sample.Proposal.Shared (proposalTxRef, stakeTxRef)
import Sample.Shared (
  agoraScripts,
  fromDiscrete,
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
  testValidator,
 )
import Test.Util (CombinableBuilder, closedBoundedInterval, mkSpending, pubKeyHashes, sortValue)

-- | Parameters for cosigning a proposal.
data Parameters = Parameters
  { newCosigners :: [Credential]
  -- ^ New cosigners to be added, and the owners of the generated stakes.
  , proposalStatus :: ProposalStatus
  -- ^ Current state of the proposal.
  }

-- | Owner of the creator stake, doesn't really matter in this case.
proposalCreator :: PubKeyHash
proposalCreator = signer

-- | The amount of GTs every generated stake has, doesn't really matter in this case.
perStakedGTs :: Discrete GTTag
perStakedGTs = 5

{- | Create input proposal datum given the parameters.
   In particular, 'status' is set to 'proposalStstus'.
-}
mkProposalInputDatum :: Parameters -> ProposalDatum
mkProposalInputDatum ps =
  let effects =
        StrictMap.fromList
          [ (ResultTag 0, StrictMap.empty)
          , (ResultTag 1, StrictMap.empty)
          ]
   in ProposalDatum
        { proposalId = ProposalId 0
        , effects = effects
        , status = ps.proposalStatus
        , cosigners = [PubKeyCredential proposalCreator]
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
mkStakeInputDatums =
  fmap (\pk -> StakeDatum perStakedGTs pk Nothing [])
    . (.newCosigners)

-- | Create a 'TxInfo' that tries to cosign a proposal with new cosigners.
cosign :: forall b. CombinableBuilder b => Parameters -> b
cosign ps = builder
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
            (untag governor.gtClassRef)
            (fromDiscrete perStakedGTs)
          <> sst

    stakeBuilder =
      foldMap
        ( \(stakeDatum, refIdx) ->
            mconcat
              [ referenceInput $
                  mconcat
                    [ script stakeValidatorHash
                    , withValue stakeValue
                    , withInlineDatum stakeDatum
                    , withRef (mkStakeRef refIdx)
                    ]
              , case stakeDatum.owner of
                  PubKeyCredential k -> signedWith k
                  _ -> mempty
              ]
        )
        $ zip
          stakeInputDatums
          [0 ..]

    ---

    proposalInputDatum :: ProposalDatum
    proposalInputDatum = mkProposalInputDatum ps

    proposalOutputDatum :: ProposalDatum
    proposalOutputDatum = mkProposalOutputDatum ps

    proposalBuilder =
      mconcat
        [ input $
            mconcat
              [ script proposalValidatorHash
              , withValue pst
              , withDatum proposalInputDatum
              , withRef proposalRef
              ]
        , output $
            mconcat
              [ script proposalValidatorHash
              , withValue (sortValue (pst <> minAda))
              , withDatum proposalOutputDatum
              ]
        ]

    validTimeRange :: POSIXTimeRange
    validTimeRange =
      closedBoundedInterval
        (coerce proposalInputDatum.startingTime + 1)
        ( coerce proposalInputDatum.startingTime
            + proposalInputDatum.timingConfig.draftTime - 1
        )

    ---

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
proposalRef :: TxOutRef
proposalRef = TxOutRef proposalTxRef proposalRefIdx

-- | Consume the given stake.
mkStakeRef :: Int -> TxOutRef
mkStakeRef idx =
  TxOutRef
    stakeTxRef
    $ proposalRefIdx + 1 + fromIntegral idx

-- | Create a proposal redeemer which cosigns with the new cosginers.
mkProposalRedeemer :: Parameters -> ProposalRedeemer
mkProposalRedeemer = Cosign . sort . (.newCosigners)

---

-- | Create a valid parameters that cosign the proposal with a given number of cosigners.
validCosignNParameters :: Int -> Parameters
validCosignNParameters n
  | n > 0 =
      Parameters
        { newCosigners = take n (fmap PubKeyCredential pubKeyHashes)
        , proposalStatus = Draft
        }
  | otherwise = error "Number of cosigners should be positive"

---

{- | Parameters that make 'cosign' yield duplicate cosigners.
   Invalid for the ptoposal validator, perfectly valid for stake validator.
-}
duplicateCosignersParameters :: Parameters
duplicateCosignersParameters =
  Parameters
    { newCosigners = [PubKeyCredential proposalCreator]
    , proposalStatus = Draft
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
          { newCosigners = take n (fmap PubKeyCredential pubKeyHashes)
          , proposalStatus = st
          }
    )
    [VotingReady, Locked, Finished]

---

-- | Create a test tree given the parameters. Both the proposal validator and stake validator will be run.
mkTestTree ::
  -- | The name of the test group.
  String ->
  Parameters ->
  -- | Are the parameters valid for the proposal validator?
  Bool ->
  SpecificationTree
mkTestTree name ps isValid = proposal
  where
    spend = mkSpending cosign ps

    proposal =
      let proposalInputDatum = mkProposalInputDatum ps
       in testValidator
            isValid
            (name <> ": proposal")
            agoraScripts.compiledProposalValidator
            proposalInputDatum
            (mkProposalRedeemer ps)
            (spend proposalRef)
