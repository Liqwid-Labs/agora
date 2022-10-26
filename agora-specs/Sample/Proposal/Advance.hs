{- |
Module     : Sample.Proposal.Advance
Maintainer : connor@mlabs.city
Description: Generate sample data for testing the functionalities of advancing proposals

Sample and utilities for testing the functionalities of advancing proposals.
-}
module Sample.Proposal.Advance (
  -- * Parameters
  ParameterBundle (..),
  GovernorParameters (..),
  AuthorityTokenParameters (..),
  ProposalParameters (..),
  StakeParameters (..),
  Winner (..),

  -- * Testing Utilities
  Validity (..),
  advance,
  mkTestTree,
  mkTestTree',

  -- * Parameter Bundles
  mkValidToNextStateBundle,
  mkValidToNextStateBundles,
  mkValidToFailedStateBundles,
  mkInsufficientVotesBundle,
  mkAmbiguousWinnerBundle,
  mkFromFinishedBundles,
  mkInsufficientCosignsBundle,
  mkToNextStateTooLateBundles,
  mkMintGATsForWrongEffectsBundle,
  mkNoGATMintedBundle,
  mkGATsWithWrongDatumBundle,
  mkMintGATsWithoutTagBundle,
  mkBadGovernorOutputDatumBundle,
  mkUnexpectedOutputStakeBundles,
) where

import Agora.Governor (
  Governor (..),
  GovernorDatum (..),
  GovernorRedeemer (MintGATs),
 )
import Agora.Proposal (
  ProposalDatum (..),
  ProposalEffectGroup,
  ProposalEffectMetadata (ProposalEffectMetadata),
  ProposalId (ProposalId),
  ProposalRedeemer (AdvanceProposal),
  ProposalStatus (..),
  ProposalThresholds (..),
  ProposalVotes (ProposalVotes),
  ResultTag (ResultTag),
  emptyVotesFor,
 )
import Agora.Proposal.Time (
  ProposalStartingTime (ProposalStartingTime),
  ProposalTimingConfig (
    draftTime,
    executingTime,
    lockingTime,
    votingTime
  ),
 )
import Agora.SafeMoney (AuthorityTokenTag, GTTag)
import Agora.Stake (
  StakeDatum (..),
 )
import Agora.Utils (scriptHashToTokenName)
import Control.Applicative (liftA2)
import Control.Monad.State (execState, modify, when)
import Data.Default (def)
import Data.List (singleton, sort)
import Data.Map.Strict qualified as StrictMap
import Data.Maybe (fromJust)
import Data.Tagged (Tagged (Tagged), untag)
import Plutarch.Context (
  input,
  mint,
  output,
  referenceInput,
  script,
  signedWith,
  timeRange,
  withDatum,
  withInlineDatum,
  withRef,
  withValue,
 )
import Plutarch.Extra.AssetClass (AssetClass (AssetClass), assetClassValue)
import Plutarch.Lift (PLifted, PUnsafeLiftDecl)
import PlutusLedgerApi.V2 (
  Credential (PubKeyCredential),
  DatumHash,
  POSIXTime,
  POSIXTimeRange,
  PubKeyHash,
  ScriptHash,
  TxOutRef (TxOutRef),
  ValidatorHash,
 )
import Sample.Proposal.Shared (
  governorTxRef,
  proposalTxRef,
  stakeTxRef,
 )
import Sample.Shared (
  authorityTokenPolicy,
  authorityTokenSymbol,
  governor,
  governorAssetClass,
  governorValidator,
  governorValidatorHash,
  minAda,
  proposalAssetClass,
  proposalValidator,
  proposalValidatorHash,
  signer,
  stakeAssetClass,
  stakeValidatorHash,
 )
import Test.Specification (
  SpecificationTree,
  group,
  testPolicy,
  testValidator,
 )
import Test.Util (
  CombinableBuilder,
  closedBoundedInterval,
  datumHash,
  groupsOfN,
  mkMinting,
  mkSpending,
  pubKeyHashes,
  scriptHashes,
  sortValue,
  toDatum,
  validatorHashes,
 )

{- | A bunch of parameters that control the generation of the transaction
    context.
-}
data ParameterBundle = ParameterBundle
  { proposalParameters :: ProposalParameters
  -- ^ Parameters related to the the advancing proposal.
  , stakeParameters :: StakeParameters
  -- ^ Parameters related to stakes.
  , governorParameters :: Maybe GovernorParameters
  -- ^ Parameters related to GST moving. If set to 'Nothing', the GST won't
  --   be moved, thus the governor validator won't be run in 'mkTestTree'.
  , authorityTokenParameters :: [AuthorityTokenParameters]
  -- ^ Parameters related to GAT minting. If set to 'Nothing', no GAT will
  --     be minted, thus the GAT minting policy won't be run in 'mkTestTree'.
  , transactionTimeRange :: POSIXTimeRange
  -- ^ The value of 'TxInfo.txInfoValidRange', valid range of the generated
  --    transaction.
  , extraSignature :: Maybe PubKeyHash
  -- ^ An extra signator. Intended to be used when
  --    'StakeParametersstakeParameters.transactionSignedByOwners' is set to
  --    false.
  }

-- | Everything about the generated governor stuff.
newtype GovernorParameters = GovernorParameters
  { invalidGovernorOutputDatum :: Bool
  -- ^ The output governor datum will be changed.
  }

-- | Everything about the generated authority token stuff.
data AuthorityTokenParameters = forall
    (datum :: Type)
    (pdatum :: S -> Type).
  ( PUnsafeLiftDecl pdatum
  , PLifted pdatum ~ datum
  , PIsData pdatum
  ) =>
  AuthorityTokenParameters
  { mintGATsFor :: ValidatorHash
  -- ^ GATs will be minted and sent to the given group of effects.
  , carryDatum :: Maybe datum
  -- ^ The datum that GAT UTxOs will be carrying.
  , carryAuthScript :: Maybe ScriptHash
  -- ^ The authentication script that GAT UTxOs link to through their token name.
  , invalidTokenName :: Bool
  -- ^ If set to true, GATs won't be tagged by their corresponding effect
  --    hashes.
  }

-- | Represent the winning effect group(s).
data Winner
  = -- | Only one effect at the given index has the highest votes.
    EffectAt Index
  | -- | All the effects have the same highest votes.
    All

-- | Everything about the generated proposal stuff.
data ProposalParameters = ProposalParameters
  { fromStatus :: ProposalStatus
  -- ^ What status is the proposal advancing from
  , toStatus :: ProposalStatus
  -- ^ What status is the proposal advancing to
  , effectList :: [ProposalEffectGroup]
  -- ^ The effect groups of the proposal. A neutral effect group is not
  --    required here.
  , winnerAndVotes :: Maybe (Winner, Integer)
  -- ^ Specify the effect group(s) that have the highest votes, and the value
  --    of the highest votes.
  , numCosigners :: NumStake
  -- ^ The number of cosigners.
  , invalidProposalOutputDatum :: Bool
  -- ^ Whether to make the proposal output datum invalid or not.
  }

-- | Everything about the generated stake stuff.
data StakeParameters = StakeParameters
  { numStake :: NumStake
  , perStakeGTs :: Tagged GTTag Integer
  , transactionSignedByOwners :: Bool
  }

-- | Represent the number of stakes or the number of the cosigners.
type NumStake = Int

-- | Represent an index.
type Index = Int

{- | The validity of the generated transaction for variuos componets.
     'True' means valid, 'False' means invalid.
-}
data Validity = Validity
  { forProposalValidator :: Bool
  , forStakeValidator :: Bool
  , forGovernorValidator :: Maybe Bool
  , forAuthorityTokenPolicy :: Maybe Bool
  }

--------------------------------------------------------------------------------

-- * Proposal

-- | Mock cosigners.
mkCosigners :: NumStake -> [Credential]
mkCosigners = sort . fmap PubKeyCredential . flip take pubKeyHashes

-- | Allocate the result tag for the effect at the given index.
outcomeIdxToResultTag :: Index -> ResultTag
outcomeIdxToResultTag = ResultTag . fromIntegral

-- | Add a neutral effect group and allocate result tags for the effect groups.
mkEffects ::
  ProposalParameters ->
  StrictMap.Map ResultTag ProposalEffectGroup
mkEffects ps =
  let resultTags = map ResultTag [0 ..]
      neutralEffect = StrictMap.empty
      finalEffects = ps.effectList <> [neutralEffect]
   in StrictMap.fromList $ zip resultTags finalEffects

-- | Set the votes of the winning group(s).
setWinner :: (Winner, Integer) -> ProposalVotes -> ProposalVotes
setWinner (All, votes) (ProposalVotes m) =
  ProposalVotes $ StrictMap.mapMaybe (const $ Just votes) m
setWinner (EffectAt winnerIdx, votes) (ProposalVotes m) =
  let winnerResultTag = outcomeIdxToResultTag winnerIdx
   in ProposalVotes $ StrictMap.adjust (const votes) winnerResultTag m

-- | Mock votes for the proposal, given the parameters.
mkVotes ::
  ProposalParameters ->
  ProposalVotes
mkVotes ps =
  let effects = mkEffects ps
      emptyVotes = emptyVotesFor effects
   in maybe emptyVotes (`setWinner` emptyVotes) (ps.winnerAndVotes)

-- | The starting time of every generated proposal.
proposalStartingTime :: POSIXTime
proposalStartingTime = 0

-- | Create the input proposal datum given the parameters.
mkProposalInputDatum :: ProposalParameters -> ProposalDatum
mkProposalInputDatum ps =
  let effects = mkEffects ps
      votes = mkVotes ps
      st = ProposalStartingTime proposalStartingTime
   in ProposalDatum
        { proposalId = ProposalId 0
        , effects = effects
        , status = ps.fromStatus
        , cosigners = mkCosigners ps.numCosigners
        , thresholds = def
        , votes = votes
        , timingConfig = def
        , startingTime = st
        }

-- | Create the output proposal datum given the parameters.
mkProposalOutputDatum :: ProposalParameters -> ProposalDatum
mkProposalOutputDatum ps =
  let inputDatum = mkProposalInputDatum ps
      outputCosigners =
        if ps.invalidProposalOutputDatum
          then []
          else inputDatum.cosigners
   in inputDatum
        { status = ps.toStatus
        , cosigners = outputCosigners
        }

-- | Reference to the proposal UTXO.
proposalRef :: TxOutRef
proposalRef = TxOutRef proposalTxRef 1

{- | Create a context builder that contains all the information about the
      input/output of the proposal validator, given the paramters.
-}
mkProposalBuilder :: forall b. CombinableBuilder b => ProposalParameters -> b
mkProposalBuilder ps =
  let pst = assetClassValue proposalAssetClass 1
      value = sortValue $ minAda <> pst
   in mconcat
        [ input $
            mconcat
              [ script proposalValidatorHash
              , withRef proposalRef
              , withDatum (mkProposalInputDatum ps)
              , withValue value
              ]
        , output $
            mconcat
              [ script proposalValidatorHash
              , withDatum (mkProposalOutputDatum ps)
              , withValue value
              ]
        ]

{- | The proposal redeemer used to spend the proposal UTXO, which is always
      'AdvanceProposal' in this case.
-}
proposalRedeemer :: ProposalRedeemer
proposalRedeemer = AdvanceProposal

--------------------------------------------------------------------------------

-- * Stake

-- Mock owners of the stakes.
mkStakeOwners :: NumStake -> [Credential]
mkStakeOwners = mkCosigners

-- | Create the input stake datums given the parameters.
mkStakeInputDatums :: StakeParameters -> [StakeDatum]
mkStakeInputDatums ps =
  let template =
        StakeDatum
          { stakedAmount = ps.perStakeGTs
          , owner = PubKeyCredential ""
          , delegatedTo = Nothing
          , lockedBy = []
          }
   in (\owner -> template {owner = owner})
        <$> mkStakeOwners ps.numStake

-- | Create the reference to a particular stake UTXO.
mkStakeRef :: Index -> TxOutRef
mkStakeRef = TxOutRef stakeTxRef . (+ 3) . fromIntegral

{- | Create a context builder that contains all the inputs/outputs of the
      stake validator.
-}
mkStakeBuilder :: forall b. CombinableBuilder b => StakeParameters -> b
mkStakeBuilder ps =
  let perStakeValue =
        sortValue $
          minAda
            <> assetClassValue stakeAssetClass 1
            <> assetClassValue
              governor.gtClassRef
              ps.perStakeGTs
      perStake idx i =
        let withSig =
              case (i.owner, ps.transactionSignedByOwners) of
                (PubKeyCredential owner, True) -> signedWith owner
                _ -> mempty
         in mconcat
              [ withSig
              , referenceInput $
                  mconcat
                    [ script stakeValidatorHash
                    , withRef (mkStakeRef idx)
                    , withValue perStakeValue
                    , withInlineDatum i
                    ]
              ]
   in mconcat $
        zipWith
          perStake
          [0 :: Index ..]
          (mkStakeInputDatums ps)

--------------------------------------------------------------------------------

-- * Governor

-- | The input governor datum.
governorInputDatum :: GovernorDatum
governorInputDatum =
  GovernorDatum
    { proposalThresholds = def
    , nextProposalId = ProposalId 42
    , proposalTimings = def
    , createProposalTimeRangeMaxWidth = def
    , maximumProposalsPerStake = 3
    }

-- | Create the output governor datum given the parameters.
mkGovernorOutputDatum :: GovernorParameters -> GovernorDatum
mkGovernorOutputDatum ps =
  if ps.invalidGovernorOutputDatum
    then governorInputDatum {maximumProposalsPerStake = 15}
    else governorInputDatum

-- | Reference to the governor UTXO.
governorRef :: TxOutRef
governorRef = TxOutRef governorTxRef 2

{- | Create a context builder that contains the input and  the output of the
      governor validator.
-}
mkGovernorBuilder :: forall b. CombinableBuilder b => GovernorParameters -> b
mkGovernorBuilder ps =
  let gst = assetClassValue governorAssetClass 1
      value = sortValue $ gst <> minAda
   in mconcat
        [ input $
            mconcat
              [ script governorValidatorHash
              , withValue value
              , withRef governorRef
              , withDatum governorInputDatum
              ]
        , output $
            mconcat
              [ script governorValidatorHash
              , withValue value
              , withRef governorRef
              , withDatum (mkGovernorOutputDatum ps)
              ]
        ]

{- | The proposal redeemer used to spend the governor UTXO, which is always
      'MintGATs' in this case.
-}
governorRedeemer :: GovernorRedeemer
governorRedeemer = MintGATs

--------------------------------------------------------------------------------

-- * Authority Token

{- | Create a context builder that contains the infomation about the minted
      authority tokens and where they're sent to.
-}
mkAuthorityTokenBuilder ::
  forall b.
  CombinableBuilder b =>
  AuthorityTokenParameters ->
  b
mkAuthorityTokenBuilder ps@AuthorityTokenParameters {carryDatum} =
  let tn =
        case (ps.invalidTokenName, ps.carryAuthScript) of
          (True, Just _) -> "deadbeef"
          (True, Nothing) -> "deadbeef"
          (False, Just as) -> scriptHashToTokenName as
          (False, Nothing) -> ""
      ac = Tagged @AuthorityTokenTag $ AssetClass authorityTokenSymbol tn
      minted = assetClassValue ac 1
      value = sortValue $ minAda <> minted
   in mconcat
        [ mint minted
        , output $
            mconcat
              [ script ps.mintGATsFor
              , maybe mempty withDatum carryDatum
              , withValue value
              ]
        ]

-- | The redeemer used while running the authority token policy.
authorityTokenRedeemer :: ()
authorityTokenRedeemer = ()

--------------------------------------------------------------------------------

-- | Create a 'TxInfo' that update the status of a proposal.
advance ::
  forall b.
  CombinableBuilder b =>
  ParameterBundle ->
  b
advance pb =
  let mkBuilderMaybe = maybe mempty
   in mconcat
        [ mkProposalBuilder pb.proposalParameters
        , mkStakeBuilder pb.stakeParameters
        , mkBuilderMaybe mkGovernorBuilder pb.governorParameters
        , foldMap mkAuthorityTokenBuilder pb.authorityTokenParameters
        , timeRange pb.transactionTimeRange
        , maybe mempty signedWith pb.extraSignature
        ]

--------------------------------------------------------------------------------

{- | Create a test tree that runs the relavant componets to test the advancing
      functionalities.
-}
mkTestTree ::
  String ->
  ParameterBundle ->
  Validity ->
  SpecificationTree
mkTestTree name pb val =
  group name $ mconcat [proposal, governor, authority]
  where
    spend = mkSpending advance pb

    proposal =
      let proposalInputDatum = mkProposalInputDatum pb.proposalParameters
       in singleton $
            testValidator
              val.forProposalValidator
              "proposal"
              proposalValidator
              proposalInputDatum
              proposalRedeemer
              (spend proposalRef)
    governor =
      maybe [] singleton $
        testValidator
          (fromJust val.forGovernorValidator)
          "governor"
          governorValidator
          governorInputDatum
          governorRedeemer
          (spend governorRef)
          <$ pb.governorParameters

    authority = case pb.authorityTokenParameters of
      [] -> []
      _ ->
        singleton
          ( testPolicy
              (fromJust val.forAuthorityTokenPolicy)
              "authority"
              authorityTokenPolicy
              authorityTokenRedeemer
              (mkMinting advance pb authorityTokenSymbol)
          )

{- | Create a test tree that runs a bunch of parameter bundles. These bundles
      should have the same validity.
-}
mkTestTree' ::
  String ->
  (ParameterBundle -> String) ->
  [ParameterBundle] ->
  Validity ->
  SpecificationTree
mkTestTree' groupName mkCaseName bundles val =
  group groupName $
    (\b -> mkTestTree (mkCaseName b) b val)
      <$> bundles

--------------------------------------------------------------------------------

-- Utilities for creating parameter bundles

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

-- | Next state of the given proposal status.
getNextState :: ProposalStatus -> ProposalStatus
getNextState = \case
  Draft -> VotingReady
  VotingReady -> Locked
  Locked -> Finished
  Finished -> error "Cannot advance 'Finished' proposal"

-- | Calculate the number of GTs per stake in order to exceed the minimum limit.
compPerStakeGTsForDraft :: NumStake -> Tagged GTTag Integer
compPerStakeGTsForDraft nCosigners =
  Tagged $
    untag (def :: ProposalThresholds).toVoting
      `div` fromIntegral nCosigners + 1

dummyDatum :: ()
dummyDatum = ()

dummyDatumHash :: DatumHash
dummyDatumHash = datumHash $ toDatum dummyDatum

-- | Create given number of effect groups. Each group will have 3 effects.
mkMockEffects :: Bool -> Int -> [ProposalEffectGroup]
mkMockEffects useAuthScript n = effects
  where
    effectsPerGroup = 3

    mkAuthScripts True = Just <$> scriptHashes
    mkAuthScripts False = repeat Nothing
    authScripts = mkAuthScripts useAuthScript

    datums = repeat dummyDatumHash

    effectMetadata = zipWith ProposalEffectMetadata datums authScripts
    effectScripts = validatorHashes

    effects =
      take n $
        StrictMap.fromList
          <$> groupsOfN
            effectsPerGroup
            (zip effectScripts effectMetadata)

numberOfVotesThatExceedsTheMinimumRequirement :: Integer
numberOfVotesThatExceedsTheMinimumRequirement =
  untag (def @ProposalThresholds).execute + 1

mkWinnerVotes :: Index -> (Winner, Integer)
mkWinnerVotes idx =
  ( EffectAt idx
  , numberOfVotesThatExceedsTheMinimumRequirement
  )

ambiguousWinnerVotes :: (Winner, Integer)
ambiguousWinnerVotes =
  ( All
  , numberOfVotesThatExceedsTheMinimumRequirement
  )

--------------------------------------------------------------------------------

-- * Parameter Bundles

---

-- * Legal

defaultWinnerIdx :: Index
defaultWinnerIdx = 0

{- | Advance a proposal to the next state, perfectly valid for all the
    componets.
-}
mkValidToNextStateBundle ::
  -- | Number of cosigners.
  Word ->
  -- | Number of effects.
  Word ->
  -- | Toggle the referenc script in GAT UTXO.
  Bool ->
  -- | The initial proposal state, should not be 'Finished'.
  ProposalStatus ->
  ParameterBundle
mkValidToNextStateBundle _ _ _ Finished =
  error "Cannot advance from Finished"
mkValidToNextStateBundle nCosigners nEffects authScript from =
  let next = getNextState from
      effects = mkMockEffects authScript $ fromIntegral nEffects
      winner = defaultWinnerIdx

      template =
        ParameterBundle
          { proposalParameters =
              ProposalParameters
                { fromStatus = from
                , toStatus = next
                , effectList = effects
                , winnerAndVotes = Nothing
                , numCosigners = fromIntegral nCosigners
                , invalidProposalOutputDatum = False
                }
          , stakeParameters =
              StakeParameters
                { numStake = 0
                , perStakeGTs =
                    compPerStakeGTsForDraft $
                      fromIntegral nCosigners
                , transactionSignedByOwners = False
                }
          , governorParameters = Nothing
          , authorityTokenParameters = []
          , transactionTimeRange = mkInTimeTimeRange from
          , extraSignature = Just signer
          }

      -- This is my favourite part of the test suite, lol.
      modifyTemplate = do
        when (from == Draft) $
          modify $ \b ->
            b
              { stakeParameters =
                  b.stakeParameters
                    { transactionSignedByOwners = True
                    , numStake = fromIntegral nCosigners
                    }
              , extraSignature = Nothing
              }

        when (from == VotingReady || from == Locked) $
          modify $ \b ->
            b
              { proposalParameters =
                  b.proposalParameters
                    { winnerAndVotes = Just $ mkWinnerVotes winner
                    }
              }

        when (from == Locked) $
          modify $ \b ->
            let aut =
                  StrictMap.elems $
                    StrictMap.mapWithKey
                      ( \vh (ProposalEffectMetadata _ authScript) ->
                          AuthorityTokenParameters
                            { mintGATsFor = vh
                            , carryDatum = Just dummyDatum
                            , carryAuthScript = authScript
                            , invalidTokenName = False
                            }
                      )
                      (effects !! winner)
                gov =
                  GovernorParameters
                    { invalidGovernorOutputDatum = False
                    }
             in b
                  { governorParameters = Just gov
                  , authorityTokenParameters = aut
                  }
   in execState modifyTemplate template

mkValidToNextStateBundles ::
  -- | Number of cosigners
  Word ->
  -- | Number of effects
  Word ->
  [ParameterBundle]
mkValidToNextStateBundles nCosigners nEffects =
  liftA2
    (mkValidToNextStateBundle nCosigners nEffects)
    [True, False]
    [Draft, VotingReady, Locked]

mkValidToFailedStateBundles ::
  -- | Number of cosigners
  Word ->
  -- | Number of effects
  Word ->
  [ParameterBundle]
mkValidToFailedStateBundles nCosigners nEffects =
  liftA2
    mkBundle
    [True, False]
    [Draft, VotingReady, Locked]
  where
    mkBundle authScript from =
      let next = Finished
          effects = mkMockEffects authScript $ fromIntegral nEffects
       in ParameterBundle
            { proposalParameters =
                ProposalParameters
                  { fromStatus = from
                  , toStatus = next
                  , effectList = effects
                  , winnerAndVotes = Nothing
                  , numCosigners = fromIntegral nCosigners
                  , invalidProposalOutputDatum = False
                  }
            , stakeParameters =
                StakeParameters
                  { numStake = 0
                  , perStakeGTs =
                      compPerStakeGTsForDraft $
                        fromIntegral nCosigners
                  , transactionSignedByOwners = False
                  }
            , governorParameters = Nothing
            , authorityTokenParameters = []
            , transactionTimeRange = mkTooLateTimeRange from
            , extraSignature = Just signer
            }

-- * Illegal

mkFromFinishedBundles ::
  -- | Number of cosigners
  Word ->
  -- | Number of effects
  Word ->
  [ParameterBundle]
mkFromFinishedBundles nCosigners nEffects =
  liftA2
    mkBundle
    [True, False]
    [Draft, VotingReady, Locked]
  where
    mkBundle authScript from =
      let template = mkValidToNextStateBundle nCosigners nEffects authScript from
       in template
            { proposalParameters =
                template.proposalParameters
                  { fromStatus = Finished
                  , toStatus = Finished
                  }
            }

mkToNextStateTooLateBundles :: Word -> Word -> [ParameterBundle]
mkToNextStateTooLateBundles nCosigners nEffects =
  liftA2
    mkBundle
    [True, False]
    [Draft, VotingReady, Locked]
  where
    mkBundle authScript from =
      let template = mkValidToNextStateBundle nCosigners nEffects authScript from
       in template
            { transactionTimeRange = mkTooLateTimeRange from
            }

mkUnexpectedOutputStakeBundles :: Word -> Word -> [ParameterBundle]
mkUnexpectedOutputStakeBundles nCosigners nEffects =
  liftA2
    mkBundle
    [True, False]
    [VotingReady, Locked]
  where
    mkBundle authScript from =
      let template = mkValidToNextStateBundle nCosigners nEffects authScript from
       in template
            { stakeParameters =
                template.stakeParameters
                  { numStake = 1
                  }
            }

-- * From Draft

mkInsufficientCosignsBundle :: Word -> Word -> ParameterBundle
mkInsufficientCosignsBundle nCosigners nEffects =
  template
    { stakeParameters =
        template.stakeParameters
          { perStakeGTs = insuffcientPerStakeGTs
          }
    }
  where
    insuffcientPerStakeGTs =
      Tagged $
        untag (def :: ProposalThresholds).toVoting
          `div` fromIntegral nCosigners - 1
    template = mkValidToNextStateBundle nCosigners nEffects False Draft

-- * From VotingReady

setWinnerAndVotes ::
  ParameterBundle ->
  Maybe (Winner, Integer) ->
  ParameterBundle
setWinnerAndVotes pb wv =
  pb
    { proposalParameters =
        pb.proposalParameters
          { winnerAndVotes = wv
          }
    }

mkInsufficientVotesBundle ::
  Word ->
  Word ->
  ParameterBundle
mkInsufficientVotesBundle nCosigners nEffects =
  mkValidToNextStateBundle nCosigners nEffects False VotingReady
    `setWinnerAndVotes` Nothing

mkAmbiguousWinnerBundle ::
  Word ->
  Word ->
  ParameterBundle
mkAmbiguousWinnerBundle nCosigners nEffects =
  mkValidToNextStateBundle nCosigners nEffects False VotingReady
    `setWinnerAndVotes` Just ambiguousWinnerVotes

-- * From Locked

mkValidFromLockedBundle :: Word -> Word -> ParameterBundle
mkValidFromLockedBundle nCosigners nEffects =
  mkValidToNextStateBundle nCosigners nEffects False Locked

mkMintGATsForWrongEffectsBundle ::
  Word ->
  Word ->
  ParameterBundle
mkMintGATsForWrongEffectsBundle nCosigners nEffects =
  template
    { authorityTokenParameters =
        take 4 $
          zipWith
            (\a i -> a {mintGATsFor = validatorHashes !! i})
            template.authorityTokenParameters
            [1, 3 ..]
    }
  where
    template = mkValidFromLockedBundle nCosigners nEffects

mkNoGATMintedBundle ::
  Word ->
  Word ->
  ParameterBundle
mkNoGATMintedBundle nCosigners nEffects =
  template
    { authorityTokenParameters = []
    }
  where
    template = mkValidFromLockedBundle nCosigners nEffects

mkMintGATsWithoutTagBundle ::
  Word ->
  Word ->
  ParameterBundle
mkMintGATsWithoutTagBundle nCosigners nEffects =
  template
    { authorityTokenParameters =
        ( \aut ->
            aut
              { invalidTokenName = True
              }
        )
          <$> template.authorityTokenParameters
    }
  where
    template = mkValidFromLockedBundle nCosigners nEffects

mkGATsWithWrongDatumBundle ::
  Word ->
  Word ->
  ParameterBundle
mkGATsWithWrongDatumBundle nCosigners nEffects =
  template
    { authorityTokenParameters = newAut
    }
  where
    template = mkValidFromLockedBundle nCosigners nEffects
    newAut =
      ( \aut ->
          AuthorityTokenParameters
            aut.mintGATsFor
            (Just (1 :: Integer))
            aut.carryAuthScript
            False
      )
        <$> template.authorityTokenParameters

mkBadGovernorOutputDatumBundle ::
  Word ->
  Word ->
  ParameterBundle
mkBadGovernorOutputDatumBundle nCosigners nEffects =
  template
    { governorParameters = Just gov
    }
  where
    template = mkValidFromLockedBundle nCosigners nEffects
    gov = GovernorParameters True
