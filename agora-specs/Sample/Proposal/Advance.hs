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
  mkInvalidOutputStakeBundles,
  mkMintGATsForWrongEffectsBundle,
  mkNoGATMintedBundle,
  mkGATsWithWrongDatumBundle,
  mkMintGATsWithoutTagBundle,
  mkBadGovernorOutputDatumBundle,
) where

import Agora.AuthorityToken (
  AuthorityToken (AuthorityToken),
  authorityTokenPolicy,
 )
import Agora.Governor (
  GovernorDatum (..),
  GovernorRedeemer (MintGATs),
 )
import Agora.Governor.Scripts (governorValidator)
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
import Agora.Stake (
  Stake (gtClassRef),
  StakeDatum (..),
  StakeRedeemer (WitnessStake),
 )
import Agora.Stake.Scripts (stakeValidator)
import Agora.Utils (validatorHashToTokenName)
import Control.Monad.State (execState, modify, when)
import Data.Default (def)
import Data.List (sort)
import Data.Maybe (catMaybes, fromJust)
import Data.Tagged (Tagged (..), untag)
import Plutarch.Context (
  BaseBuilder,
  buildTxInfoUnsafe,
  input,
  mint,
  output,
  script,
  signedWith,
  timeRange,
  withDatum,
  withOutRef,
  withValue,
 )
import Plutarch.Lift (PLifted, PUnsafeLiftDecl)
import PlutusLedgerApi.V1 (
  DatumHash,
  POSIXTime,
  POSIXTimeRange,
  PubKeyHash,
  ScriptContext (ScriptContext),
  ScriptPurpose (Minting, Spending),
  TxInfo,
  TxOutRef (TxOutRef),
  ValidatorHash,
 )
import PlutusLedgerApi.V1.Value (AssetClass (..))
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusTx.AssocMap qualified as AssocMap
import Sample.Proposal.Shared (
  governorTxRef,
  proposalTxRef,
  stakeTxRef,
 )
import Sample.Shared (
  authorityTokenSymbol,
  govAssetClass,
  govValidatorHash,
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
  testPolicy,
  testValidator,
 )
import Test.Util (
  closedBoundedInterval,
  datumHash,
  groupsOfN,
  pubKeyHashes,
  sortValue,
  toDatum,
  updateMap,
  validatorHashes,
 )

data ParameterBundle = ParameterBundle
  { proposalParameters :: ProposalParameters
  , stakeParameters :: StakeParameters
  , governorParameters :: Maybe GovernorParameters
  , authorityTokenParameters :: Maybe AuthorityTokenParameters
  , transactionTimeRange :: POSIXTimeRange
  , extraSignature :: Maybe PubKeyHash
  }

newtype GovernorParameters = GovernorParameters
  { invalidGovernorOutputDatum :: Bool
  }

data AuthorityTokenParameters = forall
    (datum :: Type)
    (pdatum :: S -> Type).
  ( PUnsafeLiftDecl pdatum
  , PLifted pdatum ~ datum
  , PIsData pdatum
  ) =>
  AuthorityTokenParameters
  { mintGATsFor :: [ValidatorHash]
  , carryDatum :: Maybe datum
  , invalidTokenName :: Bool
  }

data Winner = EffectAt Index | All

data ProposalParameters = ProposalParameters
  { fromStatus :: ProposalStatus
  , toStatus :: ProposalStatus
  , effectList :: [AssocMap.Map ValidatorHash DatumHash]
  , winnerAndVotes :: Maybe (Winner, Integer)
  , numCosigners :: NumStake
  , invalidProposalOutputDatum :: Bool
  }

data StakeParameters = StakeParameters
  { numStake :: NumStake
  , perStakeGTs :: Integer
  , transactionSignedByOwners :: Bool
  , invalidStakeOutputDatum :: Bool
  }

type NumStake = Int
type Index = Int

data Validity = Validity
  { forProposalValidator :: Bool
  , forStakeValidator :: Bool
  , forGovernorValidator :: Maybe Bool
  , forAuthorityTokenPolicy :: Maybe Bool
  }

--------------------------------------------------------------------------------

-- * Proposal

mkCosigners :: NumStake -> [PubKeyHash]
mkCosigners = sort . flip take pubKeyHashes

outcomeIdxToResultTag :: Index -> ResultTag
outcomeIdxToResultTag = ResultTag . fromIntegral

mkEffects ::
  ProposalParameters ->
  AssocMap.Map ResultTag (AssocMap.Map ValidatorHash DatumHash)
mkEffects ps =
  let resultTags = map ResultTag [0 ..]
      neutralEffect = AssocMap.empty
      finalEffects = ps.effectList <> [neutralEffect]
   in AssocMap.fromList $ zip resultTags finalEffects

setWinner :: (Winner, Integer) -> ProposalVotes -> ProposalVotes
setWinner (All, votes) (ProposalVotes m) =
  ProposalVotes $ AssocMap.mapMaybe (const $ Just votes) m
setWinner (EffectAt winnerIdx, votes) (ProposalVotes m) =
  let winnerResultTag = outcomeIdxToResultTag winnerIdx
   in ProposalVotes $ updateMap (const $ Just votes) winnerResultTag m

mkVotes ::
  ProposalParameters ->
  ProposalVotes
mkVotes ps =
  let effects = mkEffects ps
      emptyVotes = emptyVotesFor effects
   in maybe emptyVotes (`setWinner` emptyVotes) (ps.winnerAndVotes)

proposalStartingTime :: POSIXTime
proposalStartingTime = 0

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

mkProposalBuilder :: ProposalParameters -> BaseBuilder
mkProposalBuilder ps =
  let pst = Value.singleton proposalPolicySymbol "" 1
      value = sortValue $ minAda <> pst
   in mconcat
        [ input $
            script proposalValidatorHash
              . withOutRef proposalRef
              . withDatum (mkProposalInputDatum ps)
              . withValue value
        , output $
            script proposalValidatorHash
              . withDatum (mkProposalOutputDatum ps)
              . withValue value
        ]

-- | Script purpose of the proposal validator.
proposalScriptPurpose :: ScriptPurpose
proposalScriptPurpose = Spending proposalRef

{- | The proposal redeemer used to spend the proposal UTXO, which is always
      'AdvanceProposal' in this case.
-}
proposalRedeemer :: ProposalRedeemer
proposalRedeemer = AdvanceProposal

--------------------------------------------------------------------------------

-- * Stake

mkStakeOwners :: NumStake -> [PubKeyHash]
mkStakeOwners = mkCosigners

-- | Create the input stake datums given the parameters.
mkStakeInputDatums :: StakeParameters -> [StakeDatum]
mkStakeInputDatums ps =
  let template =
        StakeDatum
          { stakedAmount = Tagged ps.perStakeGTs
          , owner = ""
          , lockedBy = []
          }
   in (\owner -> template {owner = owner})
        <$> mkStakeOwners ps.numStake

mkStakeOutputDatums :: StakeParameters -> [StakeDatum]
mkStakeOutputDatums ps =
  let inputDatums = mkStakeInputDatums ps
      outputStakedAmount =
        Tagged $
          if ps.invalidStakeOutputDatum
            then ps.perStakeGTs * 10
            else ps.perStakeGTs
      modify inp = inp {stakedAmount = outputStakedAmount}
   in modify <$> inputDatums

getStakeInputDatumAt :: StakeParameters -> Index -> StakeDatum
getStakeInputDatumAt ps = (!!) (mkStakeInputDatums ps)

-- | Create the reference to a particular stake UTXO.
mkStakeRef :: Index -> TxOutRef
mkStakeRef = TxOutRef stakeTxRef . (+ 3) . fromIntegral

mkStakeBuilder :: StakeParameters -> BaseBuilder
mkStakeBuilder ps =
  let perStakeValue =
        sortValue $
          minAda
            <> Value.assetClassValue stakeAssetClass 1
            <> Value.assetClassValue
              (untag stake.gtClassRef)
              ps.perStakeGTs
      perStake idx i o =
        let withSig =
              if ps.transactionSignedByOwners
                then signedWith i.owner
                else mempty
         in mconcat
              [ withSig
              , input $
                  script stakeValidatorHash
                    . withOutRef (mkStakeRef idx)
                    . withValue perStakeValue
                    . withDatum i
              , output $
                  script stakeValidatorHash
                    . withValue perStakeValue
                    . withDatum o
              ]
   in mconcat $
        zipWith3
          perStake
          [0 :: Index ..]
          (mkStakeInputDatums ps)
          (mkStakeOutputDatums ps)

-- | Script purpose of the stake validator, given which stake we want to spend.
getStakeScriptPurposeAt :: Index -> ScriptPurpose
getStakeScriptPurposeAt = Spending . mkStakeRef

{- | The proposal redeemer used to spend the stake UTXO, which is always
      'WitnessStake' in this case.
-}
stakeRedeemer :: StakeRedeemer
stakeRedeemer = WitnessStake

--------------------------------------------------------------------------------

-- * Governor

governorInputDatum :: GovernorDatum
governorInputDatum =
  GovernorDatum
    { proposalThresholds = def
    , nextProposalId = ProposalId 42
    , proposalTimings = def
    , createProposalTimeRangeMaxWidth = def
    , maximumProposalsPerStake = 3
    }

mkGovernorOutputDatum :: GovernorParameters -> GovernorDatum
mkGovernorOutputDatum ps =
  if ps.invalidGovernorOutputDatum
    then governorInputDatum {maximumProposalsPerStake = 15}
    else governorInputDatum

-- | Reference to the governor UTXO.
governorRef :: TxOutRef
governorRef = TxOutRef governorTxRef 2

mkGovernorBuilder :: GovernorParameters -> BaseBuilder
mkGovernorBuilder ps =
  let gst = Value.assetClassValue govAssetClass 1
      value = sortValue $ gst <> minAda
   in mconcat
        [ input $
            script govValidatorHash
              . withValue value
              . withOutRef governorRef
              . withDatum governorInputDatum
        , output $
            script govValidatorHash
              . withValue value
              . withOutRef governorRef
              . withDatum (mkGovernorOutputDatum ps)
        ]

governorScriptPurpose :: ScriptPurpose
governorScriptPurpose = Spending governorRef

{- | The proposal redeemer used to spend the governor UTXO, which is always
      'MintGATs' in this case.
-}
governorRedeemer :: GovernorRedeemer
governorRedeemer = MintGATs

--------------------------------------------------------------------------------

-- * Authority Token

mkAuthorityTokenBuilder :: AuthorityTokenParameters -> BaseBuilder
mkAuthorityTokenBuilder (AuthorityTokenParameters es mdt invalidTokenName) =
  foldMap perEffect es
  where
    perEffect :: ValidatorHash -> BaseBuilder
    perEffect vh =
      let tn =
            if invalidTokenName
              then ""
              else validatorHashToTokenName vh
          ac = AssetClass (authorityTokenSymbol, tn)
          minted = Value.assetClassValue ac 1
          value = sortValue $ minAda <> minted
       in mconcat
            [ mint minted
            , output $
                script vh
                  . maybe id withDatum mdt
                  . withValue value
            ]

authorityTokenScriptPurepose :: ScriptPurpose
authorityTokenScriptPurepose = Minting authorityTokenSymbol

authorityTokenRedeemer :: ()
authorityTokenRedeemer = ()

--------------------------------------------------------------------------------

-- | Create a 'TxInfo' that update the status of a proposal.
advance ::
  ParameterBundle ->
  TxInfo
advance pb =
  let mkBuilderMaybe = maybe mempty
   in buildTxInfoUnsafe $
        mconcat
          [ mkProposalBuilder pb.proposalParameters
          , mkStakeBuilder pb.stakeParameters
          , mkBuilderMaybe mkGovernorBuilder pb.governorParameters
          , mkBuilderMaybe mkAuthorityTokenBuilder pb.authorityTokenParameters
          , timeRange pb.transactionTimeRange
          , maybe mempty signedWith pb.extraSignature
          ]

--------------------------------------------------------------------------------

{- | Create a test tree that runs the stake validator and proposal validator to
      test the advancing functionalities.
-}
mkTestTree ::
  String ->
  ParameterBundle ->
  Validity ->
  SpecificationTree
mkTestTree name params val =
  group name $ catMaybes [proposal, stake, governor, authority]
  where
    txInfo = advance params

    proposal =
      let proposalInputDatum = mkProposalInputDatum params.proposalParameters
       in Just $
            testValidator
              val.forProposalValidator
              "proposal"
              (proposalValidator Shared.proposal)
              proposalInputDatum
              proposalRedeemer
              ( ScriptContext
                  txInfo
                  proposalScriptPurpose
              )

    stake =
      let idx = 0
       in Just $
            testValidator
              val.forStakeValidator
              "stake"
              (stakeValidator Shared.stake)
              (getStakeInputDatumAt params.stakeParameters idx)
              stakeRedeemer
              ( ScriptContext
                  txInfo
                  (getStakeScriptPurposeAt idx)
              )

    governor =
      testValidator
        (fromJust val.forGovernorValidator)
        "governor"
        (governorValidator Shared.governor)
        governorInputDatum
        governorRedeemer
        ( ScriptContext
            txInfo
            governorScriptPurpose
        )
        <$ params.governorParameters

    authority =
      testPolicy
        (fromJust val.forAuthorityTokenPolicy)
        "authority"
        (authorityTokenPolicy $ AuthorityToken Shared.govAssetClass)
        authorityTokenRedeemer
        ( ScriptContext
            txInfo
            authorityTokenScriptPurepose
        )
        <$ (params.authorityTokenParameters)

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

compPerStakeGTsForDraft :: NumStake -> Integer
compPerStakeGTsForDraft nCosigners =
  untag (def :: ProposalThresholds).vote
    `div` fromIntegral nCosigners + 1

dummyDatum :: ()
dummyDatum = ()

dummyDatumHash :: DatumHash
dummyDatumHash = datumHash $ toDatum dummyDatum

mkMockEffects :: Int -> [AssocMap.Map ValidatorHash DatumHash]
mkMockEffects =
  flip
    take
    ( AssocMap.fromList
        . flip zip (repeat dummyDatumHash)
        <$> groupsOfN 3 validatorHashes
    )

mkWinnerVotes :: Index -> (Winner, Integer)
mkWinnerVotes idx = (EffectAt idx, untag (def @ProposalThresholds).execute + 1)

ambiguousWinnerVotes :: (Winner, Integer)
ambiguousWinnerVotes = (All, untag (def @ProposalThresholds).execute + 1)

--------------------------------------------------------------------------------

-- * Parameter Bundles

---

-- * Legal

defaultWinnerIdx :: Index
defaultWinnerIdx = 0

mkValidToNextStateBundle :: Word -> Word -> ProposalStatus -> ParameterBundle
mkValidToNextStateBundle nCosigners nEffects from =
  let next = getNextState from
      effects = mkMockEffects $ fromIntegral nEffects
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
                { numStake = 1
                , perStakeGTs =
                    compPerStakeGTsForDraft $
                      fromIntegral nCosigners
                , transactionSignedByOwners = False
                , invalidStakeOutputDatum = False
                }
          , governorParameters = Nothing
          , authorityTokenParameters = Nothing
          , transactionTimeRange = mkInTimeTimeRange from
          , extraSignature = Just signer
          }

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
                  AuthorityTokenParameters
                    { mintGATsFor = AssocMap.keys $ effects !! winner
                    , carryDatum = Just dummyDatum
                    , invalidTokenName = False
                    }
                gov =
                  GovernorParameters
                    { invalidGovernorOutputDatum = False
                    }
             in b
                  { governorParameters = Just gov
                  , authorityTokenParameters = Just aut
                  }
   in execState modifyTemplate template

mkValidToNextStateBundles ::
  -- | Number of cosigners
  Word ->
  -- | Number of effects
  Word ->
  [ParameterBundle]
mkValidToNextStateBundles nCosigners nEffects =
  mkValidToNextStateBundle nCosigners nEffects
    <$> [ Draft
        , VotingReady
        , Locked
        ]

mkValidToFailedStateBundles ::
  -- | Number of cosigners
  Word ->
  -- | Number of effects
  Word ->
  [ParameterBundle]
mkValidToFailedStateBundles nCosigners nEffects =
  mkBundle
    <$> [ Draft
        , VotingReady
        , Locked
        ]
  where
    mkBundle from =
      let next = Finished
          effects = mkMockEffects $ fromIntegral nEffects
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
                  { numStake = 1
                  , perStakeGTs =
                      compPerStakeGTsForDraft $
                        fromIntegral nCosigners
                  , transactionSignedByOwners = False
                  , invalidStakeOutputDatum = False
                  }
            , governorParameters = Nothing
            , authorityTokenParameters = Nothing
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
  mkBundle
    <$> [ Draft
        , VotingReady
        , Locked
        ]
  where
    mkBundle from =
      let template = mkValidToNextStateBundle nCosigners nEffects from
       in template
            { proposalParameters =
                template.proposalParameters
                  { fromStatus = Finished
                  , toStatus = Finished
                  }
            }

mkToNextStateTooLateBundles :: Word -> Word -> [ParameterBundle]
mkToNextStateTooLateBundles nCosigners nEffects =
  mkBundle
    <$> [ Draft
        , VotingReady
        , Locked
        ]
  where
    mkBundle from =
      let template = mkValidToNextStateBundle nCosigners nEffects from
       in template
            { transactionTimeRange = mkTooLateTimeRange from
            }

mkInvalidOutputStakeBundles :: Word -> Word -> [ParameterBundle]
mkInvalidOutputStakeBundles nCosigners nEffects =
  mkBundle <$> [Draft, VotingReady, Locked]
  where
    mkBundle from =
      let template = mkValidToNextStateBundle nCosigners nEffects from
       in template
            { stakeParameters =
                template.stakeParameters
                  { invalidStakeOutputDatum = True
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
      untag (def :: ProposalThresholds).vote
        `div` fromIntegral nCosigners - 1
    template = mkValidToNextStateBundle nCosigners nEffects Draft

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
  mkValidToNextStateBundle nCosigners nEffects VotingReady
    `setWinnerAndVotes` Nothing

mkAmbiguousWinnerBundle ::
  Word ->
  Word ->
  ParameterBundle
mkAmbiguousWinnerBundle nCosigners nEffects =
  mkValidToNextStateBundle nCosigners nEffects VotingReady
    `setWinnerAndVotes` Just ambiguousWinnerVotes

-- * From Locked

mkValidFromLockedBundle :: Word -> Word -> ParameterBundle
mkValidFromLockedBundle nCosigners nEffects = mkValidToNextStateBundle nCosigners nEffects Locked

mkMintGATsForWrongEffectsBundle ::
  Word ->
  Word ->
  ParameterBundle
mkMintGATsForWrongEffectsBundle nCosigners nEffects =
  template
    { authorityTokenParameters =
        ( \aut ->
            aut
              { mintGATsFor =
                  [ validatorHashes !! 1
                  , validatorHashes !! 3
                  , validatorHashes !! 5
                  , validatorHashes !! 7
                  ]
              }
        )
          <$> template.authorityTokenParameters
    }
  where
    template = mkValidFromLockedBundle nCosigners nEffects

mkNoGATMintedBundle ::
  Word ->
  Word ->
  ParameterBundle
mkNoGATMintedBundle nCosigners nEffects =
  template
    { authorityTokenParameters = Nothing
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
    { authorityTokenParameters = Just newAut
    }
  where
    template = mkValidFromLockedBundle nCosigners nEffects
    aut = fromJust template.authorityTokenParameters
    newAut =
      AuthorityTokenParameters
        aut.mintGATsFor
        (Just (1 :: Integer))
        False

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
