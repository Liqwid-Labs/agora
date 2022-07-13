{- |
Module     : Sample.Governor
Maintainer : connor@mlabs.city
Description: Sample based testing for Governor utxos

This module tests primarily the happy path for Governor interactions
-}
module Sample.Governor (
  createProposal,
  mutateState,
  mintGATs,
  mintGST,
) where

import Agora.Effect.NoOp (noOpValidator)
import Agora.Governor (GovernorDatum (..), getNextProposalId)
import Agora.Proposal (
  ProposalDatum (..),
  ProposalId (..),
  ProposalStatus (..),
  ProposalVotes (..),
  ResultTag (..),
  emptyVotesFor,
 )
import Agora.Proposal qualified as P (ProposalDatum (proposalId))
import Agora.Proposal.Time (
  ProposalStartingTime (ProposalStartingTime),
  ProposalTimingConfig (..),
 )
import Agora.Stake (ProposalLock (..), Stake (..), StakeDatum (..))
import Data.Default.Class (Default (def))
import Data.Tagged (Tagged (..), untag)
import Plutarch.Api.V1 (mkValidator, validatorHash)
import Plutarch.Context (
  MintingBuilder,
  SpendingBuilder,
  buildMintingUnsafe,
  buildSpendingUnsafe,
  fee,
  input,
  mint,
  output,
  script,
  signedWith,
  timeRange,
  txId,
  withDatum,
  withRefIndex,
  withSpending,
  withTxId,
  withValue,
 )
import PlutusLedgerApi.V1 (
  BuiltinData (BuiltinData),
  Data (I),
  Datum (Datum),
  ScriptContext,
  TokenName (TokenName),
  TxOutRef (txOutRefId),
  Validator,
  ValidatorHash (..),
 )
import PlutusLedgerApi.V1.Value (AssetClass (AssetClass))
import PlutusLedgerApi.V1.Value qualified as Value (
  assetClassValue,
  singleton,
 )
import PlutusTx.AssocMap qualified as AssocMap (
  empty,
  fromList,
  singleton,
 )
import Sample.Shared (
  authorityTokenSymbol,
  govAssetClass,
  govValidatorHash,
  gstUTXORef,
  minAda,
  proposalPolicySymbol,
  proposalStartingTimeFromTimeRange,
  proposalValidatorHash,
  signer,
  signer2,
  stake,
  stakeAssetClass,
  stakeValidatorHash,
 )
import Test.Util (closedBoundedInterval, toDatumHash)

-- | Unit datum
unitDatum :: Datum
unitDatum = Datum . BuiltinData $ I 0 -- This could be anything, really. It doesn't matter.

{- | A valid 'ScriptContext' for minting GST.

    - Only the minting policy will be ran in the transaction.
    - An arbitrary UTXO is spent to create the token.

        - We call this the "witness" UTXO.
        - This UTXO is referenced in the 'Agora.Governor.Governor' parameter
        - The minting policy should only be ran once its life time,
          cause the GST cannot be minted twice or burnt.

    - The output UTXO must carry a valid 'GovernorDatum'.
    - It's worth noticing that the transaction should send the GST to the governor validator,
        but unfortunately we can't check it in the policy. The GST will stay at the address of
        the governor validator forever once the token is under control of the said validator.

    TODO: tag the output UTXO with the target address.
-}
mintGST :: ScriptContext
mintGST =
  let gst = Value.assetClassValue govAssetClass 1

      governorOutputDatum :: GovernorDatum
      governorOutputDatum =
        GovernorDatum
          { proposalThresholds = def
          , nextProposalId = ProposalId 0
          , proposalTimings = def
          , createProposalTimeRangeMaxWidth = def
          , maximumProposalsPerStake = 3
          }

      witness :: ValidatorHash
      witness = "a926a9a72a0963f428e3252caa8354e655603996fb8892d6b8323fd072345924"

      builder :: MintingBuilder
      builder =
        mconcat
          [ txId "90906d3e6b4d6dec2e747dcdd9617940ea8358164c7244694cfa39dec18bd9d4"
          , signedWith signer
          , mint gst
          , input $
              script witness
                . withTxId (txOutRefId gstUTXORef)
                . withRefIndex 0
          , output $
              script govValidatorHash
                . withValue (gst <> minAda)
                . withDatum governorOutputDatum
          ]
   in buildMintingUnsafe builder

{- | A valid script context to create a proposal.

    Three component will run in the transaction:
    TODO: mention redeemers

    - Governor validator
    - Stake validator
    - Proposal policy

    The components will ensure:

    - The governor state UTXO is spent

        - A new UTXO is paid back to governor validator, which carries the GST.
        - The proposal id in the state datum is advanced.

    - A new UTXO is sent to the proposal validator

        - The UTXO contains a newly minted proposal state token.
        - It also carries a legal proposal state datum, whose status is set to 'Agora.Proposal.Draft'.

    - A stake is spent to create a proposal

        - The stake owner must sign the transaction.
        - The output stake must paid back to the stake validator.
        - The output stake is locked by the newly created proposal.
-}
createProposal :: ScriptContext
createProposal =
  let pst = Value.singleton proposalPolicySymbol "" 1
      gst = Value.assetClassValue govAssetClass 1
      sst = Value.assetClassValue stakeAssetClass 1
      stackedGTs = 424242424242
      thisProposalId = ProposalId 0

      governorInputDatum :: GovernorDatum
      governorInputDatum =
        GovernorDatum
          { proposalThresholds = def
          , nextProposalId = thisProposalId
          , proposalTimings = def
          , createProposalTimeRangeMaxWidth = def
          , maximumProposalsPerStake = 3
          }

      effects =
        AssocMap.fromList
          [ (ResultTag 0, AssocMap.empty)
          , (ResultTag 1, AssocMap.empty)
          ]
      proposalDatum :: ProposalDatum
      proposalDatum =
        ProposalDatum
          { P.proposalId = ProposalId 0
          , effects = effects
          , status = Draft
          , cosigners = [signer]
          , thresholds = def
          , votes = emptyVotesFor effects
          , timingConfig = def
          , startingTime = proposalStartingTimeFromTimeRange validTimeRange
          }

      stakeInputDatum :: StakeDatum
      stakeInputDatum =
        StakeDatum
          { stakedAmount = Tagged stackedGTs
          , owner = signer
          , lockedBy = []
          }

      governorOutputDatum :: GovernorDatum
      governorOutputDatum = governorInputDatum {nextProposalId = getNextProposalId thisProposalId}

      proposalLocks :: [ProposalLock]
      proposalLocks =
        [ Created thisProposalId
        ]
      stakeOutputDatum :: StakeDatum
      stakeOutputDatum = stakeInputDatum {lockedBy = proposalLocks}

      validTimeRange = closedBoundedInterval 10 15

      builder :: SpendingBuilder
      builder =
        mconcat
          [ txId "1ffb9669335c908d9a4774a4bf7aa7bfafec91d015249b4138bc83fde4a3330a"
          , fee $ Value.singleton "" "" 2
          , timeRange $ closedBoundedInterval 10 15
          , signedWith signer
          , mint pst
          , input $
              script govValidatorHash
                . withValue gst
                . withDatum governorInputDatum
                . withTxId "4355a46b19d348dc2f57c046f8ef63d4538ebb936000f3c9ee954a27460dd865"
          , input $
              script stakeValidatorHash
                . withValue (sst <> Value.assetClassValue (untag stake.gtClassRef) stackedGTs)
                . withDatum stakeInputDatum
                . withTxId "4262bbd0b3fc926b74eaa8abab5def6ce5e6b94f19cf221c02a16e7da8cd470f"
          , output $
              script proposalValidatorHash
                . withValue (pst <> minAda)
                . withDatum proposalDatum
          , output $
              script govValidatorHash
                . withValue (gst <> minAda)
                . withDatum governorOutputDatum
          , output $
              script stakeValidatorHash
                . withValue (sst <> Value.assetClassValue (untag stake.gtClassRef) stackedGTs <> minAda)
                . withDatum stakeOutputDatum
          , withSpending $
              script govValidatorHash
                . withValue gst
                . withDatum governorInputDatum
          ]
   in buildSpendingUnsafe builder

{- This script context should be a valid transaction for minting authority for the effect scrips.

    The following components will run:

    - Governor validator
    - Authority policy
    - Proposal validator

    There should be only one proposal the transaction.
    The validity of the proposal will be checked:

    - It's in 'Agora.Proposal.Locked' state.
    - It has a 'winner' effect group, meaning that the votes meet the requirements.

    The system will ensure that for every effect scrips in said effect group,
    a newly minted GAT is sent to the corresponding effect, and properly tagged.
-}
mintGATs :: ScriptContext
mintGATs =
  let pst = Value.singleton proposalPolicySymbol "" 1
      gst = Value.assetClassValue govAssetClass 1
      gat = Value.assetClassValue atAssetClass 1

      mockEffect :: Validator
      mockEffect = mkValidator $ noOpValidator ""
      mockEffectHash :: ValidatorHash
      mockEffectHash = validatorHash mockEffect
      mockEffectOutputDatum :: Datum
      mockEffectOutputDatum = unitDatum
      atTokenName :: TokenName
      atTokenName = TokenName hash
        where
          ValidatorHash hash = mockEffectHash
      atAssetClass :: AssetClass
      atAssetClass = AssetClass (authorityTokenSymbol, atTokenName)

      governorInputDatum :: GovernorDatum
      governorInputDatum =
        GovernorDatum
          { proposalThresholds = def
          , nextProposalId = ProposalId 5
          , proposalTimings = def
          , createProposalTimeRangeMaxWidth = def
          , maximumProposalsPerStake = 3
          }

      effects =
        AssocMap.fromList
          [ (ResultTag 0, AssocMap.empty)
          , (ResultTag 1, AssocMap.singleton mockEffectHash $ toDatumHash mockEffectOutputDatum)
          ]
      proposalVotes :: ProposalVotes
      proposalVotes =
        ProposalVotes $
          AssocMap.fromList
            [ (ResultTag 0, 100)
            , (ResultTag 1, 2000) -- The winner
            ]
      proposalInputDatum :: ProposalDatum
      proposalInputDatum =
        ProposalDatum
          { P.proposalId = ProposalId 0
          , effects = effects
          , status = Locked
          , cosigners = [signer, signer2]
          , thresholds = def
          , votes = proposalVotes
          , timingConfig = def
          , startingTime = ProposalStartingTime 10
          }

      governorOutputDatum :: GovernorDatum
      governorOutputDatum = governorInputDatum

      proposalOutputDatum :: ProposalDatum
      proposalOutputDatum = proposalInputDatum {status = Finished}

      validTimeRange =
        closedBoundedInterval
          ((def :: ProposalTimingConfig).lockingTime + 11)
          ((def :: ProposalTimingConfig).executingTime - 11)

      builder :: SpendingBuilder
      builder =
        mconcat
          [ txId "ff755f613c1f7487dfbf231325c67f481f7a97e9faf4d8b09ad41176fd65cbe7"
          , signedWith signer
          , signedWith signer2
          , timeRange validTimeRange
          , fee (Value.singleton "" "" 2)
          , mint gat
          , input $
              script govValidatorHash
                . withValue gst
                . withDatum governorInputDatum
                . withTxId "4355a46b19d348dc2f57c046f8ef63d4538ebb936000f3c9ee954a27460dd865"
          , input $
              script proposalValidatorHash
                . withValue pst
                . withDatum proposalInputDatum
                . withTxId "11b2162f267614b803761032b6333040fc61478ae788c088614ee9487ab0c1b7"
          , output $
              script govValidatorHash
                . withValue (gst <> minAda)
                . withDatum governorOutputDatum
          , output $
              script proposalValidatorHash
                . withValue (pst <> minAda)
                . withDatum proposalOutputDatum
          , output $
              script mockEffectHash
                . withValue (gat <> minAda)
                . withDatum mockEffectOutputDatum
          , withSpending $
              script govValidatorHash
                . withValue gst
                . withDatum governorInputDatum
          ]
   in buildSpendingUnsafe builder

{- | A valid script context for changing the state datum of the governor.

    In this case, the following components will run:

    * Governor validator
    * Effect script

    The effect script should carry an valid tagged authority token,
      and said token will be burnt in the transaction. We use 'noOpValidator'
      here as a mock effect, so no actual change is done to the governor state.
    TODO: use 'Agora.Effect.GovernorMutation.mutateGovernorEffect' as the mock effect in the future.

    The governor will ensure the new governor state is valid.
-}
mutateState :: ScriptContext
mutateState =
  let gst = Value.assetClassValue govAssetClass 1
      gat = Value.assetClassValue atAssetClass 1
      burntGAT = Value.assetClassValue atAssetClass (-1)

      -- TODO: Use the *real* effect, see https://github.com/Liqwid-Labs/agora/pull/62
      mockEffect :: Validator
      mockEffect = mkValidator $ noOpValidator ""
      mockEffectHash :: ValidatorHash
      mockEffectHash = validatorHash mockEffect
      atTokenName :: TokenName
      atTokenName = TokenName hash
        where
          ValidatorHash hash = mockEffectHash
      atAssetClass :: AssetClass
      atAssetClass = AssetClass (authorityTokenSymbol, atTokenName)
      mockEffectInputDatum :: Datum
      mockEffectInputDatum = unitDatum
      mockEffectOutputDatum :: Datum
      mockEffectOutputDatum = mockEffectInputDatum

      governorInputDatum :: GovernorDatum
      governorInputDatum =
        GovernorDatum
          { proposalThresholds = def
          , nextProposalId = ProposalId 5
          , proposalTimings = def
          , createProposalTimeRangeMaxWidth = def
          , maximumProposalsPerStake = 3
          }

      governorOutputDatum :: GovernorDatum
      governorOutputDatum = governorInputDatum

      builder :: SpendingBuilder
      builder =
        mconcat
          [ txId "9a12a605086a9f866731869a42d0558036fc739c74fea3849aa41562c015aaf9"
          , signedWith signer
          , mint burntGAT
          , fee $ Value.singleton "" "" 2
          , input $
              script govValidatorHash
                . withValue gst
                . withDatum governorInputDatum
                . withTxId "f867238a04597c99a0b9858746557d305025cca3b9f78ea14d5c88c4cfcf58ff"
          , input $
              script mockEffectHash
                . withValue gat
                . withDatum mockEffectInputDatum
                . withTxId "ecff06d7cf99089294569cc8b92609e44927278f9901730715d14634fbc10089"
          , output $
              script govValidatorHash
                . withValue (gst <> minAda)
                . withDatum governorOutputDatum
          , input $
              script mockEffectHash
                . withValue minAda
                . withDatum mockEffectOutputDatum
          , withSpending $
              script govValidatorHash
                . withValue gst
                . withDatum governorInputDatum
          ]
   in buildSpendingUnsafe builder
