{- |
Module     : Spec.Sample.Governor
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

--------------------------------------------------------------------------------

import Data.Tagged (Tagged (..), untag)
import Plutarch.Api.V1 (mkValidator, validatorHash)

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Api (
  Address (..),
  Credential (ScriptCredential),
  Datum (..),
  ScriptContext (..),
  ScriptPurpose (Minting, Spending),
  ToData (toBuiltinData),
  TokenName (..),
  TxInInfo (TxInInfo),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
  Validator,
  ValidatorHash (..),
 )
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Scripts (unitDatum)
import Plutus.V1.Ledger.Value (
  AssetClass (..),
 )
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.AssocMap qualified as AssocMap

--------------------------------------------------------------------------------

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
import Agora.Proposal qualified as P
import Agora.Stake (
  ProposalLock (..),
  Stake (..),
  StakeDatum (..),
 )

--------------------------------------------------------------------------------

import Sample.Shared (
  authorityTokenSymbol,
  defaultProposalThresholds,
  govAssetClass,
  govSymbol,
  govValidatorAddress,
  gstUTXORef,
  minAda,
  proposalPolicySymbol,
  proposalTimingConfig,
  proposalValidatorAddress,
  signer,
  signer2,
  stake,
  stakeAddress,
  stakeAssetClass,
  tmpProposalStartingTime,
 )
import Test.Util (datumPair, toDatumHash)

--------------------------------------------------------------------------------

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

      ---

      governorOutputDatum' :: GovernorDatum
      governorOutputDatum' =
        GovernorDatum
          { proposalThresholds = defaultProposalThresholds
          , nextProposalId = ProposalId 0
          }
      governorOutputDatum :: Datum
      governorOutputDatum = Datum $ toBuiltinData governorOutputDatum'
      governorOutput :: TxOut
      governorOutput =
        TxOut
          { txOutAddress = govValidatorAddress
          , txOutValue = gst <> minAda
          , txOutDatumHash = Just $ toDatumHash governorOutputDatum
          }

      ---

      witness :: ValidatorHash
      witness = "a926a9a72a0963f428e3252caa8354e655603996fb8892d6b8323fd072345924"
      witnessAddress :: Address
      witnessAddress = Address (ScriptCredential witness) Nothing

      ---

      -- The witness UTXO must be consumed.
      witnessInput :: TxOut
      witnessInput =
        TxOut
          { txOutAddress = witnessAddress
          , txOutValue = mempty
          , txOutDatumHash = Nothing
          }
      initialSpend :: TxInInfo
      initialSpend = TxInInfo gstUTXORef witnessInput
   in ScriptContext
        { scriptContextTxInfo =
            TxInfo
              { txInfoInputs =
                  [ initialSpend
                  ]
              , txInfoOutputs = [governorOutput]
              , -- Some ada to cover the transaction fee
                txInfoFee = Value.singleton "" "" 2
              , -- Exactly one GST is minted
                txInfoMint = gst
              , txInfoDCert = []
              , txInfoWdrl = []
              , txInfoValidRange = Interval.always
              , txInfoSignatories = [signer]
              , txInfoData = [datumPair governorOutputDatum]
              , txInfoId = "90906d3e6b4d6dec2e747dcdd9617940ea8358164c7244694cfa39dec18bd9d4"
              }
        , scriptContextPurpose = Minting govSymbol
        }

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

      ---

      governorInputDatum' :: GovernorDatum
      governorInputDatum' =
        GovernorDatum
          { proposalThresholds = defaultProposalThresholds
          , nextProposalId = thisProposalId
          }
      governorInputDatum :: Datum
      governorInputDatum = Datum $ toBuiltinData governorInputDatum'
      governorInput :: TxOut
      governorInput =
        TxOut
          { txOutAddress = govValidatorAddress
          , txOutValue = gst
          , txOutDatumHash = Just $ toDatumHash governorInputDatum
          }

      ---

      effects =
        AssocMap.fromList
          [ (ResultTag 0, AssocMap.empty)
          , (ResultTag 1, AssocMap.empty)
          ]
      proposalDatum :: Datum
      proposalDatum =
        Datum
          ( toBuiltinData $
              ProposalDatum
                { P.proposalId = ProposalId 0
                , effects = effects
                , status = Draft
                , cosigners = [signer]
                , thresholds = defaultProposalThresholds
                , votes = emptyVotesFor effects
                , timingConfig = proposalTimingConfig
                , startingTime = tmpProposalStartingTime
                }
          )
      proposalOutput :: TxOut
      proposalOutput =
        TxOut
          { txOutAddress = proposalValidatorAddress
          , txOutValue = pst <> minAda
          , txOutDatumHash = Just (toDatumHash proposalDatum)
          }

      ---

      stakeInputDatum' :: StakeDatum
      stakeInputDatum' =
        StakeDatum
          { stakedAmount = Tagged stackedGTs
          , owner = signer
          , lockedBy = []
          }
      stakeInputDatum :: Datum
      stakeInputDatum = Datum $ toBuiltinData stakeInputDatum'
      stakeInput :: TxOut
      stakeInput =
        TxOut
          { txOutAddress = stakeAddress
          , txOutValue = sst <> Value.assetClassValue (untag stake.gtClassRef) stackedGTs
          , txOutDatumHash = Just (toDatumHash stakeInputDatum)
          }

      ---
      governorOutputDatum' :: GovernorDatum
      governorOutputDatum' = governorInputDatum' {nextProposalId = getNextProposalId thisProposalId}
      governorOutputDatum :: Datum
      governorOutputDatum = Datum $ toBuiltinData governorOutputDatum'
      governorOutput :: TxOut
      governorOutput =
        governorInput
          { txOutDatumHash = Just $ toDatumHash governorOutputDatum
          , txOutValue = gst <> minAda
          }

      ---

      proposalLocks :: [ProposalLock]
      proposalLocks =
        [ ProposalLock (ResultTag 0) thisProposalId
        , ProposalLock (ResultTag 1) thisProposalId
        ]
      stakeOutputDatum' :: StakeDatum
      stakeOutputDatum' = stakeInputDatum' {lockedBy = proposalLocks}
      stakeOutputDatum :: Datum
      stakeOutputDatum = Datum $ toBuiltinData stakeOutputDatum'
      stakeOutput :: TxOut
      stakeOutput =
        stakeInput
          { txOutDatumHash = Just $ toDatumHash stakeOutputDatum
          , txOutValue = sst <> Value.assetClassValue (untag stake.gtClassRef) stackedGTs <> minAda
          }

      ---
      ownInputRef :: TxOutRef
      ownInputRef = TxOutRef "4355a46b19d348dc2f57c046f8ef63d4538ebb936000f3c9ee954a27460dd865" 1
   in ScriptContext
        { scriptContextTxInfo =
            TxInfo
              { txInfoInputs =
                  [ TxInInfo
                      ownInputRef
                      governorInput
                  , TxInInfo
                      (TxOutRef "4262bbd0b3fc926b74eaa8abab5def6ce5e6b94f19cf221c02a16e7da8cd470f" 1)
                      stakeInput
                  ]
              , txInfoOutputs = [proposalOutput, governorOutput, stakeOutput]
              , txInfoFee = Value.singleton "" "" 2
              , txInfoMint = pst
              , txInfoDCert = []
              , txInfoWdrl = []
              , txInfoValidRange = Interval.always
              , txInfoSignatories = [signer]
              , txInfoData =
                  datumPair
                    <$> [ governorInputDatum
                        , governorOutputDatum
                        , proposalDatum
                        , stakeInputDatum
                        , stakeOutputDatum
                        ]
              , txInfoId = "1ffb9669335c908d9a4774a4bf7aa7bfafec91d015249b4138bc83fde4a3330a"
              }
        , scriptContextPurpose = Spending ownInputRef
        }

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

      ---

      mockEffect :: Validator
      mockEffect = mkValidator $ noOpValidator ""
      mockEffectHash :: ValidatorHash
      mockEffectHash = validatorHash mockEffect
      mockEffectAddress :: Address
      mockEffectAddress = scriptHashAddress mockEffectHash
      mockEffectOutputDatum :: Datum
      mockEffectOutputDatum = unitDatum
      atTokenName :: TokenName
      atTokenName = TokenName hash
        where
          ValidatorHash hash = mockEffectHash
      atAssetClass :: AssetClass
      atAssetClass = AssetClass (authorityTokenSymbol, atTokenName)

      ---

      governorInputDatum' :: GovernorDatum
      governorInputDatum' =
        GovernorDatum
          { proposalThresholds = defaultProposalThresholds
          , nextProposalId = ProposalId 5
          }
      governorInputDatum :: Datum
      governorInputDatum = Datum $ toBuiltinData governorInputDatum'
      governorInput :: TxOut
      governorInput =
        TxOut
          { txOutAddress = govValidatorAddress
          , txOutValue = gst
          , txOutDatumHash = Just $ toDatumHash governorInputDatum
          }

      ---

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
      proposalInputDatum' :: ProposalDatum
      proposalInputDatum' =
        ProposalDatum
          { P.proposalId = ProposalId 0
          , effects = effects
          , status = Locked
          , cosigners = [signer, signer2]
          , thresholds = defaultProposalThresholds
          , votes = proposalVotes
          , timingConfig = proposalTimingConfig
          , startingTime = tmpProposalStartingTime
          }
      proposalInputDatum :: Datum
      proposalInputDatum = Datum $ toBuiltinData proposalInputDatum'
      proposalInput :: TxOut
      proposalInput =
        TxOut
          { txOutAddress = proposalValidatorAddress
          , txOutValue = pst
          , txOutDatumHash = Just (toDatumHash proposalInputDatum)
          }

      ---

      governorOutputDatum' :: GovernorDatum
      governorOutputDatum' = governorInputDatum'
      governorOutputDatum :: Datum
      governorOutputDatum = Datum $ toBuiltinData governorOutputDatum'
      governorOutput :: TxOut
      governorOutput =
        governorInput
          { txOutDatumHash = Just $ toDatumHash governorOutputDatum
          , txOutValue = gst <> minAda
          }

      ---

      proposalOutputDatum' :: ProposalDatum
      proposalOutputDatum' = proposalInputDatum' {status = Finished}
      proposalOutputDatum :: Datum
      proposalOutputDatum = Datum $ toBuiltinData proposalOutputDatum'
      proposalOutput :: TxOut
      proposalOutput =
        proposalInput
          { txOutDatumHash = Just $ toDatumHash proposalOutputDatum
          , txOutValue = pst <> minAda
          }

      --

      mockEffectOutput :: TxOut
      mockEffectOutput =
        TxOut
          { txOutAddress = mockEffectAddress
          , txOutDatumHash = Just $ toDatumHash mockEffectOutputDatum
          , txOutValue = gat <> minAda
          }

      --

      ownInputRef :: TxOutRef
      ownInputRef = TxOutRef "4355a46b19d348dc2f57c046f8ef63d4538ebb936000f3c9ee954a27460dd865" 1
   in ScriptContext
        { scriptContextTxInfo =
            TxInfo
              { txInfoInputs =
                  [ TxInInfo ownInputRef governorInput
                  , TxInInfo
                      (TxOutRef "11b2162f267614b803761032b6333040fc61478ae788c088614ee9487ab0c1b7" 1)
                      proposalInput
                  ]
              , txInfoOutputs =
                  [ governorOutput
                  , proposalOutput
                  , mockEffectOutput
                  ]
              , txInfoFee = Value.singleton "" "" 2
              , txInfoMint = gat
              , txInfoDCert = []
              , txInfoWdrl = []
              , txInfoValidRange = Interval.always
              , txInfoSignatories = [signer, signer2]
              , txInfoData =
                  datumPair
                    <$> [ governorInputDatum
                        , governorOutputDatum
                        , proposalInputDatum
                        , proposalOutputDatum
                        , mockEffectOutputDatum
                        ]
              , txInfoId = "ff755f613c1f7487dfbf231325c67f481f7a97e9faf4d8b09ad41176fd65cbe7"
              }
        , scriptContextPurpose = Spending ownInputRef
        }

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

      ---

      -- TODO: Use the *real* effect, see https://github.com/Liqwid-Labs/agora/pull/62

      mockEffect :: Validator
      mockEffect = mkValidator $ noOpValidator ""
      mockEffectHash :: ValidatorHash
      mockEffectHash = validatorHash mockEffect
      mockEffectAddress :: Address
      mockEffectAddress = scriptHashAddress mockEffectHash
      atTokenName :: TokenName
      atTokenName = TokenName hash
        where
          ValidatorHash hash = mockEffectHash
      atAssetClass :: AssetClass
      atAssetClass = AssetClass (authorityTokenSymbol, atTokenName)

      --

      mockEffectInputDatum :: Datum
      mockEffectInputDatum = unitDatum
      mockEffectInput :: TxOut
      mockEffectInput =
        TxOut
          { txOutAddress = mockEffectAddress
          , txOutValue = gat -- Will be burnt
          , txOutDatumHash = Just $ toDatumHash mockEffectInputDatum
          }

      --

      mockEffectOutputDatum :: Datum
      mockEffectOutputDatum = mockEffectInputDatum
      mockEffectOutput :: TxOut
      mockEffectOutput =
        mockEffectInput
          { txOutValue = minAda
          , txOutDatumHash = Just $ toDatumHash mockEffectOutputDatum
          }

      --

      governorInputDatum' :: GovernorDatum
      governorInputDatum' =
        GovernorDatum
          { proposalThresholds = defaultProposalThresholds
          , nextProposalId = ProposalId 5
          }
      governorInputDatum :: Datum
      governorInputDatum = Datum $ toBuiltinData governorInputDatum'
      governorInput :: TxOut
      governorInput =
        TxOut
          { txOutAddress = govValidatorAddress
          , txOutValue = gst
          , txOutDatumHash = Just $ toDatumHash governorInputDatum
          }

      --

      governorOutputDatum' :: GovernorDatum
      governorOutputDatum' = governorInputDatum'
      governorOutputDatum :: Datum
      governorOutputDatum = Datum $ toBuiltinData governorOutputDatum'
      governorOutput :: TxOut
      governorOutput =
        governorInput
          { txOutDatumHash = Just $ toDatumHash governorOutputDatum
          , txOutValue = gst <> minAda
          }

      --

      ownInputRef :: TxOutRef
      ownInputRef = TxOutRef "f867238a04597c99a0b9858746557d305025cca3b9f78ea14d5c88c4cfcf58ff" 1
   in ScriptContext
        { scriptContextTxInfo =
            TxInfo
              { txInfoInputs =
                  [ TxInInfo ownInputRef governorInput
                  , TxInInfo
                      (TxOutRef "ecff06d7cf99089294569cc8b92609e44927278f9901730715d14634fbc10089" 1)
                      mockEffectInput
                  ]
              , txInfoOutputs =
                  [ governorOutput
                  , mockEffectOutput
                  ]
              , txInfoFee = Value.singleton "" "" 2
              , txInfoMint = burntGAT
              , txInfoDCert = []
              , txInfoWdrl = []
              , txInfoValidRange = Interval.always
              , txInfoSignatories = [signer]
              , txInfoData =
                  datumPair
                    <$> [ governorInputDatum
                        , governorOutputDatum
                        , mockEffectInputDatum
                        , mockEffectOutputDatum
                        ]
              , txInfoId = "9a12a605086a9f866731869a42d0558036fc739c74fea3849aa41562c015aaf9"
              }
        , scriptContextPurpose = Spending ownInputRef
        }
