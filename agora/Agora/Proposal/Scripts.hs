{- |
Module     : Agora.Proposal.Scripts
Maintainer : emi@haskell.fyi
Description: Plutus Scripts for Proposals.

Plutus Scripts for Proposals.
-}
module Agora.Proposal.Scripts (
  proposalValidator,
  proposalPolicy,
) where

import Agora.Proposal (
  PProposalDatum (PProposalDatum),
  PProposalRedeemer (..),
  PProposalStatus (..),
  PProposalVotes (PProposalVotes),
  Proposal (governorSTAssetClass, stakeSTAssetClass),
  ProposalStatus (..),
 )
import Agora.Proposal.Time (
  currentProposalTime,
  isDraftPeriod,
  isExecutionPeriod,
  isLockingPeriod,
  isVotingPeriod,
 )
import Agora.Stake (PProposalLock (..), PStakeDatum (..), findStakeOwnedBy)
import Agora.Utils (
  findTxOutByTxOutRef,
  getMintingPolicySymbol,
  mustBePJust,
  mustFindDatum',
 )
import Plutarch.Api.V1 (
  PMintingPolicy,
  PScriptContext (PScriptContext),
  PScriptPurpose (PMinting, PSpending),
  PTxInfo (PTxInfo),
  PValidator,
 )
import Plutarch.Api.V1.AssetClass (passetClass, passetClassValueOf)
import Plutarch.Api.V1.ScriptContext (
  pisTokenSpent,
  ptxSignedBy,
  pvalueSpent,
 )
import "liqwid-plutarch-extra" Plutarch.Api.V1.Value (psymbolValueOf)
import Plutarch.Extra.Comonad (pextract)
import Plutarch.Extra.List (pisUniqBy)
import Plutarch.Extra.Map (plookup, pupdate)
import Plutarch.Extra.Maybe (pisJust)
import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import Plutarch.Extra.TermCont (
  pguardC,
  pletC,
  pmatchC,
  ptryFromC,
 )
import Plutarch.SafeMoney (PDiscrete (..))
import PlutusLedgerApi.V1.Value (AssetClass (AssetClass))

{- | Policy for Proposals.

   == What this policy does

   === For minting:

   - Governor is happy with mint.

     * The governor must do most of the checking for the validity of the
       transaction. For example, the governor must check that the datum
       is correct, and that the ST is correctly paid to the right validator.

   - Exactly 1 token is minted.

   === For burning:

   - This policy cannot be burned.
-}
proposalPolicy ::
  -- | The assetclass of GST, see 'Agora.Governor.Scripts.governorPolicy'.
  AssetClass ->
  ClosedTerm PMintingPolicy
proposalPolicy (AssetClass (govCs, govTn)) =
  plam $ \_redeemer ctx' -> unTermCont $ do
    PScriptContext ctx' <- pmatchC ctx'
    ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
    PTxInfo txInfo' <- pmatchC $ pfromData ctx.txInfo
    txInfo <- tcont $ pletFields @'["inputs", "mint"] txInfo'
    PMinting _ownSymbol <- pmatchC $ pfromData ctx.purpose

    let inputs = txInfo.inputs
        mintedValue = pfromData txInfo.mint

    PMinting ownSymbol' <- pmatchC $ pfromData ctx.purpose
    let mintedProposalST =
          passetClassValueOf
            # mintedValue
            # (passetClass # (pfield @"_0" # ownSymbol') # pconstant "")

    pguardC "Governance state-thread token must move" $
      pisTokenSpent
        # (passetClass # pconstant govCs # pconstant govTn)
        # inputs

    pguardC "Minted exactly one proposal ST" $
      mintedProposalST #== 1

    pure $ popaque (pconstant ())

{- | The validator for Proposals.

The documentation for various of the redeemers lives at 'Agora.Proposal.ProposalRedeemer'.

== What this validator does

=== Voting/unlocking

When voting and unlocking, the proposal must witness a state transition
occuring in the relevant Stake. This transition must place a lock on
the stake that is tagged with the right 'Agora.Proposal.ResultTag', and 'Agora.Proposal.ProposalId'.

=== Periods

Most redeemers are time-sensitive.

A list of all time-sensitive redeemers and their requirements:

- 'Agora.Proposal.Vote' can only be used when both the status is in 'Agora.Proposal.VotingReady',
  and 'Agora.Proposal.Time.isVotingPeriod' is true.
- 'Agora.Proposal.Cosign' can only be used when both the status is in 'Agora.Proposal.Draft',
  and 'Agora.Proposal.Time.isDraftPeriod' is true.
- 'Agora.Proposal.AdvanceProposal' can only be used when the status can be advanced
  (see 'Agora.Proposal.AdvanceProposal' docs).
- 'Agora.Proposal.Unlock' is always valid.
-}
proposalValidator :: Proposal -> ClosedTerm PValidator
proposalValidator proposal =
  plam $ \datum redeemer ctx' -> unTermCont $ do
    PScriptContext ctx' <- pmatchC ctx'
    ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
    txInfo <- pletC $ pfromData ctx.txInfo
    PTxInfo txInfo' <- pmatchC txInfo
    txInfoF <-
      tcont $
        pletFields
          @'[ "inputs"
            , "outputs"
            , "mint"
            , "datums"
            , "signatories"
            , "validRange"
            ]
          txInfo'
    PSpending ((pfield @"_0" #) -> txOutRef) <- pmatchC $ pfromData ctx.purpose

    PJust txOut <- pmatchC $ findTxOutByTxOutRef # txOutRef # txInfoF.inputs
    txOutF <- tcont $ pletFields @'["address", "value"] $ txOut

    (pfromData -> proposalDatum, _) <-
      ptryFromC @(PAsData PProposalDatum) datum
    (pfromData -> proposalRedeemer, _) <-
      ptryFromC @(PAsData PProposalRedeemer) redeemer

    proposalF <-
      tcont $
        pletFields
          @'[ "proposalId"
            , "effects"
            , "status"
            , "cosigners"
            , "thresholds"
            , "votes"
            , "timingConfig"
            , "startingTime"
            ]
          proposalDatum

    ownAddress <- pletC $ txOutF.address

    let stCurrencySymbol =
          pconstant $ getMintingPolicySymbol (proposalPolicy proposal.governorSTAssetClass)
    valueSpent <- pletC $ pvalueSpent # txInfoF.inputs
    spentST <- pletC $ psymbolValueOf # stCurrencySymbol #$ valueSpent

    let AssetClass (stakeSym, stakeTn) = proposal.stakeSTAssetClass
    stakeSTAssetClass <-
      pletC $ passetClass # pconstant stakeSym # pconstant stakeTn
    spentStakeST <-
      pletC $ passetClassValueOf # valueSpent # stakeSTAssetClass

    signedBy <- pletC $ ptxSignedBy # txInfoF.signatories

    pguardC "ST at inputs must be 1" (spentST #== 1)

    currentTime <- pletC $ currentProposalTime # txInfoF.validRange

    -- Filter out own output with own address and PST.
    -- Delay the evaluation cause in some cases there won't be any continuing output.
    ownOutput <-
      pletC $
        mustBePJust # "Own output should be present" #$ pfind
          # plam
            ( \input -> unTermCont $ do
                inputF <- tcont $ pletFields @'["address", "value"] input
                pure $
                  inputF.address #== ownAddress
                    #&& psymbolValueOf # stCurrencySymbol # inputF.value #== 1
            )
          # pfromData txInfoF.outputs

    proposalOut <-
      pletC $
        mustFindDatum' @PProposalDatum
          # (pfield @"datumHash" # ownOutput)
          # txInfoF.datums

    pure $
      pmatch proposalRedeemer $ \case
        PVote r -> unTermCont $ do
          pguardC "Input proposal must be in VotingReady state" $
            proposalF.status #== pconstant VotingReady

          pguardC "Proposal time should be wthin the voting period" $
            isVotingPeriod # proposalF.timingConfig # proposalF.startingTime # currentTime

          -- Ensure the transaction is voting to a valid 'ResultTag'(outcome).
          PProposalVotes voteMap <- pmatchC proposalF.votes
          voteFor <- pletC $ pfromData $ pfield @"resultTag" # r

          pguardC "Vote option should be valid" $
            pisJust #$ plookup # voteFor # voteMap

          -- Find the input stake, the amount of new votes should be the 'stakedAmount'.
          let stakeInput =
                pfield @"resolved"
                  #$ mustBePJust
                  # "Stake input should be present"
                    #$ pfind
                  # plam
                    ( \(pfromData . (pfield @"value" #) . (pfield @"resolved" #) -> value) ->
                        passetClassValueOf # value # stakeSTAssetClass #== 1
                    )
                  # pfromData txInfoF.inputs

              stakeIn :: Term _ PStakeDatum
              stakeIn = mustFindDatum' # (pfield @"datumHash" # stakeInput) # txInfoF.datums

          stakeInF <- tcont $ pletFields @'["stakedAmount", "lockedBy", "owner"] stakeIn

          -- Ensure that no lock with the current proposal id has been put on the stake.
          pguardC "Same stake shouldn't vote on the same propsoal twice" $
            pnot #$ pany
              # plam
                ( \((pfield @"proposalTag" #) . pfromData -> pid) ->
                    pid #== proposalF.proposalId
                )
              # pfromData stakeInF.lockedBy

          let -- Update the vote counter of the proposal, and leave other stuff as is.
              expectedNewVotes = pmatch (pfromData proposalF.votes) $ \(PProposalVotes m) ->
                pcon $
                  PProposalVotes $
                    pupdate
                      # plam
                        ( \votes -> unTermCont $ do
                            PDiscrete v <- pmatchC stakeInF.stakedAmount
                            pure $ pcon $ PJust $ votes + (pextract # v)
                        )
                      # voteFor
                      # m
              expectedProposalOut =
                mkRecordConstr
                  PProposalDatum
                  ( #proposalId .= proposalF.proposalId
                      .& #effects .= proposalF.effects
                      .& #status .= proposalF.status
                      .& #cosigners .= proposalF.cosigners
                      .& #thresholds .= proposalF.thresholds
                      .& #votes .= pdata expectedNewVotes
                      .& #timingConfig .= proposalF.timingConfig
                      .& #startingTime .= proposalF.startingTime
                  )

          pguardC "Output proposal should be valid" $ proposalOut #== expectedProposalOut

          -- We validate the output stake datum here as well: We need the vote option
          -- to create a valid 'ProposalLock', however the vote option is encoded
          -- in the proposal redeemer, which is invisible for the stake validator.

          let stakeOutput =
                mustBePJust # "Stake output should be present"
                  #$ pfind
                  # plam
                    ( \(pfromData . (pfield @"value" #) -> value) ->
                        passetClassValueOf # value # stakeSTAssetClass #== 1
                    )
                  # pfromData txInfoF.outputs

              stakeOut :: Term _ PStakeDatum
              stakeOut = mustFindDatum' # (pfield @"datumHash" # stakeOutput) # txInfoF.datums

          let newProposalLock =
                mkRecordConstr
                  PProposalLock
                  ( #vote .= pdata voteFor
                      .& #proposalTag .= proposalF.proposalId
                  )
              -- Prepend the new lock to existing locks
              expectedProposalLocks =
                pcons
                  # pdata newProposalLock
                  # pfromData stakeInF.lockedBy
              expectedStakeOut =
                mkRecordConstr
                  PStakeDatum
                  ( #stakedAmount .= stakeInF.stakedAmount
                      .& #owner .= stakeInF.owner
                      .& #lockedBy .= pdata expectedProposalLocks
                  )

          pguardC "Output stake should be locked by the proposal" $ expectedStakeOut #== stakeOut

          pure $ popaque (pconstant ())
        --------------------------------------------------------------------------
        PCosign r -> unTermCont $ do
          newSigs <- pletC $ pfield @"newCosigners" # r

          pguardC "Cosigners are unique" $
            pisUniqBy
              # phoistAcyclic (plam (#==))
              # phoistAcyclic (plam $ \(pfromData -> x) (pfromData -> y) -> x #< y)
              # newSigs

          pguardC "Signed by all new cosigners" $
            pall # signedBy # newSigs

          pguardC "As many new cosigners as Stake datums" $
            spentStakeST #== plength # newSigs

          pguardC "All new cosigners are witnessed by their Stake datums" $
            pall
              # plam
                ( \sig ->
                    pmatch
                      ( findStakeOwnedBy # stakeSTAssetClass
                          # pfromData sig
                          # txInfoF.datums
                          # txInfoF.inputs
                      )
                      $ \case
                        PNothing -> pcon PFalse
                        PJust _ -> pcon PTrue
                )
              # newSigs

          let updatedSigs = pconcat # newSigs # proposalF.cosigners
              expectedDatum =
                mkRecordConstr
                  PProposalDatum
                  ( #proposalId .= proposalF.proposalId
                      .& #effects .= proposalF.effects
                      .& #status .= proposalF.status
                      .& #cosigners .= pdata updatedSigs
                      .& #thresholds .= proposalF.thresholds
                      .& #votes .= proposalF.votes
                      .& #timingConfig .= proposalF.timingConfig
                      .& #startingTime .= proposalF.startingTime
                  )

          pguardC "Signatures are correctly added to cosignature list" $
            proposalOut #== expectedDatum

          pure $ popaque (pconstant ())
        --------------------------------------------------------------------------
        PUnlock _r ->
          popaque (pconstant ())
        --------------------------------------------------------------------------
        PAdvanceProposal _r -> unTermCont $ do
          pguardC "No stake input is allowed" $ spentStakeST #== 0

          currentTime <- pletC $ currentProposalTime # txInfoF.validRange
          proposalOutStatus <- pletC $ pfield @"status" # proposalOut

          let -- Only the status of proposals should be updated in this case.
              templateProposalOut =
                mkRecordConstr
                  PProposalDatum
                  ( #proposalId .= proposalF.proposalId
                      .& #effects .= proposalF.effects
                      .& #status .= proposalOutStatus
                      .& #cosigners .= proposalF.cosigners
                      .& #thresholds .= proposalF.thresholds
                      .& #votes .= proposalF.votes
                      .& #timingConfig .= proposalF.timingConfig
                      .& #startingTime .= proposalF.startingTime
                  )

          pguardC "Only status changes in the output proposal" $
            templateProposalOut #== proposalOut

          inDraftPeriod <- pletC $ isDraftPeriod # proposalF.timingConfig # proposalF.startingTime # currentTime
          inVotingPeriod <- pletC $ isVotingPeriod # proposalF.timingConfig # proposalF.startingTime # currentTime
          inLockedPeriod <- pletC $ isLockingPeriod # proposalF.timingConfig # proposalF.startingTime # currentTime
          inExecutionPeriod <- pletC $ isExecutionPeriod # proposalF.timingConfig # proposalF.startingTime # currentTime

          -- Check the timings.
          let isFinished = proposalF.status #== pconstantData Finished

              notTooLate = pmatch (pfromData proposalF.status) $ \case
                PDraft _ -> inDraftPeriod
                -- Can only advance after the voting period is over.
                PVotingReady _ -> inLockedPeriod
                PLocked _ -> inExecutionPeriod
                _ -> pconstant False

              notTooEarly = pmatch (pfromData proposalF.status) $ \case
                PVotingReady _ -> pnot # inVotingPeriod
                PLocked _ -> pnot # inLockedPeriod
                _ -> pconstant True

          pguardC "Cannot advance ahead of time" notTooEarly
          pguardC "Finished proposals cannot be advanced" $ pnot # isFinished

          pure $
            pif
              notTooLate
              -- On time: advance to next status.
              ( pmatch (pfromData proposalF.status) $ \case
                  PDraft _ -> unTermCont $ do
                    -- TODO: Perform other necessary checks.

                    -- 'Draft' -> 'VotingReady'
                    pguardC "Proposal status set to VotingReady" $
                      proposalOutStatus #== pconstantData VotingReady

                    pure $ popaque (pconstant ())
                  PVotingReady _ -> unTermCont $ do
                    -- 'VotingReady' -> 'Locked'
                    pguardC "Proposal status set to Locked" $
                      proposalOutStatus #== pconstantData Locked

                    pure $ popaque (pconstant ())
                  PLocked _ -> unTermCont $ do
                    -- 'Locked' -> 'Finished'
                    pguardC "Proposal status set to Finished" $
                      proposalOutStatus #== pconstantData Finished

                    -- TODO: Perform other necessary checks.
                    pure $ popaque (pconstant ())
                  _ -> popaque (pconstant ())
              )
              -- Too late: failed proposal, status set to 'Finished'.
              ( popaque $
                  ptraceIfFalse "Proposal should fail: not on time" $
                    proposalOutStatus #== pconstantData Finished
                    -- TODO: Should check that the GST is not moved
                    --        if the proposal is in 'Locked' state.
              )
