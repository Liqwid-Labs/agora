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
  PProposalVotes (PProposalVotes),
  Proposal (governorSTAssetClass, stakeSTAssetClass),
  ProposalStatus (VotingReady),
 )
import Agora.Proposal.Time (currentProposalTime, isVotingPeriod)
import Agora.Record (mkRecordConstr, (.&), (.=))
import Agora.Stake (PProposalLock (..), PStakeDatum (..), findStakeOwnedBy)
import Agora.Utils (
  anyOutput,
  findTxOutByTxOutRef,
  getMintingPolicySymbol,
  mustBePJust,
  mustFindDatum',
  pisJust,
  pisUniqBy,
  psymbolValueOf,
  ptokenSpent,
  ptxSignedBy,
  pupdate,
  pvalueSpent,
  tcassert,
  tclet,
  tcmatch,
  tctryFrom,
 )
import Plutarch.Api.V1 (
  PMintingPolicy,
  PScriptContext (PScriptContext),
  PScriptPurpose (PMinting, PSpending),
  PTxInfo (PTxInfo),
  PValidator,
 )
import Plutarch.Api.V1.Extra (passetClass, passetClassValueOf)
import Plutarch.Map.Extra (plookup)
import Plutarch.SafeMoney (puntag)
import Plutus.V1.Ledger.Value (AssetClass (AssetClass))

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
    PScriptContext ctx' <- tcmatch ctx'
    ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
    PTxInfo txInfo' <- tcmatch $ pfromData ctx.txInfo
    txInfo <- tcont $ pletFields @'["inputs", "mint"] txInfo'
    PMinting _ownSymbol <- tcmatch $ pfromData ctx.purpose

    let inputs = txInfo.inputs
        mintedValue = pfromData txInfo.mint

    PMinting ownSymbol' <- tcmatch $ pfromData ctx.purpose
    let mintedProposalST =
          passetClassValueOf
            # mintedValue
            # (passetClass # (pfield @"_0" # ownSymbol') # pconstant "")

    tcassert "Governance state-thread token must move" $
      ptokenSpent
        # (passetClass # pconstant govCs # pconstant govTn)
        # inputs

    tcassert "Minted exactly one proposal ST" $
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
    PScriptContext ctx' <- tcmatch ctx'
    ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
    txInfo <- tclet $ pfromData ctx.txInfo
    PTxInfo txInfo' <- tcmatch txInfo
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
    PSpending ((pfield @"_0" #) -> txOutRef) <- tcmatch $ pfromData ctx.purpose

    PJust txOut <- tcmatch $ findTxOutByTxOutRef # txOutRef # txInfoF.inputs
    txOutF <- tcont $ pletFields @'["address", "value"] $ txOut

    (pfromData -> proposalDatum, _) <-
      tctryFrom @(PAsData PProposalDatum) datum
    (pfromData -> proposalRedeemer, _) <-
      tctryFrom @(PAsData PProposalRedeemer) redeemer

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

    ownAddress <- tclet $ txOutF.address

    let stCurrencySymbol =
          pconstant $ getMintingPolicySymbol (proposalPolicy proposal.governorSTAssetClass)
    valueSpent <- tclet $ pvalueSpent # txInfoF.inputs
    spentST <- tclet $ psymbolValueOf # stCurrencySymbol #$ valueSpent

    let AssetClass (stakeSym, stakeTn) = proposal.stakeSTAssetClass
    stakeSTAssetClass <-
      tclet $ passetClass # pconstant stakeSym # pconstant stakeTn
    spentStakeST <-
      tclet $ passetClassValueOf # valueSpent # stakeSTAssetClass

    signedBy <- tclet $ ptxSignedBy # txInfoF.signatories

    tcassert "ST at inputs must be 1" (spentST #== 1)

    currentTime <- tclet $ currentProposalTime # txInfoF.validRange

    pure $
      pmatch proposalRedeemer $ \case
        PVote r -> unTermCont $ do
          tcassert "Input proposal must be in VotingReady state" $
            proposalF.status #== pconstant VotingReady

          tcassert "Proposal time should be wthin the voting period" $
            isVotingPeriod # proposalF.timingConfig # proposalF.startingTime # currentTime

          -- Ensure the transaction is voting to a valid 'ResultTag'(outcome).
          PProposalVotes voteMap <- tcmatch proposalF.votes
          voteFor <- tclet $ pfromData $ pfield @"resultTag" # r

          tcassert "Invalid vote option" $
            pisJust #$ plookup # voteFor # voteMap

          -- Find the input stake, the amount of new votes should be the 'stakedAmount'.
          let stakeInput =
                pfield @"resolved"
                  #$ mustBePJust
                  # "Stake input not found"
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
          tcassert "Cannot vote on the a proposal using the same stake twice" $
            pnot #$ pany
              # plam
                ( \((pfield @"proposalTag" #) . pfromData -> pid) ->
                    pid #== proposalF.proposalId
                )
              # pfromData stakeInF.lockedBy

          -- TODO: maybe we can move this outside of the pmatch block.
          -- Filter out own output with own address and PST.
          let ownOutput =
                mustBePJust # "Own output not found" #$ pfind
                  # plam
                    ( \input -> unTermCont $ do
                        inputF <- tcont $ pletFields @'["address", "value"] input
                        pure $
                          inputF.address #== ownAddress
                            #&& psymbolValueOf # stCurrencySymbol # inputF.value #== 1
                    )
                  # pfromData txInfoF.outputs

              proposalOut :: Term _ PProposalDatum
              proposalOut = mustFindDatum' # (pfield @"datumHash" # ownOutput) # txInfoF.datums

          let -- Update the vote counter of the proposal, and leave other stuff as is.
              expectedNewVotes = pmatch (pfromData proposalF.votes) $ \(PProposalVotes m) ->
                pcon $
                  PProposalVotes $
                    pupdate
                      # plam
                        ( \votes ->
                            pcon $ PJust $ votes + (puntag stakeInF.stakedAmount)
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

          tcassert "Invalid output proposal" $ proposalOut #== expectedProposalOut

          -- We validate the output stake datum here as well: We need the vote option
          -- to create a valid 'ProposalLock', however the vote option is encoded
          -- in the proposal redeemer, which is invisible for the stake validator.

          let stakeOutput =
                mustBePJust # "Stake output not found"
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

          tcassert "Output stake should be locked by the proposal" $ expectedStakeOut #== stakeOut

          pure $ popaque (pconstant ())
        --------------------------------------------------------------------------
        PCosign r -> unTermCont $ do
          newSigs <- tclet $ pfield @"newCosigners" # r

          tcassert "Cosigners are unique" $
            pisUniqBy
              # phoistAcyclic (plam (#==))
              # phoistAcyclic (plam $ \(pfromData -> x) (pfromData -> y) -> x #< y)
              # newSigs

          tcassert "Signed by all new cosigners" $
            pall # signedBy # newSigs

          tcassert "As many new cosigners as Stake datums" $
            spentStakeST #== plength # newSigs

          tcassert "All new cosigners are witnessed by their Stake datums" $
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

          tcassert "Signatures are correctly added to cosignature list" $
            anyOutput @PProposalDatum # ctx.txInfo
              #$ plam
              $ \newValue address newProposalDatum ->
                let updatedSigs = pconcat # newSigs # proposalF.cosigners
                    correctDatum =
                      pdata newProposalDatum
                        #== pdata
                          ( mkRecordConstr
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
                          )
                 in foldr1
                      (#&&)
                      [ ptraceIfFalse "Datum must be correct" correctDatum
                      , ptraceIfFalse "Value should be correct" $
                          pdata txOutF.value #== pdata newValue
                      , ptraceIfFalse "Must be sent to Proposal's address" $
                          ownAddress #== pdata address
                      ]

          pure $ popaque (pconstant ())
        --------------------------------------------------------------------------
        PUnlock _r ->
          popaque (pconstant ())
        --------------------------------------------------------------------------
        PAdvanceProposal _r ->
          popaque (pconstant ())
