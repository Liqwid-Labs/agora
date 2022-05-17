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
  Proposal (governorSTAssetClass, stakeSTAssetClass),
 )
import Agora.Record (mkRecordConstr, (.&), (.=))
import Agora.Stake (findStakeOwnedBy)
import Agora.Utils (
  anyOutput,
  findTxOutByTxOutRef,
  getMintingPolicySymbol,
  pisUniqBy,
  psymbolValueOf,
  ptokenSpent,
  ptxSignedBy,
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
    txInfoF <- tcont $ pletFields @'["inputs", "mint", "datums", "signatories"] txInfo'
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

    pure $
      pmatch proposalRedeemer $ \case
        PVote _r -> popaque (pconstant ())
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
