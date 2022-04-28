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
  passert,
  pisUniq,
  psymbolValueOf,
  ptokenSpent,
  ptxSignedBy,
  pvalueSpent,
 )
import Plutarch.Api.V1 (
  PMintingPolicy,
  PScriptContext (PScriptContext),
  PScriptPurpose (PMinting, PSpending),
  PTxInfo (PTxInfo),
  PValidator,
 )
import Plutarch.Api.V1.Extra (passetClass, passetClassValueOf)
import Plutarch.Monadic qualified as P
import Plutarch.TryFrom (ptryFrom)
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
proposalPolicy :: Proposal -> ClosedTerm PMintingPolicy
proposalPolicy proposal =
  plam $ \_redeemer ctx' -> P.do
    PScriptContext ctx' <- pmatch ctx'
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    PTxInfo txInfo' <- pmatch $ pfromData ctx.txInfo
    txInfo <- pletFields @'["inputs", "mint"] txInfo'
    PMinting _ownSymbol <- pmatch $ pfromData ctx.purpose

    let inputs = txInfo.inputs
        mintedValue = pfromData txInfo.mint
        AssetClass (govCs, govTn) = proposal.governorSTAssetClass

    PMinting ownSymbol' <- pmatch $ pfromData ctx.purpose
    let mintedProposalST =
          passetClassValueOf
            # mintedValue
            # (passetClass # (pfield @"_0" # ownSymbol') # pconstant "")

    passert "Governance state-thread token must move" $
      ptokenSpent
        # (passetClass # pconstant govCs # pconstant govTn)
        # inputs

    passert "Minted exactly one proposal ST" $
      mintedProposalST #== 1

    popaque (pconstant ())

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
  plam $ \datum redeemer ctx' -> P.do
    PScriptContext ctx' <- pmatch ctx'
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    txInfo <- plet $ pfromData ctx.txInfo
    PTxInfo txInfo' <- pmatch txInfo
    txInfoF <- pletFields @'["inputs", "mint", "datums", "signatories"] txInfo'
    PSpending ((pfield @"_0" #) -> txOutRef) <- pmatch $ pfromData ctx.purpose

    PJust txOut <- pmatch $ findTxOutByTxOutRef # txOutRef # txInfoF.inputs
    txOutF <- pletFields @'["address", "value"] $ txOut

    (pfromData -> proposalDatum, _) <-
      ptryFrom @(PAsData PProposalDatum) datum
    (pfromData -> proposalRedeemer, _) <-
      ptryFrom @(PAsData PProposalRedeemer) redeemer

    proposalF <-
      pletFields
        @'[ "proposalId"
          , "effects"
          , "status"
          , "cosigners"
          , "thresholds"
          , "votes"
          ]
        proposalDatum

    ownAddress <- plet $ txOutF.address

    let stCurrencySymbol =
          pconstant $ getMintingPolicySymbol (proposalPolicy proposal)
    valueSpent <- plet $ pvalueSpent # txInfoF.inputs
    spentST <- plet $ psymbolValueOf # stCurrencySymbol #$ valueSpent
    let AssetClass (stakeSym, stakeTn) = proposal.stakeSTAssetClass
    stakeSTAssetClass <-
      plet $ passetClass # pconstant stakeSym # pconstant stakeTn
    spentStakeST <-
      plet $ passetClassValueOf # valueSpent # stakeSTAssetClass

    signedBy <- plet $ ptxSignedBy # txInfoF.signatories

    passert "ST at inputs must be 1" $
      spentST #== 1

    pmatch proposalRedeemer $ \case
      PVote _r -> P.do
        popaque (pconstant ())
      --------------------------------------------------------------------------
      PCosign r -> P.do
        newSigs <- plet $ pfield @"newCosigners" # r

        passert "Cosigners are unique" $
          pisUniq # newSigs

        passert "Signed by all new cosigners" $
          pall # signedBy # newSigs

        passert "As many new cosigners as Stake datums" $
          spentStakeST #== plength # newSigs

        passert "All new cosigners are witnessed by their Stake datums" $
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

        passert "Signatures are correctly added to cosignature list" $
          anyOutput @PProposalDatum # ctx.txInfo
            #$ plam
            $ \newValue address newProposalDatum -> P.do
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

              foldr1
                (#&&)
                [ pcon PTrue
                , ptraceIfFalse "Datum must be correct" correctDatum
                , ptraceIfFalse "Value should be correct" $
                    pdata txOutF.value #== pdata newValue
                , ptraceIfFalse "Must be sent to Proposal's address" $
                    ownAddress #== pdata address
                ]

        popaque (pconstant ())
      --------------------------------------------------------------------------
      PUnlock _r -> P.do
        popaque (pconstant ())
      --------------------------------------------------------------------------
      PAdvanceProposal _r -> P.do
        popaque (pconstant ())
