{- |
Module     : Agora.Proposal.Scripts
Maintainer : emi@haskell.fyi
Description: Plutus Scripts for Proposals.

Plutus Scripts for Proposals.
-}
module Agora.Proposal.Scripts (
  proposalValidator,
  proposalPolicy,
  proposalDatumValid,
) where

import Agora.Proposal (
  PProposalDatum (PProposalDatum),
  PProposalRedeemer (..),
  PResultTag,
  Proposal (governorSTAssetClass, stakeSTAssetClass),
 )
import Agora.Record (mkRecordConstr, (.&), (.=))
import Agora.Stake (findStakeOwnedBy)
import Agora.Utils (
  anyOutput,
  findTxOutByTxOutRef,
  passert,
  pnotNull,
  psymbolValueOf,
  ptokenSpent,
  ptxSignedBy,
  pvalueSpent,
 )
import Plutarch.Api.V1 (
  PDatumHash,
  PMintingPolicy,
  PScriptContext (PScriptContext),
  PScriptPurpose (PMinting, PSpending),
  PTxInfo (PTxInfo),
  PValidator,
  PValidatorHash,
  mintingPolicySymbol,
  mkMintingPolicy,
 )
import Plutarch.Api.V1.Extra (passetClass, passetClassValueOf)
import Plutarch.Builtin (PBuiltinMap)
import Plutarch.Monadic qualified as P
import Plutarch.Unsafe (punsafeCoerce)
import Plutus.V1.Ledger.Value (AssetClass (AssetClass))

{- | Policy for Proposals.
   This needs to perform two checks:
     - Governor is happy with mint.
     - Exactly 1 token is minted.

   NOTE: The governor needs to check that the datum is correct
         and sent to the right address.
-}
proposalPolicy :: Agora.Proposal.Proposal -> ClosedTerm Plutarch.Api.V1.PMintingPolicy
proposalPolicy proposal =
  plam $ \_redeemer ctx' -> P.do
    Plutarch.Api.V1.PScriptContext ctx' <- pmatch ctx'
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    Plutarch.Api.V1.PTxInfo txInfo' <- pmatch $ pfromData ctx.txInfo
    txInfo <- pletFields @'["inputs", "mint"] txInfo'
    Plutarch.Api.V1.PMinting _ownSymbol <- pmatch $ pfromData ctx.purpose

    let inputs = txInfo.inputs
        mintedValue = pfromData txInfo.mint
        AssetClass (govCs, govTn) = proposal.governorSTAssetClass

    Plutarch.Api.V1.PMinting ownSymbol' <- pmatch $ pfromData ctx.purpose
    let mintedProposalST = passetClassValueOf # mintedValue # (passetClass # (pfield @"_0" # ownSymbol') # pconstant "")

    passert "Governance state-thread token must move" $
      ptokenSpent
        # (passetClass # pconstant govCs # pconstant govTn)
        # inputs

    passert "Minted exactly one proposal ST" $
      mintedProposalST #== 1

    popaque (pconstant ())

-- | Validator for Proposals.
proposalValidator :: Agora.Proposal.Proposal -> ClosedTerm Plutarch.Api.V1.PValidator
proposalValidator proposal =
  plam $ \datum redeemer ctx' -> P.do
    Plutarch.Api.V1.PScriptContext ctx' <- pmatch ctx'
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    txInfo <- plet $ pfromData ctx.txInfo
    Plutarch.Api.V1.PTxInfo txInfo' <- pmatch txInfo
    txInfoF <- pletFields @'["inputs", "mint", "datums", "signatories"] txInfo'
    Plutarch.Api.V1.PSpending ((pfield @"_0" #) -> txOutRef) <- pmatch $ pfromData ctx.purpose

    PJust txOut <- pmatch $ findTxOutByTxOutRef # txOutRef # txInfoF.inputs
    txOutF <- pletFields @'["address", "value"] $ txOut

    let proposalDatum :: Term _ Agora.Proposal.PProposalDatum
        proposalDatum = pfromData $ punsafeCoerce datum
        proposalRedeemer :: Term _ Agora.Proposal.PProposalRedeemer
        proposalRedeemer = pfromData $ punsafeCoerce redeemer

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

    stCurrencySymbol <- plet $ pconstant $ Plutarch.Api.V1.mintingPolicySymbol $ Plutarch.Api.V1.mkMintingPolicy (proposalPolicy proposal)
    valueSpent <- plet $ pvalueSpent # txInfoF.inputs
    spentST <- plet $ psymbolValueOf # stCurrencySymbol #$ valueSpent
    let AssetClass (stakeSym, stakeTn) = proposal.stakeSTAssetClass
    stakeSTAssetClass <- plet $ passetClass # pconstant stakeSym # pconstant stakeTn
    spentStakeST <- plet $ passetClassValueOf # valueSpent # stakeSTAssetClass

    signedBy <- plet $ ptxSignedBy # txInfoF.signatories

    pmatch proposalRedeemer $ \case
      Agora.Proposal.PVote _r -> P.do
        passert "ST at inputs must be 1" $
          spentST #== 1

        popaque (pconstant ())
      --------------------------------------------------------------------------
      Agora.Proposal.PCosign r -> P.do
        newSigs <- plet $ pfield @"newCosigners" # r

        passert "ST at inputs must be 1" $
          spentST #== 1

        passert "Signed by all new cosigners" $
          pall # signedBy # newSigs

        passert "As many new cosigners as Stake datums" $
          spentStakeST #== plength # newSigs

        passert "All new cosigners are witnessed by their Stake datums" $
          pall
            # plam
              ( \sig ->
                  pmatch (findStakeOwnedBy # stakeSTAssetClass # pfromData sig # txInfoF.datums # txInfoF.inputs) $ \case
                    PNothing -> pcon PFalse
                    PJust _ -> pcon PTrue
              )
            # newSigs

        passert "Signatures are correctly added to cosignature list" $
          anyOutput @Agora.Proposal.PProposalDatum # ctx.txInfo
            #$ plam
            $ \newValue address newProposalDatum -> P.do
              let correctDatum =
                    pdata newProposalDatum
                      #== pdata
                        ( mkRecordConstr
                            Agora.Proposal.PProposalDatum
                            ( #proposalId .= proposalF.proposalId
                                .& #effects .= proposalF.effects
                                .& #status .= proposalF.status
                                .& #cosigners .= pdata (pconcat # newSigs # proposalF.cosigners)
                                .& #thresholds .= proposalF.thresholds
                                .& #votes .= proposalF.votes
                            )
                        )

              foldr1
                (#&&)
                [ pcon PTrue
                , ptraceIfFalse "Datum must be correct" correctDatum
                , ptraceIfFalse "Value should be correct" $ pdata txOutF.value #== pdata newValue
                , ptraceIfFalse "Must be sent to Proposal's address" $ ownAddress #== pdata address
                ]

        popaque (pconstant ())
      --------------------------------------------------------------------------
      Agora.Proposal.PUnlock _r -> P.do
        passert "ST at inputs must be 1" $
          spentST #== 1

        popaque (pconstant ())
      --------------------------------------------------------------------------
      Agora.Proposal.PAdvanceProposal _r -> P.do
        passert "ST at inputs must be 1" $
          spentST #== 1

        popaque (pconstant ())

{- | Check for various invariants a proposal must uphold.
     This can be used to check both upopn creation and
     upon any following state transitions in the proposal.
-}
proposalDatumValid :: Term s (Agora.Proposal.PProposalDatum :--> PBool)
proposalDatumValid =
  phoistAcyclic $
    plam $ \datum' -> P.do
      datum <- pletFields @'["effects", "cosigners"] $ datum'

      let effects :: Term _ (PBuiltinMap Agora.Proposal.PResultTag (PBuiltinMap Plutarch.Api.V1.PValidatorHash Plutarch.Api.V1.PDatumHash))
          effects = punsafeCoerce datum.effects

          atLeastOneNegativeResult :: Term _ PBool
          atLeastOneNegativeResult =
            pany # plam (\pair -> pnull #$ pfromData $ psndBuiltin # pair) # effects

      foldr1
        (#&&)
        [ ptraceIfFalse "Proposal has at least one ResultTag has no effects" atLeastOneNegativeResult
        , ptraceIfFalse "Proposal has at least one cosigner" $ pnotNull # pfromData datum.cosigners
        , ptraceIfFalse "Proposal has at most five cosigners" $ plength # (pfromData datum.cosigners) #< 6
        ]
