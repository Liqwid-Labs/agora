module Agora.Proposal.Scripts (
  proposalValidator,
  proposalPolicy,
  proposalDatumValid,
) where

import Agora.Proposal
import Agora.Record (mkRecordConstr, (.&), (.=))
import Agora.Stake (PStakeDatum)
import Agora.Utils (
  anyOutput,
  findTxOutByTxOutRef,
  passert,
  pfindDatum',
  pnotNull,
  psymbolValueOf,
  ptokenSpent,
  ptxSignedBy,
  pvalueSpent,
 )
import Plutarch.Api.V1 (
  PDatumHash,
  PMaybeData (PDJust, PDNothing),
  PMintingPolicy,
  PPubKeyHash,
  PScriptContext (PScriptContext),
  PScriptPurpose (PMinting, PSpending),
  PTxInInfo (PTxInInfo),
  PTxInfo (PTxInfo),
  PTxOut (PTxOut),
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
    let mintedProposalST = passetClassValueOf # mintedValue # (passetClass # (pfield @"_0" # ownSymbol') # pconstant "")

    passert "Governance state-thread token must move" $
      ptokenSpent
        # (passetClass # pconstant govCs # pconstant govTn)
        # inputs

    passert "Minted exactly one proposal ST" $
      mintedProposalST #== 1

    popaque (pconstant ())

-- | Validator for Proposals.
proposalValidator :: Proposal -> ClosedTerm PValidator
proposalValidator proposal =
  plam $ \datum redeemer ctx' -> P.do
    PScriptContext ctx' <- pmatch ctx'
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    txInfo <- plet $ pfromData ctx.txInfo
    PTxInfo txInfo' <- pmatch txInfo
    txInfoF <- pletFields @'["inputs", "mint"] txInfo'
    PSpending ((pfield @"_0" #) -> txOutRef) <- pmatch $ pfromData ctx.purpose

    PJust txOut <- pmatch $ findTxOutByTxOutRef # txOutRef # txInfoF.inputs
    txOutF <- pletFields @'["address", "value"] $ txOut

    let proposalDatum :: Term _ PProposalDatum
        proposalDatum = pfromData $ punsafeCoerce datum
        proposalRedeemer :: Term _ PProposalRedeemer
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

    stCurrencySymbol <- plet $ pconstant $ mintingPolicySymbol $ mkMintingPolicy (proposalPolicy proposal)
    valueSpent <- plet $ pvalueSpent # txInfoF.inputs
    spentST <- plet $ psymbolValueOf # stCurrencySymbol #$ valueSpent
    let AssetClass (stakeSym, stakeTn) = proposal.stakeSTAssetClass
    stakeSTAssetClass <- plet $ passetClass # pconstant stakeSym # pconstant stakeTn
    spentStakeST <- plet $ passetClassValueOf # valueSpent # stakeSTAssetClass

    pmatch proposalRedeemer $ \case
      PVote _r -> P.do
        passert "ST at inputs must be 1" $
          spentST #== 1

        popaque (pconstant ())
      --------------------------------------------------------------------------
      PCosign r -> P.do
        newSigs <- plet $ pfield @"newCosigners" # r

        passert "ST at inputs must be 1" $
          spentST #== 1

        passert "Signed by all new cosigners" $
          pall # plam (\sig -> ptxSignedBy # ctx.txInfo # sig) # newSigs

        passert "As many new cosigners as Stake datums" $
          spentStakeST #== plength # newSigs

        let stakeDatumOwnedBy :: Term _ (PPubKeyHash :--> PStakeDatum :--> PBool)
            stakeDatumOwnedBy =
              phoistAcyclic $
                plam $ \pk stakeDatum -> P.do
                  stakeDatumF <- pletFields @'["owner"] $ pto stakeDatum
                  stakeDatumF.owner #== pdata pk

        -- Does the input have a `Stake` owned by a particular PK?
        let isInputStakeOwnedBy :: Term _ (PAsData PPubKeyHash :--> PAsData PTxInInfo :--> PBool)
            isInputStakeOwnedBy =
              plam $ \ss txInInfo' -> P.do
                PTxInInfo ((pfield @"resolved" #) -> txOut) <- pmatch $ pfromData txInInfo'
                PTxOut txOut' <- pmatch txOut
                txOutF <- pletFields @'["value", "datumHash"] txOut'
                outStakeST <- plet $ passetClassValueOf # txOutF.value # stakeSTAssetClass
                pmatch txOutF.datumHash $ \case
                  PDNothing _ -> pcon PFalse
                  PDJust ((pfield @"_0" #) -> datumHash) ->
                    pif
                      (outStakeST #== 1)
                      -- TODO: use 'ptryFindDatum' instead in the future
                      ( pmatch (pfindDatum' # datumHash # txInfo) $ \case
                          PNothing -> pcon PFalse
                          PJust v -> stakeDatumOwnedBy # pfromData ss # pfromData v
                      )
                      (pcon PFalse)

        passert "All new cosigners are witnessed by their Stake datums" $
          pall
            # plam (\sig -> pany # (isInputStakeOwnedBy # sig) # txInfoF.inputs)
            # newSigs

        passert "Signatures are correctly added to cosignature list" $
          anyOutput @PProposalDatum # ctx.txInfo
            #$ plam
            $ \newValue address newProposalDatum -> P.do
              let correctDatum =
                    pdata newProposalDatum
                      #== pdata
                        ( mkRecordConstr
                            PProposalDatum
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
      PUnlock _r -> P.do
        passert "ST at inputs must be 1" $
          spentST #== 1

        popaque (pconstant ())
      --------------------------------------------------------------------------
      PAdvanceProposal _r -> P.do
        passert "ST at inputs must be 1" $
          spentST #== 1

        popaque (pconstant ())

{- | Check for various invariants a proposal must uphold.
     This can be used to check both upopn creation and
     upon any following state transitions in the proposal.
-}
proposalDatumValid :: Term s (PProposalDatum :--> PBool)
proposalDatumValid =
  phoistAcyclic $
    plam $ \datum' -> P.do
      datum <- pletFields @'["effects", "cosigners"] $ datum'

      let effects :: Term _ (PBuiltinMap PResultTag (PBuiltinMap PValidatorHash PDatumHash))
          effects = punsafeCoerce datum.effects

          atLeastOneNegativeResult :: Term _ PBool
          atLeastOneNegativeResult =
            pany # plam (\pair -> pnull #$ pfromData $ psndBuiltin # pair) # effects

      foldr1
        (#&&)
        [ ptraceIfFalse "Proposal has at least one ResultTag has no effects" atLeastOneNegativeResult
        , ptraceIfFalse "Proposal has at least one cosigner" $ pnotNull # pfromData datum.cosigners
        ]
