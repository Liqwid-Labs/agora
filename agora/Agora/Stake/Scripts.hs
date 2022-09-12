{- |
Module     : Agora.Stake.Scripts
Maintainer : emi@haskell.fyi
Description: Plutus Scripts for Stakes.

Plutus Scripts for Stakes.
-}
module Agora.Stake.Scripts (
  stakePolicy,
  stakeValidator,
  mkStakeValidator,
) where

import Agora.Credential (authorizationContext, pauthorizedBy)
import Agora.Proposal (PProposalRedeemer)
import Agora.SafeMoney (GTTag)
import Agora.Scripts (
  AgoraScripts,
  proposalSTAssetClass,
  stakeSTSymbol,
 )
import Agora.Stake (
  PExtraTxContext (PExtraTxContext),
  PProposalContext (
    PNewProposal,
    PNoProposal,
    PWithProposalRedeemer
  ),
  PSigContext (
    PSignedByDelegate,
    PSignedByOwner,
    PUnknownSig
  ),
  PStakeDatum,
  PStakeInputContext (PStakeInput),
  PStakeOutputContext (PStakeBurnt, PStakeOutput),
  PStakeRedeemer (
    PClearDelegate,
    PDelegateTo,
    PDepositWithdraw,
    PDestroy,
    PPermitVote,
    PRetractVotes
  ),
  PStakeRedeemerContext (
    PDepositWithdrawDelta,
    PNoMetadata,
    PSetDelegateTo
  ),
  PStakeRedeemerHandlerContext (
    PStakeRedeemerHandlerContext
  ),
  StakeRedeemerImpl (
    StakeRedeemerImpl,
    onClearDelegate,
    onDelegateTo,
    onDepositWithdraw,
    onDestroy,
    onPermitVote,
    onRetractVote
  ),
  pstakeLocked,
 )
import Agora.Stake.Redeemers (
  pclearDelegate,
  pdelegateTo,
  pdepositWithdraw',
  pdestroy,
  ppermitVote,
  pretractVote,
 )
import Data.Tagged (Tagged (Tagged))
import Plutarch.Api.V1 (
  PCredential (PPubKeyCredential, PScriptCredential),
  PTokenName,
 )
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Api.V2 (
  PMintingPolicy,
  PScriptPurpose (PMinting, PSpending),
  PTxInInfo,
  PTxInfo,
  PTxOutRef,
  PValidator,
 )
import Plutarch.Extra.AssetClass (
  passetClass,
  passetClassValueOf,
  pvalueOf,
 )
import Plutarch.Extra.Bind (PBind ((#>>=)))
import Plutarch.Extra.Field (pletAllC)
import Plutarch.Extra.List (pfirstJust)
import Plutarch.Extra.Maybe (
  passertPJust,
  pjust,
  pmaybe,
  pmaybeData,
  pnothing,
 )
import Plutarch.Extra.ScriptContext (
  pfindTxInByTxOutRef,
  pfromOutputDatum,
  pvalueSpent,
 )
import Plutarch.Extra.TermCont (
  pguardC,
  pletC,
  pletFieldsC,
  pmatchC,
  ptryFromC,
 )
import Plutarch.Extra.Value (
  psymbolValueOf,
 )
import Plutarch.SafeMoney (
  pvalueDiscrete',
 )
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1.Value (AssetClass (AssetClass))
import Prelude hiding (Num ((+)))

{- | Policy for Stake state threads.

   == What this Policy does

   === For minting:

   - Check that exactly one state thread is minted.
   - Check that an output exists with a state thread and a valid datum.
   - Check that no state thread is an input.
   - assert @'PlutusLedgerApi.V1.TokenName' == 'PlutusLedgerApi.V1.ValidatorHash'@
     of the script that we pay to.

   === For burning:

   - Check that exactly one state thread is burned.
   - Check that datum at state thread is valid and not locked.

   @since 0.1.0
-}
stakePolicy ::
  -- | The (governance) token that a Stake can store.
  Tagged GTTag AssetClass ->
  ClosedTerm PMintingPolicy
stakePolicy gtClassRef =
  plam $ \_redeemer ctx' -> unTermCont $ do
    ctx <- pletFieldsC @'["txInfo", "purpose"] ctx'
    txInfo <- pletC $ ctx.txInfo
    let _a :: Term _ PTxInfo
        _a = txInfo
    txInfoF <- pletFieldsC @'["mint", "inputs", "outputs", "signatories", "datums"] txInfo

    PMinting ownSymbol' <- pmatchC $ pfromData ctx.purpose
    ownSymbol <- pletC $ pfield @"_0" # ownSymbol'
    spentST <- pletC $ psymbolValueOf # ownSymbol #$ pvalueSpent # txInfoF.inputs
    mintedST <- pletC $ psymbolValueOf # ownSymbol # txInfoF.mint

    let burning = unTermCont $ do
          pguardC "ST at inputs must be 1" $
            spentST #== 1

          pguardC "ST burned" $
            mintedST #== -1

          pguardC "An unlocked input existed containing an ST" $
            pany
              # plam
                ( \((pfield @"resolved" #) -> txOut) -> unTermCont $ do
                    txOutF <- pletFieldsC @'["value", "datum"] txOut
                    pure $
                      pif
                        (psymbolValueOf # ownSymbol # txOutF.value #== 1)
                        ( let datum =
                                pfromData $
                                  pfromOutputDatum @(PAsData PStakeDatum)
                                    # txOutF.datum
                                    # txInfoF.datums
                           in pnot # (pstakeLocked # datum)
                        )
                        (pconstant False)
                )
              # pfromData txInfoF.inputs

          pure $ popaque (pconstant ())

    let minting = unTermCont $ do
          pguardC "ST at inputs must be 0" $
            spentST #== 0

          pguardC "Minted ST must be exactly 1" $
            mintedST #== 1

          pguardC "A UTXO must exist with the correct output" $
            unTermCont $ do
              let scriptOutputWithStakeST =
                    passertPJust
                      # "Output to script not found"
                        #$ pfind
                      # plam
                        ( \output -> unTermCont $ do
                            outputF <- pletFieldsC @'["value", "address"] output
                            pure $
                              pmatch (pfromData $ pfield @"credential" # outputF.address) $ \case
                                -- Should pay to a script address
                                PPubKeyCredential _ -> pcon PFalse
                                PScriptCredential ((pfield @"_0" #) -> validatorHash) ->
                                  let tn :: Term _ PTokenName
                                      tn = punsafeCoerce $ pfromData validatorHash
                                   in pvalueOf # outputF.value # ownSymbol # tn #== 1
                        )
                      # pfromData txInfoF.outputs

              outputF <-
                pletFieldsC @'["value", "address", "datum"] scriptOutputWithStakeST
              datumF <-
                pletFieldsC @'["owner", "stakedAmount"] $
                  pto $
                    pfromData $
                      pfromOutputDatum @(PAsData PStakeDatum) # outputF.datum # txInfoF.datums

              let hasExpectedStake =
                    ptraceIfFalse "Stake ouput has expected amount of stake token" $
                      pvalueDiscrete' gtClassRef # outputF.value #== datumF.stakedAmount
              let ownerSignsTransaction =
                    ptraceIfFalse "Stake Owner should sign the transaction" $
                      pauthorizedBy
                        # authorizationContext txInfoF
                        # datumF.owner

              pure $ hasExpectedStake #&& ownerSignsTransaction

          pure $ popaque (pconstant ())

    pure $ pif (0 #< mintedST) minting burning

--------------------------------------------------------------------------------

mkStakeValidator ::
  StakeRedeemerImpl ->
  AgoraScripts ->
  Tagged GTTag AssetClass ->
  ClosedTerm PValidator
mkStakeValidator
  impl
  as
  (Tagged (AssetClass (gtSym, gtTn))) =
    plam $ \datum redeemer ctx -> unTermCont $ do
      gtAssetClass <- pletC $ passetClass # pconstant gtSym # pconstant gtTn

      --------------------------------------------------------------------------

      ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
      txInfo <- pletC $ pfromData ctxF.txInfo
      txInfoF <-
        pletFieldsC
          @'[ "inputs"
            , "referenceInputs"
            , "outputs"
            , "mint"
            , "validRange"
            , "signatories"
            , "redeemers"
            , "datums"
            ]
          txInfo

      --------------------------------------------------------------------------

      extraTxContext <-
        pletC $
          pcon $
            PExtraTxContext
              txInfoF.inputs
              txInfoF.referenceInputs
              txInfoF.outputs
              txInfoF.mint
              txInfoF.validRange
              txInfoF.signatories
              txInfoF.redeemers
              txInfoF.datums

      --------------------------------------------------------------------------

      stakeInputDatum <- pfromData . fst <$> ptryFromC datum
      stakeInputDatumF <- pletAllC $ pto stakeInputDatum

      PSpending stakeInputRef <- pmatchC $ pfromData ctxF.purpose

      stakeInput <-
        pletC $
          pfield @"resolved"
            #$ passertPJust # "Malformed script context: own input not found"
            #$ pfindTxInByTxOutRef
              # (pfield @"_0" # stakeInputRef)
              # txInfoF.inputs

      stakeInputF <- pletFieldsC @'["address", "value"] stakeInput

      stakeInputContext <-
        pletC $
          pcon $
            PStakeInput
              stakeInputDatum
              stakeInputF.value

      --------------------------------------------------------------------------

      signedBy <- pletC $ pauthorizedBy # authorizationContext txInfoF

      let ownerSignsTransaction = signedBy # stakeInputDatumF.owner

          delegateSignsTransaction =
            pmaybeData
              # pconstant False
              # plam ((signedBy #) . pfromData)
              # pfromData stakeInputDatumF.delegatedTo

      sigContext <-
        pletC $
          pif ownerSignsTransaction (pcon PSignedByOwner) $
            pif delegateSignsTransaction (pcon PSignedByDelegate) $
              pcon PUnknownSig

      --------------------------------------------------------------------------

      stCurrencySymbol <- pletC $ pconstant $ stakeSTSymbol as
      mintedST <- pletC $ psymbolValueOf # stCurrencySymbol # txInfoF.mint
      valueSpent <- pletC $ pvalueSpent # txInfoF.inputs
      spentST <- pletC $ psymbolValueOf # stCurrencySymbol #$ valueSpent

      pguardC "ST at inputs must be 1" $
        spentST #== 1

      let stBurnt =
            ptraceIfFalse "Exactly one stake st burnt" $
              mintedST #== (-1)

      --------------------------------------------------------------------------

      let stakeOutput =
            pfirstJust
              # plam
                ( \output -> unTermCont $ do
                    outputF <-
                      pletFieldsC @'["address", "value", "datum"]
                        output

                    let isStakeOutput =
                          outputF.address #== stakeInputF.address
                            #&& psymbolValueOf
                              # stCurrencySymbol
                              # outputF.value #== 1

                        stakeOutputDatum =
                          pfromOutputDatum
                            # outputF.datum
                            # txInfoF.datums

                        context =
                          pcon $
                            PStakeOutput
                              (pfromData stakeOutputDatum)
                              outputF.value

                    pure $
                      pif
                        isStakeOutput
                        (pjust # context)
                        pnothing
                )
              # pfromData txInfoF.outputs

      stakeOutputContext <-
        pletC $
          pmatch stakeOutput $ \case
            PJust stakeOutput' -> stakeOutput'
            PNothing -> unTermCont $ do
              pguardC "One stake should be burnt" stBurnt

              pure $ pcon PStakeBurnt

      --------------------------------------------------------------------------

      let AssetClass (propCs, propTn) = proposalSTAssetClass as

      proposalSTClass <-
        pletC $
          passetClass
            # pconstant propCs
            # pconstant propTn

      let pstMinted =
            passetClassValueOf # txInfoF.mint # proposalSTClass #== 1

      proposalContext <-
        pletC $
          let convertRedeemer = plam $ \(pto -> dt) ->
                ptryFrom @PProposalRedeemer dt fst

              findRedeemer = plam $ \ref ->
                plookup
                  # pcon
                    ( PSpending $
                        pdcons @_0
                          # pdata ref
                          # pdnil
                    )
                  # txInfoF.redeemers

              f :: Term _ (PTxInInfo :--> PMaybe PTxOutRef)
              f = plam $ \inInfo ->
                let value = pfield @"value" #$ pfield @"resolved" # inInfo
                    ref = pfield @"outRef" # inInfo
                 in pif
                      (passetClassValueOf # value # proposalSTClass #== 1)
                      (pjust # ref)
                      pnothing

              proposalRef = pfirstJust # f # txInfoF.inputs
           in pif pstMinted (pcon PNewProposal) $
                pmaybe
                  # pcon PNoProposal
                  # plam
                    ( \((convertRedeemer #) -> proposalRedeemer) ->
                        pcon $ PWithProposalRedeemer proposalRedeemer
                    )
                  #$ proposalRef #>>= findRedeemer

      --------------------------------------------------------------------------

      mkRedeemerhandlerContext <- pletC $
        plam $ \redeemerContext ->
          pcon $
            PStakeRedeemerHandlerContext
              stakeInputContext
              stakeOutputContext
              redeemerContext
              sigContext
              proposalContext
              gtAssetClass
              extraTxContext

      noMetadataContext <-
        pletC $
          mkRedeemerhandlerContext
            #$ pcon
            $ PNoMetadata

      --------------------------------------------------------------------------

      stakeRedeemer :: Term _ PStakeRedeemer <- fst <$> ptryFromC redeemer

      pure $
        popaque $
          pmatch stakeRedeemer $ \case
            PDestroy _ -> onDestroy impl # noMetadataContext
            PPermitVote _ -> onPermitVote impl # noMetadataContext
            PRetractVotes _ -> onRetractVote impl # noMetadataContext
            PClearDelegate _ -> onClearDelegate impl # noMetadataContext
            PDelegateTo ((pfield @"pkh" #) -> pkh) ->
              onDelegateTo impl #$ mkRedeemerhandlerContext
                #$ pcon
                $ PSetDelegateTo pkh
            PDepositWithdraw ((pfield @"delta" #) -> delta) ->
              onDepositWithdraw impl #$ mkRedeemerhandlerContext
                #$ pcon
                $ PDepositWithdrawDelta delta

{- | Validator intended for Stake UTXOs to be locked by.

     == What this Validator does:

     === 'DepositWithdraw'

     Deposit or withdraw some GT to the stake.

     - Tx must be signed by the owner.
     - The 'stakedAmount' field must be updated.
     - The stake must not be locked.
     - The new UTXO must have the previous value plus the difference
       as stated by the redeemer.

     === 'PermitVote'

     Allow a 'ProposalLock' to be put on the stake in order to vote
     on a proposal.

     - A proposal token must be spent alongside the stake.

       * Its total votes must be correctly updated to include this stake's
         contribution.

     - Tx must be signed by the owner.

     === 'RetractVotes'

     Remove a 'ProposalLock' set when voting on a proposal.

     - A proposal token must be spent alongside the stake.
     - Tx must be signed by the owner.

     === 'Destroy'

     Destroy the stake in order to reclaim the min ADA.

     - The stake must not be locked.
     - Tx must be signed by the owner.

     === 'WitnessStake'

     Allow this Stake to be included in a transaction without making
     any changes to it. In the future,
     this could use [CIP-31](https://cips.cardano.org/cips/cip31/) instead.

     - Tx must be signed by the owner __or__ a proposal ST token must be spent
       alongside the stake.
     - The datum and value must remain unchanged.

     @since 0.1.0
-}
stakeValidator ::
  -- | Lazy precompiled scripts.
  AgoraScripts ->
  -- | See 'Agora.Governor.Governor.gtClassRef'.
  Tagged GTTag AssetClass ->
  ClosedTerm PValidator
stakeValidator =
  mkStakeValidator $
    StakeRedeemerImpl
      { onDepositWithdraw = pdepositWithdraw'
      , onDestroy = pdestroy
      , onPermitVote = ppermitVote
      , onRetractVote = pretractVote
      , onDelegateTo = pdelegateTo
      , onClearDelegate = pclearDelegate
      }
