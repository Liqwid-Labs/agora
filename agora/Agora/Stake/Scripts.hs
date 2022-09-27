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
  PStakeRedeemer (PClearDelegate, PDelegateTo, PDepositWithdraw, PDestroy, PPermitVote, PRetractVotes),
  PStakeRedeemerContext (
    PDepositWithdrawDelta,
    PNoMetadata,
    PSetDelegateTo
  ),
  PStakeRedeemerHandlerContext (
    PStakeRedeemerHandlerContext
  ),
  PStakeRedeemerHandlerTerm (PStakeRedeemerHandlerTerm),
  StakeRedeemerImpl (..),
  pstakeLocked,
  runStakeRedeemerHandler,
 )
import Agora.Stake.Redeemers (
  pclearDelegate,
  pdelegateTo,
  pdepositWithdraw,
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
import "liqwid-plutarch-extra" Plutarch.Extra.List (pfindJust)
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
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
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
    txInfo <- pletC (getField @"txInfo" ctx)
    let _a :: Term _ PTxInfo
        _a = txInfo
    txInfoF <- pletFieldsC @'["mint", "inputs", "outputs", "signatories", "datums"] txInfo

    PMinting ownSymbol' <- pmatchC $ pfromData (getField @"purpose" ctx)
    ownSymbol <- pletC $ pfield @"_0" # ownSymbol'
    spentST <- pletC $ psymbolValueOf # ownSymbol #$ pvalueSpent # getField @"inputs" txInfoF
    mintedST <- pletC $ psymbolValueOf # ownSymbol # getField @"mint" txInfoF

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
                        (psymbolValueOf # ownSymbol # getField @"value" txOutF #== 1)
                        ( let datum =
                                pfromData $
                                  pfromOutputDatum @(PAsData PStakeDatum)
                                    # getField @"datum" txOutF
                                    # getField @"datums" txInfoF
                           in pnot # (pstakeLocked # datum)
                        )
                        (pconstant False)
                )
              # pfromData (getField @"inputs" txInfoF)

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
                              pmatch (pfromData $ pfield @"credential" # getField @"address" outputF) $ \case
                                -- Should pay to a script address
                                PPubKeyCredential _ -> pcon PFalse
                                PScriptCredential ((pfield @"_0" #) -> validatorHash) ->
                                  let tn :: Term _ PTokenName
                                      tn = punsafeCoerce $ pfromData validatorHash
                                   in pvalueOf # getField @"value" outputF # ownSymbol # tn #== 1
                        )
                      # pfromData (getField @"outputs" txInfoF)

              outputF <-
                pletFieldsC @'["value", "address", "datum"] scriptOutputWithStakeST
              datumF <-
                pletFieldsC @'["owner", "stakedAmount"] $
                  pto $
                    pfromData $
                      pfromOutputDatum @(PAsData PStakeDatum) # getField @"datum" outputF # getField @"datums" txInfoF

              let hasExpectedStake =
                    ptraceIfFalse "Stake ouput has expected amount of stake token" $
                      pvalueDiscrete' gtClassRef # getField @"value" outputF #== getField @"stakedAmount" datumF
              let ownerSignsTransaction =
                    ptraceIfFalse "Stake Owner should sign the transaction" $
                      pauthorizedBy
                        # authorizationContext txInfoF
                        # getField @"owner" datumF

              pure $ hasExpectedStake #&& ownerSignsTransaction

          pure $ popaque (pconstant ())

    pure $ pif (0 #< mintedST) minting burning

--------------------------------------------------------------------------------

{- | Create a stake validator, given the implementation of stake redeemers.

     @since 1.0.0
-}
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
      txInfo <- pletC $ pfromData (getField @"txInfo" ctxF)
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

      -- Assemble the stake input context.

      stakeInputDatum <- pfromData . fst <$> ptryFromC datum
      stakeInputDatumF <- pletAllC $ pto stakeInputDatum

      PSpending stakeInputRef <- pmatchC $ pfromData (getField @"purpose" ctxF)

      -- The UTxO we are validating, which is also the input stake.
      stakeInput <-
        pletC $
          pfield @"resolved"
            #$ passertPJust # "Malformed script context: own input not found"
            #$ pfindTxInByTxOutRef
              # (pfield @"_0" # stakeInputRef)
              # getField @"inputs" txInfoF

      stakeInputF <- pletFieldsC @'["address", "value"] stakeInput

      stakeInputContext <-
        pletC $
          pcon $
            PStakeInput
              stakeInputDatum
              (getField @"value" stakeInputF)

      --------------------------------------------------------------------------

      -- Assemble the signature context.

      signedBy <- pletC $ pauthorizedBy # authorizationContext txInfoF

      let ownerSignsTransaction = signedBy # getField @"owner" stakeInputDatumF

          delegateSignsTransaction =
            pmaybeData
              # pconstant False
              # plam ((signedBy #) . pfromData)
              # pfromData (getField @"delegatedTo" stakeInputDatumF)

      sigContext <-
        pletC $
          pif ownerSignsTransaction (pcon PSignedByOwner) $
            pif delegateSignsTransaction (pcon PSignedByDelegate) $
              pcon PUnknownSig

      --------------------------------------------------------------------------

      stCurrencySymbol <- pletC $ pconstant $ stakeSTSymbol as
      mintedST <- pletC $ psymbolValueOf # stCurrencySymbol # getField @"mint" txInfoF
      valueSpent <- pletC $ pvalueSpent # getField @"inputs" txInfoF
      spentST <- pletC $ psymbolValueOf # stCurrencySymbol #$ valueSpent

      -- The stake validator can only handle one stake in one transaction.

      pguardC "ST at inputs must be 1" $
        spentST #== 1

      let oneStakeBurnt =
            ptraceIfFalse "Exactly one stake st burnt" $
              mintedST #== (-1)

      --------------------------------------------------------------------------

      -- Assemble the stake output context.

      let -- Look for the output stake.
          stakeOutput =
            pfindJust
              # plam
                ( \output -> unTermCont $ do
                    outputF <-
                      pletFieldsC @'["address", "value", "datum"]
                        output

                    let isStakeOutput =
                          -- The stake should be owned by the stake validator.
                          getField @"address" outputF #== getField @"address" stakeInputF
                            #&&
                            -- The stake UTxO carries the state thread token.
                            psymbolValueOf
                              # stCurrencySymbol
                              # getField @"value" outputF #== 1

                        stakeOutputDatum =
                          pfromOutputDatum
                            # getField @"datum" outputF
                            # getField @"datums" txInfoF

                        context =
                          pcon $
                            PStakeOutput
                              (pfromData stakeOutputDatum)
                              (getField @"value" outputF)

                    pure $
                      pif
                        isStakeOutput
                        (pjust # context)
                        pnothing
                )
              # pfromData (getField @"outputs" txInfoF)

      stakeOutputContext <-
        pletC $
          pmatch stakeOutput $ \case
            -- Stake output found.
            PJust stakeOutput' -> stakeOutput'
            -- Stake output not found, meaning the input stake should be burnt.
            PNothing -> unTermCont $ do
              pguardC "One stake should be burnt" oneStakeBurnt

              pure $ pcon PStakeBurnt

      --------------------------------------------------------------------------

      -- Assemble the proposal context.

      let AssetClass (propCs, propTn) = proposalSTAssetClass as

      proposalSTClass <-
        pletC $
          passetClass
            # pconstant propCs
            # pconstant propTn

      let pstMinted =
            passetClassValueOf # getField @"mint" txInfoF # proposalSTClass #== 1

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
                  # getField @"redeemers" txInfoF

              f :: Term _ (PTxInInfo :--> PMaybe PTxOutRef)
              f = plam $ \inInfo ->
                let value = pfield @"value" #$ pfield @"resolved" # inInfo
                    ref = pfield @"outRef" # inInfo
                 in pif
                      (passetClassValueOf # value # proposalSTClass #== 1)
                      (pjust # ref)
                      pnothing

              proposalRef = pfindJust # f # getField @"inputs" txInfoF
           in pif pstMinted (pcon PNewProposal) $
                pmaybe
                  # pcon PNoProposal
                  # plam
                    ( \((convertRedeemer #) -> proposalRedeemer) ->
                        pcon $ PWithProposalRedeemer proposalRedeemer
                    )
                  #$ proposalRef #>>= findRedeemer

      --------------------------------------------------------------------------

      -- Assemeble the redeemer handler context.

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
              txInfo

      noMetadataContext <-
        pletC $
          mkRedeemerhandlerContext
            #$ pcon
            $ PNoMetadata

      --------------------------------------------------------------------------

      -- Call the redeemer handler.

      stakeRedeemer :: Term _ PStakeRedeemer <- fst <$> ptryFromC redeemer

      pure $
        popaque $
          pmatch stakeRedeemer $ \case
            PDestroy _ -> runStakeRedeemerHandler (getField @"onDestroy" impl) # noMetadataContext
            PPermitVote _ -> runStakeRedeemerHandler (getField @"onPermitVote" impl) # noMetadataContext
            PRetractVotes _ -> runStakeRedeemerHandler (getField @"onRetractVote" impl) # noMetadataContext
            PClearDelegate _ -> runStakeRedeemerHandler (getField @"onClearDelegate" impl) # noMetadataContext
            PDelegateTo ((pfield @"pkh" #) -> pkh) ->
              runStakeRedeemerHandler (getField @"onDelegateTo" impl)
                #$ mkRedeemerhandlerContext
                #$ pcon
                $ PSetDelegateTo pkh
            PDepositWithdraw ((pfield @"delta" #) -> delta) ->
              runStakeRedeemerHandler (getField @"onDepositWithdraw" impl) #$ mkRedeemerhandlerContext
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
      { onDepositWithdraw = PStakeRedeemerHandlerTerm pdepositWithdraw
      , onDestroy = PStakeRedeemerHandlerTerm pdestroy
      , onPermitVote = PStakeRedeemerHandlerTerm ppermitVote
      , onRetractVote = PStakeRedeemerHandlerTerm pretractVote
      , onDelegateTo = PStakeRedeemerHandlerTerm pdelegateTo
      , onClearDelegate = PStakeRedeemerHandlerTerm pclearDelegate
      }
