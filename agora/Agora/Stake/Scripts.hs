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
import Agora.Proposal (PProposalDatum, PProposalRedeemer)
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
    PSpendProposal
  ),
  PSigContext (PSigContext),
  PSignedBy (
    PSignedByDelegate,
    PSignedByOwner,
    PUnknownSig
  ),
  PStakeDatum,
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
  KeyGuarantees (Sorted),
  PCredential (PPubKeyCredential, PScriptCredential),
  PTokenName,
 )
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Api.V1.Value (PValue)
import Plutarch.Api.V2 (
  AmountGuarantees,
  PMintingPolicy,
  PScriptPurpose (PMinting, PSpending),
  PTxInfo,
  PTxOut,
  PValidator,
 )
import Plutarch.Extra.AssetClass (
  passetClass,
  passetClassValueOf,
  pvalueOf,
 )
import Plutarch.Extra.Category (PSemigroupoid ((#>>>)))
import Plutarch.Extra.Field (pletAll)
import Plutarch.Extra.Functor (PFunctor (pfmap))
import "liqwid-plutarch-extra" Plutarch.Extra.List (pfindJust, pmapMaybe)
import Plutarch.Extra.Maybe (
  passertPJust,
  pfromMaybe,
  pjust,
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
  pvalueDiscrete,
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
    plam $ \_datum redeemer ctx -> unTermCont $ do
      let sstValueOf ::
            ( forall (ag :: AmountGuarantees) (s :: S).
              Term s (PValue 'Sorted ag :--> PInteger)
            )
          sstValueOf =
            phoistAcyclic $
              psymbolValueOf # pconstant (stakeSTSymbol as)

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

      PSpending stakeInputRef <- pmatchC $ pfromData ctxF.purpose

      let validatedInput =
            pfield @"resolved"
              #$ passertPJust
                # "Malformed script context: validated input not found"
              #$ pfindTxInByTxOutRef
                # (pfield @"_0" # stakeInputRef)
                # txInfoF.inputs

          stakeValidatorAddress = pfield @"address" # validatedInput

      --------------------------------------------------------------------------

      getStakeDatum :: Term _ (PTxOut :--> PMaybe PStakeDatum) <-
        pletC $
          plam $ \txOut -> unTermCont $ do
            txOutF <- pletFieldsC @'["value", "datum", "address"] txOut

            let isStakeUTxO =
                  foldl1
                    (#&&)
                    [ ptraceIfFalse "Carries SST" $
                        sstValueOf # txOutF.value #== 1
                    , ptraceIfFalse "Owned by stake validator" $
                        txOutF.address #== stakeValidatorAddress
                    ]

                datum =
                  ptrace "Resolve stake datum" $
                    pfromData $
                      pfromOutputDatum @(PAsData PStakeDatum)
                        # txOutF.datum
                        # txInfoF.datums

            pure $ pif isStakeUTxO (pjust # datum) pnothing

      --------------------------------------------------------------------------

      stakeInputDatums <-
        pletC $
          pmapMaybe
            # ((pfield @"resolved") #>>> getStakeDatum)
            # pfromData txInfoF.inputs

      --------------------------------------------------------------------------

      firstStakeInputDatumF <-
        pletFieldsC @'["owner", "delegatedTo"] $
          phead # stakeInputDatums

      restOfStakeInputDatums <- pletC $ ptail # stakeInputDatums

      pguardC "All input stakes have the same owner or delegate" $
        let allHaveSameOwner =
              pall
                # ( (pfield @"owner")
                      #>>> plam (#== firstStakeInputDatumF.owner)
                  )
                # restOfStakeInputDatums
            allHaveSameDelegate =
              pall
                # ( (pfield @"delegatedTo")
                      #>>> plam (#== firstStakeInputDatumF.delegatedTo)
                  )
                # restOfStakeInputDatums
         in allHaveSameOwner #|| allHaveSameDelegate

      authorizedBy <- pletC $ pauthorizedBy # authorizationContext txInfoF

      let ownerSignsTransaction = authorizedBy # firstStakeInputDatumF.owner

          delegateSignsTransaction =
            pmaybeData
              # pconstant False
              # plam ((authorizedBy #) . pfromData)
              # pfromData firstStakeInputDatumF.delegatedTo

          signedBy =
            pif
              ownerSignsTransaction
              (pcon PSignedByOwner)
              $ pif
                delegateSignsTransaction
                (pcon PSignedByDelegate)
                $ pcon PUnknownSig

      sigContext <-
        pletC $
          pcon $
            PSigContext
              firstStakeInputDatumF.owner
              firstStakeInputDatumF.delegatedTo
              signedBy

      --------------------------------------------------------------------------

      let gtAssetClass = passetClass # pconstant gtSym # pconstant gtTn

      stakeOutputDatums <-
        pletC $
          pmapMaybe
            # plam
              ( \output ->
                  let validateGT = plam $ \stakeDatum ->
                        let expected = pfield @"stakedAmount" # stakeDatum
                            actual =
                              pvalueDiscrete
                                # gtAssetClass
                                # (pfield @"value" # output)
                         in pif
                              (expected #== actual)
                              stakeDatum
                              (ptraceError "Unmatched GT value")
                   in pfmap
                        # validateGT
                        # (getStakeDatum # output)
              )
            # pfromData txInfoF.outputs

      --------------------------------------------------------------------------

      mintedST <- pletC $ sstValueOf # txInfoF.mint

      pguardC "No new SST minted" $
        foldl1
          (#||)
          [ ptraceIfTrue "All stakes burnt" $
              mintedST #< 0 #&& pnull # stakeOutputDatums
          , ptraceIfTrue "Nothing burnt" $
              mintedST #== 0
          ]

      --------------------------------------------------------------------------

      -- Assemble the proposal context.

      let AssetClass (propCs, propTn) = proposalSTAssetClass as

      proposalSTClass <-
        pletC $
          passetClass
            # pconstant propCs
            # pconstant propTn

      getProposalDatum <- pletC $
        plam $
          flip pletAll $ \txOutF ->
            let isProposalUTxO =
                  passetClassValueOf
                    # txOutF.value
                    # proposalSTClass #== 1
                proposalDatum =
                  pfromData $
                    pfromOutputDatum @(PAsData PProposalDatum)
                      # txOutF.datum
                      # txInfoF.datums
             in pif isProposalUTxO (pjust # proposalDatum) pnothing

      let pstMinted =
            passetClassValueOf # txInfoF.mint # proposalSTClass #== 1

          newProposalContext =
            pcon $
              PNewProposal $
                pfield @"proposalId"
                  #$ passertPJust # "Proposal output should present"
                  #$ pfindJust # getProposalDatum # pfromData txInfoF.outputs

          spendProposalContext =
            let getProposalRedeemer = plam $ \ref ->
                  flip (ptryFrom @PProposalRedeemer) fst $
                    pto $
                      passertPJust
                        # "Malformed script context: propsoal input not found in redeemer map"
                          #$ plookup
                        # pcon
                          ( PSpending $
                              pdcons @_0
                                # pdata ref
                                # pdnil
                          )
                        # txInfoF.redeemers

                getContext = plam $
                  flip pletAll $ \inInfoF ->
                    pfmap
                      # plam
                        ( \proposalDatum ->
                            let id = pfield @"proposalId" # proposalDatum
                                status = pfield @"status" # proposalDatum
                                redeemer = getProposalRedeemer # inInfoF.outRef
                             in pcon $ PSpendProposal id status redeemer
                        )
                      #$ getProposalDatum
                      # pfromData inInfoF.resolved
             in pfindJust # getContext # pfromData txInfoF.inputs

          noProposalContext = pcon PNoProposal

      proposalContext <-
        pletC $
          pif
            pstMinted
            newProposalContext
            (pfromMaybe # noProposalContext # spendProposalContext)

      --------------------------------------------------------------------------

      -- Assemeble the redeemer handler context.

      mkRedeemerhandlerContext <- pletC $
        plam $ \redeemerContext ->
          pcon $
            PStakeRedeemerHandlerContext
              stakeInputDatums
              stakeOutputDatums
              redeemerContext
              sigContext
              proposalContext
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
            PDestroy _ -> runStakeRedeemerHandler impl.onDestroy # noMetadataContext
            PPermitVote _ -> runStakeRedeemerHandler impl.onPermitVote # noMetadataContext
            PRetractVotes _ -> runStakeRedeemerHandler impl.onRetractVote # noMetadataContext
            PClearDelegate _ -> runStakeRedeemerHandler impl.onClearDelegate # noMetadataContext
            PDelegateTo ((pfield @"pkh" #) -> pkh) ->
              runStakeRedeemerHandler impl.onDelegateTo
                #$ mkRedeemerhandlerContext
                #$ pcon
                $ PSetDelegateTo pkh
            PDepositWithdraw ((pfield @"delta" #) -> delta) ->
              runStakeRedeemerHandler impl.onDepositWithdraw #$ mkRedeemerhandlerContext
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
