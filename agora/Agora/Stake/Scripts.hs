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
import Agora.Utils (passert, pmapMaybe)
import Plutarch.Api.V1 (
  PCredential (PPubKeyCredential, PScriptCredential),
  PCurrencySymbol,
  PTokenName,
 )
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Api.V2 (
  PMintingPolicy,
  PScriptPurpose (PMinting, PSpending),
  PTxInfo,
  PTxOut,
  PValidator,
 )
import Plutarch.Extra.AssetClass (
  PAssetClass,
  passetClassValueOf,
  pvalueOf,
 )
import Plutarch.Extra.Field (pletAll)
import Plutarch.Extra.Functor (PFunctor (pfmap))
import "liqwid-plutarch-extra" Plutarch.Extra.List (pfindJust)
import Plutarch.Extra.Maybe (
  passertPJust,
  pfromMaybe,
  pjust,
  pmaybeData,
  pnothing,
 )
import Plutarch.Extra.Ord (POrdering (PEQ, PGT, PLT), pcompareBy, pfromOrd)
import Plutarch.Extra.ScriptContext (
  pfindTxInByTxOutRef,
  pfromOutputDatum,
  pvalueSpent,
 )
import Plutarch.Extra.Sum (PSum (PSum))
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pguardC,
  pletC,
  pletFieldsC,
  pmatchC,
  ptryFromC,
 )
import Plutarch.Extra.Traversable (pfoldMap)
import Plutarch.Extra.Value (
  psymbolValueOf,
 )
import Plutarch.Num (PNum (pnegate))
import Plutarch.SafeMoney (
  pvalueDiscrete,
 )
import Plutarch.Unsafe (punsafeCoerce)
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

   == Arguments

   Following arguments should be provided(in this order):
   1. governor ST assetclass

   @since 1.0.0
-}
stakePolicy ::
  -- | The (governance) token that a Stake can store.
  ClosedTerm (PAssetClass :--> PMintingPolicy)
stakePolicy =
  plam $ \gstClass _redeemer ctx' -> unTermCont $ do
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
          let numStakeInputs =
                pto $
                  pfoldMap @_ @_ @(PSum PInteger)
                    # plam
                      ( \((pfield @"resolved" #) -> txOut) -> unTermCont $ do
                          txOutF <- pletFieldsC @'["value", "datum"] txOut

                          let isStakeUTxO =
                                psymbolValueOf # ownSymbol # txOutF.value #== 1

                          pmatchC isStakeUTxO
                            >>= \case
                              PTrue -> do
                                let datum =
                                      pfromData $
                                        pfromOutputDatum @(PAsData PStakeDatum)
                                          # txOutF.datum
                                          # txInfoF.datums

                                pguardC "Stake is unlocked" $
                                  pnot # (pstakeLocked # datum)

                                pure $ pcon $ PSum 1
                              PFalse -> pure mempty
                      )
                    # pfromData txInfoF.inputs

          pguardC "ST burned" $
            mintedST #== pnegate # numStakeInputs

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
                      pvalueDiscrete # gstClass # outputF.value #== datumF.stakedAmount
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

     == Arguments

     Following arguments should be provided(in this order):
     1. stake ST symbol
     2. proposal ST assetclass
     3. governor ST assetclass

     @since 1.0.0
-}
mkStakeValidator ::
  StakeRedeemerImpl ->
  ClosedTerm (PCurrencySymbol :--> PAssetClass :--> PAssetClass :--> PValidator)
mkStakeValidator impl =
  plam $ \sstSymbol pstClass gstClass _datum redeemer ctx -> unTermCont $ do
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

        stakeValidatorCredential =
          pfield @"credential"
            #$ pfield @"address" # validatedInput

    --------------------------------------------------------------------------

    -- Returns stake datum if the given UTxO is a stake UTxO.
    getStakeDatum :: Term _ (PTxOut :--> PMaybe PStakeDatum) <-
      pletC $
        plam $
          flip (pletFields @'["value", "datum", "address"]) $ \txOutF ->
            pmatch
              ( pcompareBy # pfromOrd
                  # (psymbolValueOf # sstSymbol # txOutF.value)
                  # 1
              )
              $ \case
                -- > 1
                PGT -> ptraceError "More than one SST in one UTxO"
                -- 1
                PEQ ->
                  let ownerCredential = pfield @"credential" # txOutF.address

                      isOwnedByStakeValidator =
                        ownerCredential #== stakeValidatorCredential

                      datum =
                        ptrace "Resolve stake datum" $
                          pfromData $
                            pfromOutputDatum @(PAsData PStakeDatum)
                              # txOutF.datum
                              # txInfoF.datums
                   in passert
                        "Should owned by stake validator"
                        isOwnedByStakeValidator
                        (pjust # datum)
                -- 0
                PLT -> pnothing

    --------------------------------------------------------------------------

    -- Find all stake inputs.

    stakeInputDatums <-
      pletC $
        pmapMaybe
          # plam ((getStakeDatum #) . (pfield @"resolved" #))
          # pfromData txInfoF.inputs

    --------------------------------------------------------------------------

    -- Assemble the signature context.

    firstStakeInputDatumF <-
      pletFieldsC @'["owner", "delegatedTo"] $
        phead # stakeInputDatums

    restOfStakeInputDatums <- pletC $ ptail # stakeInputDatums

    authorizedBy <- pletC $ pauthorizedBy # authorizationContext txInfoF

    PPair allHaveSameOwner allHaveSameDelegatee <-
      pmatchC $
        pfoldr
          # plam
            ( \d p -> unTermCont $ do
                dF <- pletFieldsC @'["owner", "delegatedTo"] d

                pure $
                  pmatch p $ \(PPair allHaveSameOwner allHaveSameDelegatee) ->
                    let allHaveSameOwner' =
                          allHaveSameOwner
                            #&& dF.owner #== firstStakeInputDatumF.owner
                        allHaveSameDelegatee' =
                          allHaveSameDelegatee
                            #&& dF.delegatedTo #== firstStakeInputDatumF.delegatedTo
                     in pcon $ PPair allHaveSameOwner' allHaveSameDelegatee'
            )
          # pcon (PPair (pconstant True) (pconstant True))
          # restOfStakeInputDatums

    let ownerSignsTransaction =
          allHaveSameOwner
            #&& authorizedBy # firstStakeInputDatumF.owner

        delegateSignsTransaction =
          allHaveSameDelegatee
            #&& pmaybeData
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

    -- Find all stake outputs.
    -- First step of validating stake outputs. We make sure that every stake
    --  output UTxO carries correct amount of GTs specified by its datum.
    --
    -- Note that non-GT assets are treated transparently.
    stakeOutputDatums <-
      pletC $
        pmapMaybe
          # plam
            ( \output ->
                let validateGT = plam $ \stakeDatum ->
                      let expected = pfield @"stakedAmount" # stakeDatum

                          actual =
                            pvalueDiscrete
                              # gstClass
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

    mintedST <- pletC $ psymbolValueOf # sstSymbol # txInfoF.mint

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

    getProposalDatum <- pletC $
      plam $
        flip pletAll $ \txOutF ->
          let isProposalUTxO =
                passetClassValueOf
                  # txOutF.value
                  # pstClass #== 1
              proposalDatum =
                pfromData $
                  pfromOutputDatum @(PAsData PProposalDatum)
                    # txOutF.datum
                    # txInfoF.datums
           in pif isProposalUTxO (pjust # proposalDatum) pnothing

    let pstMinted =
          passetClassValueOf # txInfoF.mint # pstClass #== 1

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

              contexts =
                pmapMaybe @PList # getContext # pfromData txInfoF.inputs
           in -- Can only handle one proposal at a time.
              precList
                ( \_ h t ->
                    pif
                      (pnull # t)
                      (pjust # h)
                      (ptraceError "Ambiguous proposal")
                )
                (const pnothing)
                # contexts

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

    stakeRedeemer <- fst <$> ptryFromC redeemer

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

     - Only one stake per tx is supported.
     - Tx must be signed by the owner.
     - The 'stakedAmount' field must be updated.
     - The stake must not be locked.
     - The new UTXO must have the previous value plus the difference
       as stated by the redeemer.

     === 'PermitVote'

     Allow a 'ProposalLock' to be put on the stake in order to vote
     on a proposal.

     - A proposal token must be spent alongside the staked.

       * Its total votes must be correctly updated to include all stakes'
         contribution.

     - Tx must be signed by the owner.

     === 'RetractVotes'

     Remove a 'ProposalLock' set when voting on a proposal.

     - A proposal token must be spent or minted alongside the stakes.
     - Tx must be signed by the owner.

     === 'Destroy'

     Destroy stakes in order to reclaim the GTs.

     - The stakes must not be locked.
     - Tx must be signed by the owner.

     == Arguments

     Following arguments should be provided(in this order):
     1. stake ST symbol
     2. proposal ST assetclass
     3. governor ST assetclass

     @since 1.0.0
-}
stakeValidator :: ClosedTerm (PCurrencySymbol :--> PAssetClass :--> PAssetClass :--> PValidator)
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
