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

import Agora.Credential (authorizationContext, pauthorizedBy)
import Agora.Proposal (
  PProposalDatum (PProposalDatum),
  PProposalRedeemer (PAdvanceProposal, PCosign, PUnlock, PVote),
  PProposalStatus (PDraft, PFinished, PLocked, PVotingReady),
  PProposalVotes (PProposalVotes),
  ProposalStatus (Draft, Finished, Locked, VotingReady),
  pretractVotes,
  pwinner',
 )
import Agora.Proposal.Time (
  currentProposalTime,
  isDraftPeriod,
  isExecutionPeriod,
  isLockingPeriod,
  isVotingPeriod,
 )
import Agora.Scripts (AgoraScripts, governorSTSymbol, proposalSTSymbol, stakeSTAssetClass)
import Agora.Stake (
  PProposalLock (PVoted),
  PStakeDatum (PStakeDatum),
  pextractVoteOption,
  pgetStakeRole,
  pisCreator,
  pisIrrelevant,
  pisPureCreator,
  pisVoter,
 )
import Agora.Utils (
  plistEqualsBy,
  pltAsData,
 )
import Plutarch.Api.V1 (PCredential)
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Api.V2 (
  PMintingPolicy,
  PScriptContext (PScriptContext),
  PScriptPurpose (PMinting, PSpending),
  PTxInInfo,
  PTxInfo (PTxInfo),
  PTxOut,
  PValidator,
 )
import Plutarch.Extra.AssetClass (passetClass, passetClassValueOf)
import Plutarch.Extra.Category (PCategory (pidentity))
import Plutarch.Extra.Comonad (pextract)
import Plutarch.Extra.Field (pletAll, pletAllC)
import Plutarch.Extra.Functor (pfmap)
import Plutarch.Extra.List (pfirstJust, pisUniq', pmergeBy, pmsort)
import Plutarch.Extra.Map (pupdate)
import Plutarch.Extra.Maybe (
  passertPJust,
  pfromJust,
  pfromMaybe,
  pisJust,
  pjust,
  pnothing,
 )
import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import Plutarch.Extra.ScriptContext (
  pfindTxInByTxOutRef,
  pfromOutputDatum,
  pisTokenSpent,
  ptryFromOutputDatum,
 )
import Plutarch.Extra.TermCont (
  pguardC,
  pletC,
  pletFieldsC,
  pmatchC,
  ptryFromC,
 )
import Plutarch.Extra.Value (psymbolValueOf)
import Plutarch.SafeMoney (PDiscrete (PDiscrete))
import Plutarch.Unsafe (punsafeCoerce)
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

     @since 0.1.0
-}
proposalPolicy ::
  -- | The assetclass of GST, see 'Agora.Governor.Scripts.governorPolicy'.
  AssetClass ->
  ClosedTerm PMintingPolicy
proposalPolicy (AssetClass (govCs, govTn)) =
  plam $ \_redeemer ctx' -> unTermCont $ do
    PScriptContext ctx' <- pmatchC ctx'
    ctx <- pletAllC ctx'
    PTxInfo txInfo' <- pmatchC $ pfromData ctx.txInfo
    txInfo <- pletFieldsC @'["inputs", "mint"] txInfo'

    PMinting ownSymbol' <- pmatchC $ pfromData ctx.purpose
    let mintedProposalST =
          passetClassValueOf
            # pfromData txInfo.mint
            # (passetClass # (pfield @"_0" # ownSymbol') # pconstant "")

    pguardC "Governance state-thread token must move" $
      pisTokenSpent
        # (passetClass # pconstant govCs # pconstant govTn)
        # txInfo.inputs

    pguardC "Minted exactly one proposal ST" $
      mintedProposalST #== 1

    pure $ popaque (pconstant ())

{- | Validation context for redeemers which witness multiple stake in the reference
      inputs.

     @since 1.0.0
-}
data PWitnessMultipleStakeContext (s :: S) = PWitnessMultipleStakeContext
  { totalAmount :: Term s PInteger
  , orderedOwners :: Term s (PList PCredential)
  }
  deriving stock
    ( -- | @since 1.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 1.0.0
      PlutusType
    )

-- | @since 1.0.0
instance DerivePlutusType PWitnessMultipleStakeContext where
  type DPTStrat _ = PlutusTypeScott

{- | Validation context for redeemers which need to modify a single stake.

     @since 1.0.0
-}
data PSpendSingleStakeContext (s :: S) = PSpendSingleStakeContext
  { inputStake :: Term s PStakeDatum
  , outputStake :: Term s PStakeDatum
  }
  deriving stock
    ( -- | @since 1.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 1.0.0
      PlutusType
    )

-- | @since 1.0.0
instance DerivePlutusType PSpendSingleStakeContext where
  type DPTStrat _ = PlutusTypeScott

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

     @since 0.1.0
-}
proposalValidator ::
  -- | Lazy precompiled scripts.
  AgoraScripts ->
  -- | See 'Agora.Governor.Governor.maximumCosigners'.
  Integer ->
  ClosedTerm PValidator
proposalValidator as maximumCosigners =
  plam $ \datum redeemer ctx' -> unTermCont $ do
    PScriptContext ctx' <- pmatchC ctx'
    ctx <- pletFieldsC @'["txInfo", "purpose"] ctx'
    txInfo <- pletC $ pfromData ctx.txInfo
    PTxInfo txInfo' <- pmatchC txInfo
    txInfoF <-
      pletFieldsC
        @'[ "referenceInputs"
          , "inputs"
          , "outputs"
          , "mint"
          , "datums"
          , "signatories"
          , "validRange"
          ]
        txInfo'
    PSpending ((pfield @"_0" #) -> txOutRef) <- pmatchC $ pfromData ctx.purpose

    PJust ((pfield @"resolved" #) -> txOut) <-
      pmatchC $
        pfindTxInByTxOutRef
          # txOutRef
          # txInfoF.inputs
    txOutF <- pletFieldsC @'["address", "value"] $ txOut

    proposalDatum <- pfromData . fst <$> ptryFromC @(PAsData PProposalDatum) datum
    proposalRedeemer <- fst <$> ptryFromC @PProposalRedeemer redeemer

    proposalF <- pletAllC $ pto proposalDatum

    ownAddress <- pletC $ txOutF.address

    thresholdsF <- pletAllC proposalF.thresholds

    currentStatus <- pletC $ pfromData $ proposalF.status

    let stCurrencySymbol = pconstant $ proposalSTSymbol as

    authorizedBy <- pletC $ pauthorizedBy # authorizationContext txInfoF

    currentTime <- pletC $ currentProposalTime # txInfoF.validRange

    -- Own output is an output that
    --  * is sent to the address of the proposal validator
    --  * has an PST
    --  * has the same proposal id as the proposal input
    --
    -- We match the proposal id here so that we can support multiple
    --  proposal inputs in one thansaction.
    proposalOut <-
      pletC $
        passertPJust
          # "Own output should be present"
            #$ pfirstJust
          # plam
            ( flip pletAll $ \outputF ->
                let isProposalUTxO =
                      foldl1
                        (#&&)
                        [ ptraceIfFalse "Own by proposal validator" $
                            outputF.address #== ownAddress
                        , ptraceIfFalse "Has proposal ST" $
                            psymbolValueOf # stCurrencySymbol # outputF.value #== 1
                        ]

                    handleProposalUTxO = unTermCont $ do
                      -- Using inline datum to avoid O(n^2) lookup.
                      datum <-
                        pletC $
                          pfromData $
                            pfromOutputDatum @(PAsData PProposalDatum)
                              # outputF.datum
                              # txInfoF.datums

                      pure $
                        pif
                          ( pfield @"proposalId" # pto datum
                              #== proposalF.proposalId
                          )
                          (pjust # datum)
                          pnothing
                 in pif
                      isProposalUTxO
                      handleProposalUTxO
                      pnothing
            )
          # pfromData txInfoF.outputs

    proposalUnchanged <- pletC $ proposalOut #== proposalDatum

    proposalOutStatus <-
      pletC $
        pfromData $
          pfield @"status" # pto proposalOut

    onlyStatusChanged <-
      pletC $
        -- Only the status of proposals is updated.
        proposalOut
          #== mkRecordConstr
            PProposalDatum
            ( #proposalId .= proposalF.proposalId
                .& #effects .= proposalF.effects
                .& #status .= pdata proposalOutStatus
                .& #cosigners .= proposalF.cosigners
                .& #thresholds .= proposalF.thresholds
                .& #votes .= proposalF.votes
                .& #timingConfig .= proposalF.timingConfig
                .& #startingTime .= proposalF.startingTime
            )

    --------------------------------------------------------------------------

    -- Find the stake inputs/outputs by SST.

    getStakeDatum :: Term _ (PTxOut :--> PMaybe PStakeDatum) <-
      pletC $
        plam $
          flip (pletFields @'["value", "datum"]) $ \txOutF ->
            let AssetClass (stakeSym, _) = stakeSTAssetClass as

                isStakeUTxO =
                  psymbolValueOf
                    # pconstant stakeSym
                    # txOutF.value
                    #== 1

                stake =
                  pfromData $
                    pfromJust
                      -- Use inline datum to avoid extra map lookup.
                      #$ ptryFromOutputDatum @(PAsData PStakeDatum)
                      # txOutF.datum
                      # txInfoF.datums
             in pif isStakeUTxO (pjust # stake) pnothing

    witnessStakes' ::
      Term
        s
        ( (PWitnessMultipleStakeContext :--> PUnit) :--> PUnit
        ) <-
      pletC $
        let updateCtx = plam $ \ctx' stake -> unTermCont $ do
              ctxF <- pmatchC ctx'

              stakeF <-
                pletFieldsC @'["stakedAmount", "owner"] $
                  pto stake

              pure $
                pcon $
                  PWitnessMultipleStakeContext
                    { totalAmount =
                        ctxF.totalAmount
                          + punsafeCoerce
                            (pfromData stakeF.stakedAmount)
                    , orderedOwners =
                        pcons # stakeF.owner
                          # ctxF.orderedOwners
                    }

            f :: Term _ (_ :--> PTxInInfo :--> _)
            f = plam $ \ctx' ((pfield @"resolved" #) -> txOut) ->
              pfromMaybe # ctx'
                #$ (pfmap # (updateCtx # ctx') #$ getStakeDatum # txOut)

            sortOwners = plam $
              flip pmatch $ \ctxF ->
                pcon $
                  PWitnessMultipleStakeContext
                    { totalAmount = ctxF.totalAmount
                    , orderedOwners = pmsort # ctxF.orderedOwners
                    }

            ctx =
              sortOwners
                #$ pfoldl
                # f
                # pcon (PWitnessMultipleStakeContext 0 pnil)
                # txInfoF.referenceInputs
         in plam (# ctx)

    let witnessStakes ::
          ( PWitnessMultipleStakeContext _ ->
            TermCont _ ()
          ) ->
          Term _ POpaque
        witnessStakes c = popaque $
          witnessStakes' #$ plam $ \sctxF ->
            unTermCont $ pmatchC sctxF >>= c >> pure (pconstant ())

    spendSingleStake' ::
      Term
        s
        ((PSpendSingleStakeContext :--> PUnit) :--> PUnit) <-
      pletC $
        let singleInput ::
              Term
                _
                ( PMaybe PStakeDatum
                    :--> PTxInInfo
                    :--> PMaybe PStakeDatum
                )
            singleInput = plam $ \l ((pfield @"resolved" #) -> txOut) ->
              unTermCont $ do
                lF <- pmatchC l
                t <- pletC $ getStakeDatum # txOut
                tF <- pmatchC t

                pure $ case (lF, tF) of
                  (PJust _, PJust _) ->
                    ptraceError "Can only deal with one stake"
                  (PNothing, _) -> t
                  (_, PNothing) -> l

            stakeInput =
              passertPJust # "Stake input not found"
                #$ pfoldl # singleInput # pnothing # txInfoF.inputs

            stakeOutput =
              pfromJust
                #$ pfirstJust # getStakeDatum # txInfoF.outputs

            ctx = pcon $ PSpendSingleStakeContext stakeInput stakeOutput
         in plam (# ctx)

    let spendSingleStake ::
          ( PSpendSingleStakeContext _ ->
            TermCont _ ()
          ) ->
          Term _ POpaque
        spendSingleStake c = popaque $
          spendSingleStake' #$ plam $ \sctx ->
            unTermCont $ pmatchC sctx >>= c >> pure (pconstant ())

    pure $
      popaque $
        pmatch proposalRedeemer $ \case
          PCosign r -> witnessStakes $ \sctxF -> do
            pguardC "Should be in draft state" $
              currentStatus #== pconstant Draft

            newSigs <- pletC $ pfield @"newCosigners" # r

            pguardC "Signed by all new cosigners" $
              pall # plam ((authorizedBy #) . pfromData) # newSigs

            updatedSigs <-
              pletC $
                pmergeBy # pltAsData
                  # newSigs
                  # proposalF.cosigners

            pguardC "Less cosigners than maximum limit" $
              plength # updatedSigs #< pconstant maximumCosigners

            pguardC "Cosigners are unique" $
              pisUniq' # updatedSigs

            pguardC "All new cosigners are witnessed by their Stake datums" $
              plistEqualsBy
                # plam (\x (pfromData -> y) -> x #== y)
                # sctxF.orderedOwners
                # newSigs

            let expectedDatum =
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

          ----------------------------------------------------------------------

          PVote r -> spendSingleStake $ \sctxF -> do
            stakeInF <- pletAllC $ pto sctxF.inputStake

            pguardC "Input proposal must be in VotingReady state" $
              currentStatus #== pconstant VotingReady

            pguardC "Proposal time should be wthin the voting period" $
              isVotingPeriod # proposalF.timingConfig
                # proposalF.startingTime
                #$ pfromJust
                # currentTime

            -- Ensure the transaction is voting to a valid 'ResultTag'(outcome).
            PProposalVotes voteMap <- pmatchC proposalF.votes
            voteFor <- pletC $ pfromData $ pfield @"resultTag" # r

            pguardC "Vote option should be valid" $
              pisJust #$ plookup # voteFor # voteMap

            -- Ensure that no lock with the current proposal id has been put on the stake.
            pguardC "Same stake shouldn't vote on the same proposal twice" $
              pnot #$ pisVoter #$ pgetStakeRole # proposalF.proposalId # stakeInF.lockedBy

            let -- The amount of new votes should be the 'stakedAmount'.
                -- Update the vote counter of the proposal, and leave other stuff as is.
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

            let newProposalLock =
                  mkRecordConstr
                    PVoted
                    ( #votedOn .= proposalF.proposalId
                        .& #votedFor .= pdata voteFor
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
                        .& #delegatedTo .= stakeInF.delegatedTo
                        .& #lockedBy .= pdata expectedProposalLocks
                    )

            pguardC "Output stake should be locked by the proposal" $ expectedStakeOut #== sctxF.outputStake

          ----------------------------------------------------------------------

          PUnlock _ -> spendSingleStake $ \sctxF -> do
            stakeInF <- pletAllC $ pto sctxF.inputStake

            stakeRole <- pletC $ pgetStakeRole # proposalF.proposalId # stakeInF.lockedBy

            pguardC "Stake input should be relevant" $
              pnot #$ pisIrrelevant # stakeRole

            retractCount <-
              pletC $
                pmatch stakeInF.stakedAmount $ \(PDiscrete v) -> pextract # v

            -- The votes can only change when the proposal still allows voting.
            let shouldUpdateVotes =
                  currentStatus #== pconstant VotingReady
                    #&& pnot # (pisPureCreator # stakeRole)

                allowRemovingCreatorLock =
                  currentStatus #== pconstant Finished

                isCreator = pisCreator # stakeRole

                -- If the stake has been used for creating the proposal,
                --  the creator lock can only be removed when the proposal
                --  is finished.
                --
                -- In other cases, all the locks related to this
                --   proposal should be removed.
                validateOutputLocks = plam $ \locks ->
                  plet
                    ( pgetStakeRole # proposalF.proposalId # locks
                    )
                    $ \newStakeRole ->
                      pif
                        (isCreator #&& pnot # allowRemovingCreatorLock)
                        (pisPureCreator # newStakeRole)
                        (pisIrrelevant # newStakeRole)

            pguardC "Proposal output correct" $
              pif
                shouldUpdateVotes
                ( let -- Remove votes and leave other parts of the proposal as it.
                      expectedVotes = pretractVotes # (pextractVoteOption # stakeRole) # retractCount # proposalF.votes

                      expectedProposalOut =
                        mkRecordConstr
                          PProposalDatum
                          ( #proposalId .= proposalF.proposalId
                              .& #effects .= proposalF.effects
                              .& #status .= proposalF.status
                              .& #cosigners .= proposalF.cosigners
                              .& #thresholds .= proposalF.thresholds
                              .& #votes .= pdata expectedVotes
                              .& #timingConfig .= proposalF.timingConfig
                              .& #startingTime .= proposalF.startingTime
                          )
                   in ptraceIfFalse "Update votes" $
                        expectedProposalOut #== proposalOut
                )
                -- No change to the proposal is allowed.
                $ ptraceIfFalse "Proposal unchanged" proposalUnchanged

            -- At last, we ensure that all locks belong to this proposal will be removed.
            stakeOutputLocks <- pletC $ pfield @"lockedBy" # pto sctxF.outputStake

            let templateStakeOut =
                  mkRecordConstr
                    PStakeDatum
                    ( #stakedAmount .= stakeInF.stakedAmount
                        .& #owner .= stakeInF.owner
                        .& #delegatedTo .= stakeInF.delegatedTo
                        .& #lockedBy .= pdata stakeOutputLocks
                    )

            pguardC "Only locks updated in the output stake" $
              templateStakeOut #== sctxF.outputStake

            pguardC "All relevant locks removed from the stake" $
              validateOutputLocks # stakeOutputLocks

          ----------------------------------------------------------------------

          PAdvanceProposal _ -> unTermCont $ do
            currentTime' <- pletC $ pfromJust # currentTime

            let inDraftPeriod = isDraftPeriod # proposalF.timingConfig # proposalF.startingTime # currentTime'
                inVotingPeriod = isVotingPeriod # proposalF.timingConfig # proposalF.startingTime # currentTime'
                inExecutionPeriod = isExecutionPeriod # proposalF.timingConfig # proposalF.startingTime # currentTime'

            inLockedPeriod <- pletC $ isLockingPeriod # proposalF.timingConfig # proposalF.startingTime # currentTime'

            pguardC "Only status changes in the output proposal" onlyStatusChanged

            pure $
              pmatch currentStatus $ \case
                PDraft ->
                  witnessStakes $ \sctxF -> do
                    let notTooLate = inDraftPeriod

                    pmatchC notTooLate >>= \case
                      PTrue -> do
                        pguardC "More cosigns than minimum amount" $
                          punsafeCoerce (pfromData thresholdsF.vote) #< sctxF.totalAmount

                        pguardC "All new cosigners are witnessed by their Stake datums" $
                          plistEqualsBy
                            # plam (\x (pfromData -> y) -> x #== y)
                            # sctxF.orderedOwners
                            # proposalF.cosigners

                        -- 'Draft' -> 'VotingReady'
                        pguardC "Proposal status set to VotingReady" $
                          proposalOutStatus #== pconstant VotingReady
                      -- Too late: failed proposal, status set to 'Finished'.
                      PFalse ->
                        pguardC "Proposal should fail: not on time" $
                          proposalOutStatus #== pconstant Finished
                PVotingReady -> unTermCont $ do
                  let notTooLate = inLockedPeriod
                      notTooEarly = pnot # inVotingPeriod

                  pguardC "Cannot advance ahead of time" notTooEarly

                  pmatchC notTooLate >>= \case
                    PTrue -> do
                      -- 'VotingReady' -> 'Locked'
                      pguardC "Proposal status set to Locked" $
                        proposalOutStatus #== pconstant Locked

                      pguardC "Winner outcome not found" $
                        pisJust #$ pwinner' # proposalF.votes
                          #$ punsafeCoerce
                          $ pfromData thresholdsF.execute
                    -- Too late: failed proposal, status set to 'Finished'.
                    PFalse ->
                      pguardC "Proposal should fail: not on time" $
                        proposalOutStatus #== pconstant Finished

                  pure $ popaque $ pconstant ()
                PLocked -> unTermCont $ do
                  let notTooLate = inExecutionPeriod
                      notTooEarly = pnot # inLockedPeriod

                  pguardC "Not too early" notTooEarly

                  pguardC "Proposal status set to Finished" $
                    proposalOutStatus #== pconstant Finished

                  let gstSymbol = pconstant $ governorSTSymbol as
                      gstMoved =
                        pany
                          # plam
                            ( \( (pfield @"value" #)
                                  . (pfield @"resolved" #) ->
                                  value
                                ) ->
                                  psymbolValueOf # gstSymbol # value #== 1
                            )
                          # pfromData txInfoF.inputs

                  pguardC "GST not moved if too late, moved otherwise" $
                    pif
                      notTooLate
                      -- Not too late: GST should moved
                      pidentity
                      -- Not too late: GST should not moved
                      pnot
                      # gstMoved

                  pure $ popaque $ pconstant ()
                PFinished -> ptraceError "Finished proposals cannot be advanced"
