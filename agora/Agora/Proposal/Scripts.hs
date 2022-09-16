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
  PStakeDatum,
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
import Plutarch.Extra.Category (PCategory (pidentity), PSemigroupoid ((#>>>)))
import Plutarch.Extra.Comonad (pextract)
import Plutarch.Extra.Field (pletAll, pletAllC)
import Plutarch.Extra.List (pfirstJust, pisUniq', pmergeBy, pmsort)
import Plutarch.Extra.Map (pupdate)
import Plutarch.Extra.Maybe (
  passertPJust,
  pisJust,
  pjust,
  pmaybe,
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
  plam $ \datum redeemer ctx -> unTermCont $ do
    ctxF <- pletAllC ctx

    txInfo <- pletC $ pfromData ctxF.txInfo
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
        txInfo

    currentTime <- pletC $ currentProposalTime # txInfoF.validRange

    authorizedBy <- pletC $ pauthorizedBy # authorizationContext txInfoF

    ----------------------------------------------------------------------------

    PSpending ((pfield @"_0" #) -> propsalInputRef) <-
      pmatchC $ pfromData ctxF.purpose

    let proposalInput =
          pfield @"resolved"
            #$ passertPJust
            # "Own input should present"
              #$ pfindTxInByTxOutRef
            # propsalInputRef
            # txInfoF.inputs

    proposalInputF <- pletFieldsC @'["address", "value"] proposalInput

    proposalInputDatum <- pfromData . fst <$> ptryFromC @(PAsData PProposalDatum) datum
    proposalInputDatumF <- pletAllC $ pto proposalInputDatum

    thresholdsF <- pletAllC proposalInputDatumF.thresholds
    currentStatus <- pletC $ pfromData $ proposalInputDatumF.status

    -- Own output is an output that
    --  * is sent to the address of the proposal validator
    --  * has an PST
    --  * has the same proposal id as the proposal input
    --
    -- We match the proposal id here so that we can support multiple
    --  proposal inputs in one thansaction.
    proposalOutputDatum <-
      pletC $
        passertPJust
          # "Own output should be present"
            #$ pfirstJust
          # plam
            ( flip pletAll $ \outputF ->
                let pstSymbol = pconstant $ proposalSTSymbol as

                    isProposalUTxO =
                      foldl1
                        (#&&)
                        [ ptraceIfFalse "Own by proposal validator" $
                            outputF.address #== proposalInputF.address
                        , ptraceIfFalse "Has proposal ST" $
                            psymbolValueOf # pstSymbol # outputF.value #== 1
                        ]

                    handleProposalUTxO = unTermCont $ do
                      -- Using inline datum to avoid O(n^2) lookup.
                      datum <-
                        pletC $
                          pfromData $
                            ptrace "Resolve proposal datum" $
                              pfromOutputDatum @(PAsData PProposalDatum)
                                # outputF.datum
                                # txInfoF.datums

                      pure $
                        pif
                          ( pfield @"proposalId" # pto datum
                              #== proposalInputDatumF.proposalId
                          )
                          (pjust # datum)
                          pnothing
                 in pif
                      isProposalUTxO
                      handleProposalUTxO
                      pnothing
            )
          # pfromData txInfoF.outputs

    --------------------------------------------------------------------------

    -- Handle stake input/output.

    -- Reslove stake datum if the given UTxO is a stake UTxO.
    getStakeDatum :: Term _ (PTxOut :--> PMaybe PStakeDatum) <-
      pletC $
        plam $
          flip (pletFields @'["value", "datum"]) $ \txOutF ->
            let AssetClass (stakeSym, _) = stakeSTAssetClass as

                isStakeUTxO =
                  -- A stake UTxO is a UTxO that carries SST.
                  psymbolValueOf
                    # pconstant stakeSym
                    # txOutF.value
                    #== 1

                stake =
                  pfromData $
                    -- If we can't resolve the stake datum, error out.
                    passertPJust # "Stake datum should present"
                      -- Use inline datum to avoid extra map lookup.
                      #$ ptryFromOutputDatum @(PAsData PStakeDatum)
                      # txOutF.datum
                      # txInfoF.datums
             in pif isStakeUTxO (pjust # stake) pnothing

    -- Witness stakes in reference inputs.
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
              let stakeDatum = getStakeDatum # txOut
                  updateCtx' = updateCtx # ctx'
               in pmaybe # ctx' # updateCtx' # stakeDatum

            sortOwners =
              plam $
                flip pmatch $
                  \ctxF ->
                    pcon $
                      ctxF
                        { orderedOwners = pmsort # ctxF.orderedOwners
                        }

            initialCtx = pcon $ PWitnessMultipleStakeContext 0 pnil

            ctx =
              sortOwners
                #$ pfoldl
                # f
                # initialCtx
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

    -- We don't need to explicitly ensure that there's only one stake in the
    --   inputs here - the stake validator will do it for us.
    spendSingleStake' ::
      Term
        s
        ((PSpendSingleStakeContext :--> PUnit) :--> PUnit) <-
      pletC $
        let stakeInput =
              passertPJust # "Stake input should present" #$ pfirstJust
                # ((pfield @"resolved" @_ @PTxInInfo) #>>> getStakeDatum)
                # txInfoF.inputs

            stakeOutput =
              passertPJust # "Stake output should present"
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

    ----------------------------------------------------------------------------

    proposalRedeemer <- fst <$> ptryFromC @PProposalRedeemer redeemer

    pure $
      popaque $
        pmatch proposalRedeemer $ \case
          PCosign r -> witnessStakes $ \sctxF -> do
            pguardC "Should be in draft state" $
              currentStatus #== pconstant Draft

            newSigs <- pletC $ pfield @"newCosigners" # r

            pguardC "Signed by all new cosigners" $
              pall # plam ((authorizedBy #) . pfromData) # newSigs

            -- Assuming that new signatures encoded in the redeemer and exsiting
            --   cosigners are sorted in ascending order, the new list of
            --   signatures will be ordered.
            updatedSigs <-
              pletC $
                pmergeBy # pltAsData
                  # newSigs
                  # proposalInputDatumF.cosigners

            pguardC "Less cosigners than maximum limit" $
              plength # updatedSigs #< pconstant maximumCosigners

            pguardC "Cosigners are unique" $
              pisUniq' # updatedSigs

            pguardC "All new cosigners are witnessed by their Stake datums" $
              -- Also, this ensures that the cosigners field in the output
              --   propopsal datum is ordered.
              plistEqualsBy
                # plam (\x (pfromData -> y) -> x #== y)
                # sctxF.orderedOwners
                # newSigs

            let expectedDatum =
                  mkRecordConstr
                    PProposalDatum
                    ( #proposalId .= proposalInputDatumF.proposalId
                        .& #effects .= proposalInputDatumF.effects
                        .& #status .= proposalInputDatumF.status
                        .& #cosigners .= pdata updatedSigs
                        .& #thresholds .= proposalInputDatumF.thresholds
                        .& #votes .= proposalInputDatumF.votes
                        .& #timingConfig .= proposalInputDatumF.timingConfig
                        .& #startingTime .= proposalInputDatumF.startingTime
                    )

            pguardC "Signatures are correctly added to cosignature list" $
              proposalOutputDatum #== expectedDatum

          ----------------------------------------------------------------------

          PVote r -> spendSingleStake $ \sctxF -> do
            stakeInF <- pletAllC $ pto sctxF.inputStake

            pguardC "Input proposal must be in VotingReady state" $
              currentStatus #== pconstant VotingReady

            pguardC "Proposal time should be wthin the voting period" $
              isVotingPeriod # proposalInputDatumF.timingConfig
                # proposalInputDatumF.startingTime
                #$ passertPJust
                # "Should be able to get current time"
                # currentTime

            -- Ensure the transaction is voting to a valid 'ResultTag'(outcome).
            PProposalVotes voteMap <- pmatchC proposalInputDatumF.votes
            voteFor <- pletC $ pfromData $ pfield @"resultTag" # r

            pguardC "Vote option should be valid" $
              pisJust #$ plookup # voteFor # voteMap

            -- Ensure that no lock with the current proposal id has been put on the stake.
            pguardC "Same stake shouldn't vote on the same proposal twice" $
              pnot #$ pisVoter #$ pgetStakeRole # proposalInputDatumF.proposalId # stakeInF.lockedBy

            let -- The amount of new votes should be the 'stakedAmount'.
                -- Update the vote counter of the proposal, and leave other stuff as is.
                expectedNewVotes =
                  pcon $
                    PProposalVotes $
                      pupdate
                        # plam
                          ( \votes -> unTermCont $ do
                              PDiscrete v <- pmatchC stakeInF.stakedAmount
                              pure $ pcon $ PJust $ votes + (pextract # v)
                          )
                        # voteFor
                        # pto (pfromData proposalInputDatumF.votes)

                expectedProposalOut =
                  mkRecordConstr
                    PProposalDatum
                    ( #proposalId .= proposalInputDatumF.proposalId
                        .& #effects .= proposalInputDatumF.effects
                        .& #status .= proposalInputDatumF.status
                        .& #cosigners .= proposalInputDatumF.cosigners
                        .& #thresholds .= proposalInputDatumF.thresholds
                        .& #votes .= pdata expectedNewVotes
                        .& #timingConfig .= proposalInputDatumF.timingConfig
                        .& #startingTime .= proposalInputDatumF.startingTime
                    )

            pguardC "Output proposal should be valid" $
              proposalOutputDatum #== expectedProposalOut

            -- We validate the output stake datum here as well: We need the vote option
            -- to create a valid 'ProposalLock', however the vote option is encoded
            -- in the proposal redeemer, which is invisible for the stake validator.

            let newProposalLock =
                  mkRecordConstr
                    PVoted
                    ( #votedOn .= proposalInputDatumF.proposalId
                        .& #votedFor .= pdata voteFor
                    )

                -- Prepend the new lock to existing locks
                expectedProposalLocks =
                  pcons
                    # pdata newProposalLock
                    # pfromData stakeInF.lockedBy

            pguardC "Output stake should be locked by the proposal" $
              pfield @"lockedBy" # sctxF.outputStake #== expectedProposalLocks

          ----------------------------------------------------------------------

          PUnlock _ -> spendSingleStake $ \sctxF -> do
            stakeInF <- pletAllC $ pto sctxF.inputStake

            stakeRole <- pletC $ pgetStakeRole # proposalInputDatumF.proposalId # stakeInF.lockedBy

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
                    ( pgetStakeRole # proposalInputDatumF.proposalId # locks
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
                      expectedVotes =
                        pretractVotes
                          # (pextractVoteOption # stakeRole)
                          # retractCount
                          # proposalInputDatumF.votes

                      expectedProposalOut =
                        mkRecordConstr
                          PProposalDatum
                          ( #proposalId .= proposalInputDatumF.proposalId
                              .& #effects .= proposalInputDatumF.effects
                              .& #status .= proposalInputDatumF.status
                              .& #cosigners .= proposalInputDatumF.cosigners
                              .& #thresholds .= proposalInputDatumF.thresholds
                              .& #votes .= pdata expectedVotes
                              .& #timingConfig .= proposalInputDatumF.timingConfig
                              .& #startingTime .= proposalInputDatumF.startingTime
                          )
                   in ptraceIfFalse "Update votes" $
                        expectedProposalOut #== proposalOutputDatum
                )
                -- No change to the proposal is allowed.
                ( ptraceIfFalse "Proposal unchanged" $
                    proposalOutputDatum #== proposalInputDatum
                )

            -- At last, we ensure that all locks belong to this proposal will be removed.
            stakeOutputLocks <- pletC $ pfield @"lockedBy" # pto sctxF.outputStake

            pguardC "All relevant locks removed from the stake" $
              validateOutputLocks # stakeOutputLocks

          ----------------------------------------------------------------------

          PAdvanceProposal _ -> unTermCont $ do
            currentTime' <-
              pletC $
                passertPJust
                  # "Should be able to get current time"
                  # currentTime

            applyIs <- pletC $
              plam $ \f ->
                f
                  # proposalInputDatumF.timingConfig
                  # proposalInputDatumF.startingTime
                  # currentTime'
            let inDraftPeriod = applyIs # isDraftPeriod
                inVotingPeriod = applyIs # isVotingPeriod
                inExecutionPeriod = applyIs # isExecutionPeriod

            inLockedPeriod <- pletC $ applyIs # isLockingPeriod

            proposalOutputStatus <-
              pletC $
                pfromData $
                  pfield @"status" # pto proposalOutputDatum

            pguardC "Only status changes in the output proposal" $
              let expectedProposalOutputDatum =
                    mkRecordConstr
                      PProposalDatum
                      ( #proposalId .= proposalInputDatumF.proposalId
                          .& #effects .= proposalInputDatumF.effects
                          .& #status .= pdata proposalOutputStatus
                          .& #cosigners .= proposalInputDatumF.cosigners
                          .& #thresholds .= proposalInputDatumF.thresholds
                          .& #votes .= proposalInputDatumF.votes
                          .& #timingConfig .= proposalInputDatumF.timingConfig
                          .& #startingTime .= proposalInputDatumF.startingTime
                      )
               in proposalOutputDatum #== expectedProposalOutputDatum

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
                            # proposalInputDatumF.cosigners

                        -- 'Draft' -> 'VotingReady'
                        pguardC "Proposal status set to VotingReady" $
                          proposalOutputStatus #== pconstant VotingReady
                      -- Too late: failed proposal, status set to 'Finished'.
                      PFalse ->
                        pguardC "Proposal should fail: not on time" $
                          proposalOutputStatus #== pconstant Finished

                ----------------------------------------------------------------

                PVotingReady -> unTermCont $ do
                  let notTooLate = inLockedPeriod
                      notTooEarly = pnot # inVotingPeriod

                  pguardC "Cannot advance ahead of time" notTooEarly

                  pmatchC notTooLate >>= \case
                    PTrue -> do
                      -- 'VotingReady' -> 'Locked'
                      pguardC "Proposal status set to Locked" $
                        proposalOutputStatus #== pconstant Locked

                      pguardC "Winner outcome not found" $
                        pisJust #$ pwinner' # proposalInputDatumF.votes
                          #$ punsafeCoerce
                          $ pfromData thresholdsF.execute
                    -- Too late: failed proposal, status set to 'Finished'.
                    PFalse ->
                      pguardC "Proposal should fail: not on time" $
                        proposalOutputStatus #== pconstant Finished

                  pure $ popaque $ pconstant ()

                ----------------------------------------------------------------

                PLocked -> unTermCont $ do
                  let notTooLate = inExecutionPeriod
                      notTooEarly = pnot # inLockedPeriod

                  pguardC "Not too early" notTooEarly

                  pguardC "Proposal status set to Finished" $
                    proposalOutputStatus #== pconstant Finished

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

                ----------------------------------------------------------------

                PFinished -> ptraceError "Finished proposals cannot be advanced"
