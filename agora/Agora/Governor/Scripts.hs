{- |
Module     : Agora.Governor.Scripts
Maintainer : connor@mlabs.city
Description: Plutus scripts for Governors.

Plutus scripts for Governors.
-}
module Agora.Governor.Scripts (
  -- * GST
  -- $gst

  -- * Scripts
  governorPolicy,
  governorValidator,
) where

--------------------------------------------------------------------------------

import Agora.AuthorityToken (
  authorityTokensValidIn,
  singleAuthorityTokenBurned,
 )
import Agora.Governor (
  PGovernorDatum (PGovernorDatum),
  PGovernorRedeemer (..),
  pgetNextProposalId,
  pisGovernorDatumValid,
 )
import Agora.Proposal (
  PProposalDatum (..),
  PProposalEffectGroup,
  ProposalStatus (Draft, Locked),
  phasNeutralEffect,
  pisEffectsVotesCompatible,
  pisVotesEmpty,
  pneutralOption,
  pwinner,
 )
import Agora.Proposal.Time (validateProposalStartingTime)
import Agora.Scripts (
  AgoraScripts,
  authorityTokenSymbol,
  governorSTSymbol,
  proposalSTSymbol,
  proposalValidatoHash,
  stakeSTSymbol,
 )
import Agora.Stake (
  PProposalLock (..),
  PStakeDatum (..),
  pnumCreatedProposals,
 )
import Agora.Utils (
  pscriptHashToTokenName,
  validatorHashToAddress,
 )
import Plutarch.Api.V1 (
  PCurrencySymbol,
  PMap (PMap),
  PTokenName,
  PValue (PValue),
 )
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V2 (
  PAddress,
  PMintingPolicy,
  PScriptPurpose (PMinting, PSpending),
  PTxOut,
  PValidator,
 )
import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.Extra.AssetClass (passetClass, passetClassValueOf)
import Plutarch.Extra.Field (pletAllC)
import Plutarch.Extra.List (pfirstJust)
import Plutarch.Extra.Map (ptryLookup)
import Plutarch.Extra.Maybe (passertPJust, pmaybe, pmaybeData, pnothing)
import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import Plutarch.Extra.ScriptContext (
  pfindOutputsToAddress,
  pfindTxInByTxOutRef,
  pfromDatumHash,
  pfromOutputDatum,
  pisUTXOSpent,
  pscriptHashFromAddress,
  ptryFromOutputDatum,
  pvalueSpent,
 )
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC, ptryFromC)
import Plutarch.Extra.Value (psymbolValueOf)
import PlutusLedgerApi.V1 (TxOutRef)

--------------------------------------------------------------------------------

{- $gst
   Governance state token, aka. GST, is an NFT that identifies a UTXO that
    carries the state datum of the Governance script.

   This token is minted by a one-shot monetary policy 'governorPolicy',
    meaning that the token has guaranteed uniqueness.

   The 'governorValidator' ensures that exactly one GST stays
    at the address of itself forever.
-}

--------------------------------------------------------------------------------

{- | Policy for minting GSTs.

   This policy perform the following checks:

    - The UTXO referenced in the parameter is spent in the transaction.
    - Exactly one GST is minted.
    - Ensure the token name is empty.
    - Said UTXO should carry a valid 'Agora.Governor.GovernorDatum'.

  NOTE: It's user's responsibility to make sure the token is sent to the corresponding governor validator.
        We /can't/ really check this in the policy, otherwise we create a cyclic reference issue.

  @since 0.1.0
-}
governorPolicy :: TxOutRef -> ClosedTerm PMintingPolicy
governorPolicy initialSpend =
  plam $ \_ ctx' -> unTermCont $ do
    let oref = pconstant initialSpend

    PMinting ((pfield @"_0" #) -> ownSymbol) <- pmatchC (pfromData $ pfield @"purpose" # ctx')
    let ownAssetClass = passetClass # ownSymbol # pconstant ""
        txInfo = pfromData $ pfield @"txInfo" # ctx'

    txInfoF <- pletFieldsC @'["mint", "inputs", "outputs", "datums", "validRange"] txInfo

    pguardC "Referenced utxo should be spent" $
      pisUTXOSpent # oref # txInfoF.inputs

    pguardC "Exactly one token should be minted" $
      psymbolValueOf # ownSymbol # txInfoF.mint #== 1
        #&& passetClassValueOf # txInfoF.mint # ownAssetClass #== 1

    govOutput <-
      pletC $
        passertPJust
          # "Governor output not found"
            #$ pfind
          # plam
            ( \((pfield @"value" #) -> value) ->
                psymbolValueOf # ownSymbol # value #== 1
            )
          # pfromData txInfoF.outputs

    let outputDatum = pfield @"datum" # govOutput
        datum = pfromOutputDatum @PGovernorDatum # outputDatum # txInfoF.datums

    pguardC "Governor output datum valid" $ pisGovernorDatumValid # datum

    pure $ popaque $ pconstant ()

{- | Validator for Governors.

     == Common checks

     The validator always ensures:

       - The UTXO which holds the GST must be spent.
       - The GST always stays at the validator's address.
       - The new state UTXO has a valid datum of type 'Agora.Governor.GovernorDatum'.

     == Creating a Proposal

     When the redeemer is 'Agora.Governor.CreateProposal', the script will check:

     - For governor's state datum:

         * 'Agora.Governor.nextProposalId' is advanced.
         * Nothing is changed other that that.

     - Exactly one stake (the "input stake") must be provided in the input:
         * At least 'Agora.Stake.stackedAmount' of GT must be spent in the transaction.
         * The transaction must be signed by the stake owner.

     - Exactly one new proposal state token is minted.
     - An UTXO which holds the newly minted proposal state token is sent to the proposal validator.
       This UTXO must have a valid datum of type 'Agora.Proposal.ProposalDatum', the datum must:

         * Copy its id and thresholds from the governor's state.
         * Have status set to 'Proposal.Draft'.
         * Have zero votes.
         * Have exactly one cosigner - the stake owner

     - An UTXO which holds the stake state token is sent back to the stake validator.
       This UTXO must have a valid datum of type 'Agora.Stake.StakeDatum':

         * The 'Agora.Stake.stakedAmount' and 'Agora.Stake.owner' should not be changed,
            comparing to the input stake.
         * The new proposal locks must be appended to the 'Agora.Stake.lockedBy'.

     == Minting GATs

     When the redeemer is 'Agora.Governor.MintGATs', the script will check:

     - Governor's state is not changed.
     - Exactly only one proposal is in the inputs. Let's call this the /input proposal/.
     - The proposal is in the 'Proposal.Executable' state.

     NOTE: The input proposal is found by looking for the UTXO with a proposal state token in the inputs.

     === Effect Group Selection

     Currently a proposal can have two or more than two options to vote on,
       meaning that it can contains two or more effect groups,
       according to [#39](https://github.com/Liqwid-Labs/agora/issues/39).

     Either way, the shapes of 'Proposal.votes' and 'Proposal.effects' should be the same.
       This is checked by 'Proposal.proposalDatumValid'.

     The script will look at the the 'Proposal.votes' to determine which group has the highest votes,
       said group shoud be executed.

     During the process, minimum votes requirement will also be enforced.

     Next, the script will:

     - Ensure that for every effect in the said effect group,
       exactly one valid GAT is minted and sent to the effect.
     - The amount of GAT minted in the transaction should be equal to the number of effects.
     - A new UTXO is sent to the proposal validator, this UTXO should:

         * Include the one proposal state token.
         * Have a valid datum of type 'Proposal.ProposalDatum'.
           This datum should be as same as the one of the input proposal,
           except its status should be 'Proposal.Finished'.

     == Changing the State

     Redeemer 'Agora.Governor.MutateGovernor' allows the state datum to be changed by an external effect.

     In this case, the script will check

     - Exactly one GAT is burnt in the transaction.
     - Said GAT is tagged by the effect.

     @since 0.1.0
-}
governorValidator ::
  -- | Lazy precompiled scripts.
  AgoraScripts ->
  ClosedTerm PValidator
governorValidator as =
  plam $ \datum' redeemer' ctx' -> unTermCont $ do
    ctxF <- pletAllC ctx'

    redeemer <- pfromData . fst <$> ptryFromC redeemer'

    txInfo' <- pletC $ pfromData $ ctxF.txInfo
    txInfoF <- pletFieldsC @'["mint", "inputs", "outputs", "datums", "signatories", "validRange"] txInfo'

    PSpending (pfromData . (pfield @"_0" #) -> ownInputRef) <- pmatchC $ pfromData ctxF.purpose

    ((pfield @"resolved" #) -> ownInput) <-
      pletC $
        passertPJust # "Own input not found"
          #$ pfindTxInByTxOutRef # ownInputRef # txInfoF.inputs
    ownInputF <- pletFieldsC @'["address", "value"] ownInput
    let ownAddress = pfromData $ ownInputF.address

    (oldGovernorDatum :: Term _ PGovernorDatum, _) <- ptryFromC datum'
    oldGovernorDatumF <- pletAllC oldGovernorDatum

    -- Check that GST will be returned to the governor.
    let ownInputGSTAmount = psymbolValueOf # pgstSymbol # ownInputF.value
    pguardC "Own input should have exactly one state token" $
      ownInputGSTAmount #== 1

    ownOutputs <- pletC $ pfindOutputsToAddress # txInfoF.outputs # ownAddress
    pguardC "Exactly one utxo should be sent to the governor" $
      plength # ownOutputs #== 1

    ownOutput <- pletFieldsC @'["value", "datum"] $ phead # ownOutputs
    let ownOuputGSTAmount = psymbolValueOf # pgstSymbol # ownOutput.value
    pguardC "State token should stay at governor's address" $
      ownOuputGSTAmount #== 1

    -- Check that own output have datum of type 'GovernorDatum'.
    newGovernorDatum <-
      pletC $ pfromOutputDatum @PGovernorDatum # ownOutput.datum # txInfoF.datums

    pguardC "New datum is valid" $ pisGovernorDatumValid # newGovernorDatum

    pure $
      pmatch redeemer $ \case
        PCreateProposal -> unTermCont $ do
          -- Check that the transaction advances proposal id.

          let expectedNextProposalId = pgetNextProposalId # oldGovernorDatumF.nextProposalId
              expectedNewDatum =
                mkRecordConstr
                  PGovernorDatum
                  ( #proposalThresholds .= oldGovernorDatumF.proposalThresholds
                      .& #nextProposalId .= pdata expectedNextProposalId
                      .& #proposalTimings .= oldGovernorDatumF.proposalTimings
                      .& #createProposalTimeRangeMaxWidth
                        .= oldGovernorDatumF.createProposalTimeRangeMaxWidth
                      .& #maximumProposalsPerStake
                        .= oldGovernorDatumF.maximumProposalsPerStake
                  )
          pguardC "Unexpected governor state datum" $
            newGovernorDatum #== expectedNewDatum

          -- Check that exactly one proposal token is being minted.

          pguardC "Exactly one proposal token must be minted" $
            let vMap = pfromData $ pto txInfoF.mint
                tnMap = plookup # ppstSymbol # vMap
                -- Ada and PST
                onlyPST = plength # pto vMap #== 2
                onePST =
                  pmaybe
                    # pconstant False
                    # plam (#== AssocMap.psingleton # pconstant "" # 1)
                    # tnMap
             in onlyPST #&& onePST

          -- Check that a stake is spent to create the propsal,
          --   and the value it contains meets the requirement.

          stakeInputs <-
            pletC $
              pfilter
                # phoistAcyclic
                  ( plam $
                      \((pfield @"value" #) . (pfield @"resolved" #) -> value) ->
                        psymbolValueOf # psstSymbol # value #== 1
                  )
                # pfromData txInfoF.inputs

          pguardC "Can process only one stake" $
            plength # stakeInputs #== 1

          stakeInput <- pletC $ phead # stakeInputs

          stakeInputF <- pletFieldsC @'["datum", "value"] $ pfield @"resolved" # stakeInput

          let stakeInputDatum = pfromOutputDatum @(PAsData PStakeDatum) # stakeInputF.datum # txInfoF.datums

          stakeInputDatumF <- pletAllC $ pto $ pfromData stakeInputDatum

          pguardC "Proposals created by the stake must not exceed the number stored in the governor." $
            pnumCreatedProposals # stakeInputDatumF.lockedBy
              #< oldGovernorDatumF.maximumProposalsPerStake

          -- Check that the newly minted PST is sent to the proposal validator,
          --   and the datum it carries is legal.

          outputsToProposalValidatorWithStateToken <-
            pletC $
              pfilter
                # phoistAcyclic
                  ( plam $
                      \txOut' -> unTermCont $ do
                        txOut <- pletFieldsC @'["address", "value"] txOut'

                        pure $
                          txOut.address #== pdata pproposalValidatorAddress
                            #&& psymbolValueOf # ppstSymbol # txOut.value #== 1
                  )
                # pfromData txInfoF.outputs

          pguardC "Exactly one UTXO with proposal state token should be sent to the proposal validator" $
            plength # outputsToProposalValidatorWithStateToken #== 1

          proposalOutputDatum' <-
            pletC $
              pfromOutputDatum @(PAsData PProposalDatum)
                # (pfield @"datum" #$ phead # outputsToProposalValidatorWithStateToken)
                # txInfoF.datums

          proposalOutputDatum <- pletAllC $ pto $ pfromData proposalOutputDatum'

          let expectedCosigners = psingleton @PBuiltinList # stakeInputDatumF.owner

          pguardC "Proposal datum correct" $
            foldl1
              (#&&)
              [ ptraceIfFalse "has neutral effect" $
                  phasNeutralEffect # proposalOutputDatum.effects
              , ptraceIfFalse "votes have valid shape" $
                  pisEffectsVotesCompatible # proposalOutputDatum.effects # proposalOutputDatum.votes
              , ptraceIfFalse "votes are empty" $
                  pisVotesEmpty # proposalOutputDatum.votes
              , ptraceIfFalse "id correct" $
                  proposalOutputDatum.proposalId #== oldGovernorDatumF.nextProposalId
              , ptraceIfFalse "status is Draft" $
                  proposalOutputDatum.status #== pconstantData Draft
              , ptraceIfFalse "cosigners correct" $
                  plistEquals # pfromData proposalOutputDatum.cosigners # expectedCosigners
              , ptraceIfFalse "starting time valid" $
                  validateProposalStartingTime
                    # oldGovernorDatumF.createProposalTimeRangeMaxWidth
                    # txInfoF.validRange
                    # proposalOutputDatum.startingTime
              , ptraceIfFalse "copy over configurations" $
                  proposalOutputDatum.thresholds #== oldGovernorDatumF.proposalThresholds
                    #&& proposalOutputDatum.timingConfig #== oldGovernorDatumF.proposalTimings
              ]

          -- Check the output stake has been proposly updated.
          let stakeOutputDatum =
                passertPJust # "Output stake should be presented"
                  #$ pfirstJust
                    # plam
                      ( \txOut -> unTermCont $ do
                          txOutF <- pletFieldsC @'["datum", "value"] txOut

                          pure $
                            pif
                              (psymbolValueOf # psstSymbol # txOutF.value #== 1)
                              (ptryFromOutputDatum @(PAsData PStakeDatum) # txOutF.datum # txInfoF.datums)
                              (pcon PNothing)
                      )
                    # pfromData txInfoF.outputs

              stakeOutputLocks =
                pfromData $ pfield @"lockedBy" #$ pto $ pfromData stakeOutputDatum

              -- The stake should be locked by the newly created proposal.
              newLock =
                mkRecordConstr
                  PCreated
                  ( #created .= oldGovernorDatumF.nextProposalId
                  )

              -- Append new locks to existing locks
              expectedProposalLocks =
                pcons # pdata newLock # stakeInputDatumF.lockedBy

          pguardC "Stake output locks correct" $
            plistEquals # stakeOutputLocks # expectedProposalLocks

          pure $ popaque $ pconstant ()

        --------------------------------------------------------------------------

        PMintGATs -> unTermCont $ do
          pguardC "Governor state should not be changed" $ newGovernorDatum #== oldGovernorDatum

          -- Filter out proposal inputs and ouputs using PST and the address of proposal validator.

          pguardC "The governor can only process one proposal at a time" $
            (psymbolValueOf # ppstSymbol #$ pvalueSpent # txInfoF.inputs) #== 1

          proposalInputDatum <-
            pletC $
              passertPJust
                # "Proposal input not found"
                  #$ pfirstJust
                # plam
                  ( \((pfield @"resolved" #) -> txOut) -> unTermCont $ do
                      txOutF <- pletFieldsC @'["address", "value", "datum"] txOut

                      pure $
                        pif
                          ( psymbolValueOf # ppstSymbol # txOutF.value #== 1
                              #&& txOutF.address #== pdata pproposalValidatorAddress
                          )
                          (ptryFromOutputDatum @(PAsData PProposalDatum) # txOutF.datum # txInfoF.datums)
                          pnothing
                  )
                # pfromData txInfoF.inputs

          proposalInputDatumF <-
            pletFieldsC @'["effects", "status", "thresholds", "votes"] $
              pto $ pfromData proposalInputDatum

          -- Check that the proposal state is advanced so that a proposal cannot be executed twice.

          pguardC "Proposal must be in locked(executable) state in order to execute effects" $
            proposalInputDatumF.status #== pconstantData Locked

          -- TODO: anything else to check here?

          -- Find the highest votes and the corresponding tag.
          let quorum = pto $ pto $ pfromData $ pfield @"execute" # proposalInputDatumF.thresholds
              neutralOption = pneutralOption # proposalInputDatumF.effects
              finalResultTag = pwinner # proposalInputDatumF.votes # quorum # neutralOption

          -- The effects of the winner outcome.
          effectGroup <- pletC $ ptryLookup # finalResultTag #$ proposalInputDatumF.effects

          gatCount <- pletC $ plength #$ pto $ pto effectGroup

          pguardC "Required amount of GATs should be minted" $
            psymbolValueOf # atSymbol # txInfoF.mint #== gatCount

          -- Ensure that every GAT goes to one of the effects in the winner effect group.
          outputsWithGAT <-
            pletC $
              pfilter
                # phoistAcyclic
                  ( plam
                      ( \((pfield @"value" #) -> value) ->
                          0 #< psymbolValueOf # atSymbol # value
                      )
                  )
                # pfromData txInfoF.outputs

          pguardC "Output GATs is more than minted GATs" $
            plength # outputsWithGAT #== gatCount

          -- For a given output, check if it contains a single valid GAT
          -- and whether it correctly belongs to the group.
          let validateGATOutput' ::
                forall (s :: S).
                Term s (PProposalEffectGroup :--> PTxOut :--> PBool)
              validateGATOutput' =
                phoistAcyclic $
                  plam
                    ( \effects output -> unTermCont $ do
                        outputF <- pletFieldsC @'["address", "datum", "value"] output
                        PValue value <- pmatchC $ outputF.value
                        PMap authorityTokens <-
                          pmatchC $
                            passertPJust # "validateGATOutput': Must have GAT in GAT output"
                              #$ plookup # atSymbol # value

                        let tagToken :: Term _ PTokenName
                            tagToken =
                              pmaybeData # pconstant "" # plam (pscriptHashToTokenName . pfromData)
                                #$ pfield @"scriptHash" # effect
                            receiverScriptHash =
                              passertPJust # "GAT receiver should be a script"
                                #$ pscriptHashFromAddress # outputF.address
                            effect =
                              passertPJust # "Receiver should be in the effect group"
                                #$ AssocMap.plookup # receiverScriptHash # effects
                            valueGATCorrect =
                              authorityTokens
                                #== psingleton # (ppairDataBuiltin # pdata tagToken # pdata 1)
                            hasCorrectDatum =
                              pfield @"datumHash" # effect #== pfromDatumHash # outputF.datum

                        pure $
                          foldr1
                            (#&&)
                            [ ptraceIfFalse "GAT valid" $ authorityTokensValidIn # atSymbol # output
                            , ptraceIfFalse "Correct datum" hasCorrectDatum
                            , ptraceIfFalse "Value correctly encodes Auth Check script" valueGATCorrect
                            ]
                    )

              validateGATOutput = validateGATOutput' # effectGroup

          pguardC "GATs valid" $
            pfoldr
              # plam
                ( \txOut r ->
                    let value = pfield @"value" # txOut
                        atValue = psymbolValueOf # atSymbol # value
                     in pif (atValue #== 0) r $
                          pif (atValue #== 1) (r #&& validateGATOutput # txOut) $ pconstant False
                )
              # pconstant True
              # pfromData txInfoF.outputs

          pure $ popaque $ pconstant ()

        --------------------------------------------------------------------------

        PMutateGovernor -> unTermCont $ do
          -- Check that a GAT is burnt.
          pguardC "One valid GAT burnt" $
            singleAuthorityTokenBurned atSymbol txInfoF.inputs txInfoF.mint

          pure $ popaque $ pconstant ()
  where
    -- The currency symbol of authority token.
    atSymbol :: forall (s :: S). Term s PCurrencySymbol
    atSymbol = pconstant $ authorityTokenSymbol as

    -- The currency symbol of the proposal state token.
    ppstSymbol :: Term s PCurrencySymbol
    ppstSymbol = pconstant $ proposalSTSymbol as

    -- The address of the proposal validator.
    pproposalValidatorAddress :: Term s PAddress
    pproposalValidatorAddress =
      pconstant $
        validatorHashToAddress $
          proposalValidatoHash as

    -- The currency symbol of the stake state token.
    psstSymbol :: Term s PCurrencySymbol
    psstSymbol = pconstant $ stakeSTSymbol as

    -- The currency symbol of the governor state token.
    pgstSymbol :: Term s PCurrencySymbol
    pgstSymbol = pconstant $ governorSTSymbol as
