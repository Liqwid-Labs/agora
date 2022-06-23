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

  -- * Bridges
  governorSTSymbolFromGovernor,
  governorSTAssetClassFromGovernor,
  proposalSTAssetClassFromGovernor,
  stakeSTSymbolFromGovernor,
  stakeFromGovernor,
  stakeValidatorHashFromGovernor,
  proposalFromGovernor,
  proposalValidatorHashFromGovernor,
  proposalSTSymbolFromGovernor,
  stakeSTAssetClassFromGovernor,
  governorValidatorHash,
  authorityTokenFromGovernor,
  authorityTokenSymbolFromGovernor,
) where

--------------------------------------------------------------------------------

import Agora.AuthorityToken (
  AuthorityToken (..),
  authorityTokenPolicy,
  authorityTokensValidIn,
  singleAuthorityTokenBurned,
 )
import Agora.Governor (
  Governor (gstOutRef, gtClassRef, maximumCosigners),
  PGovernorDatum (PGovernorDatum),
  PGovernorRedeemer (PCreateProposal, PMintGATs, PMutateGovernor),
  governorDatumValid,
  pgetNextProposalId,
 )
import Agora.Proposal (
  PProposalDatum (..),
  PProposalId (..),
  PProposalStatus (PFinished),
  PResultTag,
  Proposal (..),
  ProposalStatus (Draft, Locked),
  pemptyVotesFor,
  pneutralOption,
  proposalDatumValid,
  pwinner,
 )
import Agora.Proposal.Scripts (
  proposalPolicy,
  proposalValidator,
 )
import Agora.Proposal.Time (createProposalStartingTime)
import Agora.SafeMoney (GTTag)
import Agora.Stake (
  PProposalLock (..),
  PStakeDatum (..),
  Stake (..),
 )
import Agora.Stake.Scripts (
  stakePolicy,
  stakeValidator,
 )
import Agora.Utils (
  findOutputsToAddress,
  hasOnlyOneTokenOfCurrencySymbol,
  mustBePDJust,
  mustBePJust,
  mustFindDatum',
  scriptHashFromAddress,
  validatorHashToAddress,
  validatorHashToTokenName,
 )
import Plutarch.Extra.Record

--------------------------------------------------------------------------------

import Plutarch.Api.V1 (
  PAddress,
  PCurrencySymbol,
  PDatumHash,
  PMap,
  PMintingPolicy,
  PScriptPurpose (PMinting, PSpending),
  PTxOut,
  PValidator,
  PValidatorHash,
  PValue,
  mintingPolicySymbol,
  mkMintingPolicy,
  mkValidator,
  validatorHash,
 )
import Plutarch.Api.V1.AssetClass (
  passetClass,
  passetClassValueOf,
 )
import Plutarch.Extra.Map (
  pkeys,
  plookup,
  plookup',
 )
import Plutarch.SafeMoney (PDiscrete (..), pvalueDiscrete')
import Plutarch.TryFrom ()

--------------------------------------------------------------------------------

import Plutarch.Api.V1.ScriptContext (pfindTxInByTxOutRef, pisUTXOSpent, ptryFindDatum, ptxSignedBy, pvalueSpent)
import "liqwid-plutarch-extra" Plutarch.Api.V1.Value (psymbolValueOf)
import Plutarch.Extra.Maybe (pisDJust)
import Plutarch.Extra.TermCont
import PlutusLedgerApi.V1 (
  CurrencySymbol (..),
  MintingPolicy,
 )
import PlutusLedgerApi.V1.Scripts (ValidatorHash (..))
import PlutusLedgerApi.V1.Value (
  AssetClass (..),
 )

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
governorPolicy :: Governor -> ClosedTerm PMintingPolicy
governorPolicy gov =
  plam $ \_ ctx' -> unTermCont $ do
    let oref = pconstant gov.gstOutRef

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
        mustBePJust
          # "Governor output not found"
            #$ pfind
          # plam
            ( \((pfield @"value" #) . pfromData -> value) ->
                psymbolValueOf # ownSymbol # value #== 1
            )
          # pfromData txInfoF.outputs

    let datumHash = pfield @"datumHash" # pfromData govOutput
        datum = mustFindDatum' @PGovernorDatum # datumHash # txInfoF.datums

    pure $ popaque $ governorDatumValid # datum

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
governorValidator :: Governor -> ClosedTerm PValidator
governorValidator gov =
  plam $ \datum' redeemer' ctx' -> unTermCont $ do
    (pfromData -> redeemer, _) <- ptryFromC redeemer'
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx'

    txInfo' <- pletC $ pfromData $ ctxF.txInfo
    txInfoF <- pletFieldsC @'["mint", "inputs", "outputs", "datums", "signatories", "validRange"] txInfo'

    PSpending (pfromData . (pfield @"_0" #) -> ownInputRef) <- pmatchC $ pfromData ctxF.purpose

    ((pfield @"resolved" #) -> ownInput) <-
      pletC $
        mustBePJust # "Own input not found"
          #$ pfindTxInByTxOutRef # ownInputRef # txInfoF.inputs
    ownInputF <- pletFieldsC @'["address", "value"] ownInput
    let ownAddress = pfromData $ ownInputF.address

    (pfromData -> (oldGovernorDatum :: Term _ PGovernorDatum), _) <- ptryFromC datum'
    oldGovernorDatumF <-
      pletFieldsC
        @'[ "proposalThresholds"
          , "nextProposalId"
          , "proposalTimings"
          , "createProposalTimeRangeMaxWidth"
          ]
        oldGovernorDatum

    -- Check that GST will be returned to the governor.
    let ownInputGSTAmount = psymbolValueOf # pgstSymbol # ownInputF.value
    pguardC "Own input should have exactly one state token" $
      ownInputGSTAmount #== 1

    ownOutputs <- pletC $ findOutputsToAddress # txInfoF.outputs # ownAddress
    pguardC "Exactly one utxo should be sent to the governor" $
      plength # ownOutputs #== 1

    ownOutput <- pletFieldsC @'["value", "datumHash"] $ phead # ownOutputs
    let ownOuputGSTAmount = psymbolValueOf # pgstSymbol # ownOutput.value
    pguardC "State token should stay at governor's address" $
      ownOuputGSTAmount #== 1

    -- Check that own output have datum of type 'GovernorDatum'.
    let outputGovernorStateDatumHash =
          mustBePDJust # "Governor output doesn't have datum" # ownOutput.datumHash
    newGovernorDatum <-
      pletC $
        pfromData $
          mustBePJust # "Ouput governor state datum not found"
            #$ ptryFindDatum # outputGovernorStateDatumHash # txInfoF.datums
    pguardC "New datum is not valid" $ governorDatumValid # newGovernorDatum

    pure $
      pmatch redeemer $ \case
        PCreateProposal _ -> unTermCont $ do
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
                  )
          pguardC "Unexpected governor state datum" $
            newGovernorDatum #== expectedNewDatum

          -- Check that exactly one proposal token is being minted.

          pguardC "Exactly one proposal token must be minted" $
            hasOnlyOneTokenOfCurrencySymbol # ppstSymbol # txInfoF.mint

          -- Check that a stake is spent to create the propsal,
          --   and the value it contains meets the requirement.

          stakeInput <-
            pletC $
              mustBePJust # "Stake input not found" #$ pfind
                # phoistAcyclic
                  ( plam $
                      \((pfield @"resolved" #) -> txOut') -> unTermCont $ do
                        txOut <- pletFieldsC @'["address", "value"] txOut'

                        pure $
                          txOut.address #== pdata pstakeValidatorAddress
                            #&& psymbolValueOf # psstSymbol # txOut.value #== 1
                  )
                # pfromData txInfoF.inputs

          stakeInputF <- pletFieldsC @'["datumHash", "value"] $ pfield @"resolved" # stakeInput

          pguardC "Stake input doesn't have datum" $
            pisDJust # stakeInputF.datumHash

          let stakeInputDatum = mustFindDatum' @PStakeDatum # stakeInputF.datumHash # txInfoF.datums

          stakeInputDatumF <-
            pletFieldsC @["stakedAmount", "owner", "lockedBy"] stakeInputDatum

          pguardC "Required amount of stake GTs should be presented" $
            stakeInputDatumF.stakedAmount #== (pgtValueOf # stakeInputF.value)

          -- TODO: Is this required?
          pguardC "Tx should be signed by the stake owner" $
            ptxSignedBy # txInfoF.signatories # stakeInputDatumF.owner

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

          outputDatumHash <- pletC $ pfield @"datumHash" #$ phead # outputsToProposalValidatorWithStateToken

          proposalOutputDatum' <-
            pletC $
              mustFindDatum' @PProposalDatum
                # outputDatumHash
                # txInfoF.datums

          pguardC "Proposal datum must be valid" $
            proposalDatumValid' # proposalOutputDatum'

          proposalOutputDatum <-
            pletFieldsC
              @'["effects", "cosigners", "proposalId", "votes"]
              proposalOutputDatum'

          pguardC "Proposal should have only one cosigner" $
            plength # pfromData proposalOutputDatum.cosigners #== 1

          let -- Votes should be empty at this point
              expectedVotes = pemptyVotesFor # pfromData proposalOutputDatum.effects
              expectedStartingTime =
                createProposalStartingTime
                  # oldGovernorDatumF.createProposalTimeRangeMaxWidth
                  # txInfoF.validRange
              -- Id, thresholds and timings should be copied from the old governor state datum.
              expectedProposalOut =
                mkRecordConstr
                  PProposalDatum
                  ( #proposalId .= oldGovernorDatumF.nextProposalId
                      .& #effects .= proposalOutputDatum.effects
                      .& #status .= pconstantData Draft
                      .& #cosigners .= proposalOutputDatum.cosigners
                      .& #thresholds .= oldGovernorDatumF.proposalThresholds
                      .& #votes .= pdata expectedVotes
                      .& #timingConfig .= oldGovernorDatumF.proposalTimings
                      .& #startingTime .= pdata expectedStartingTime
                  )

          pguardC "Datum correct" $ expectedProposalOut #== proposalOutputDatum'

          let cosigner = phead # pfromData proposalOutputDatum.cosigners

          pguardC "Cosigner should be the stake owner" $
            pdata stakeInputDatumF.owner #== cosigner

          -- Check the output stake has been proposly updated.

          stakeOutput <-
            pletC $
              mustBePJust
                # "Stake output not found"
                  #$ pfind
                # phoistAcyclic
                  ( plam $
                      \txOut' -> unTermCont $ do
                        txOut <- pletFieldsC @'["address", "value"] txOut'

                        pure $
                          txOut.address #== pdata pstakeValidatorAddress
                            #&& psymbolValueOf # psstSymbol # txOut.value #== 1
                  )
                # pfromData txInfoF.outputs

          stakeOutputF <- pletFieldsC @'["datumHash", "value"] $ stakeOutput

          pguardC "Staked GTs should be sent back to stake validator" $
            stakeInputDatumF.stakedAmount #== (pgtValueOf # stakeOutputF.value)

          let stakeOutputDatumHash = mustBePDJust # "Stake output should have datum" # stakeOutputF.datumHash

              stakeOutputDatum =
                mustBePJust # "Stake output not found" #$ ptryFindDatum # stakeOutputDatumHash # txInfoF.datums

          -- The stake should be locked by the newly created proposal.

          let possibleVoteResults = pkeys #$ pto $ pfromData proposalOutputDatum.votes

              mkProposalLock :: Term _ (PProposalId :--> PAsData PResultTag :--> PAsData PProposalLock)
              mkProposalLock =
                phoistAcyclic $
                  plam
                    ( \pid rt' ->
                        pdata $
                          mkRecordConstr
                            PProposalLock
                            ( #vote .= rt' .& #proposalTag .= pdata pid
                            )
                    )

              -- Append new locks to existing locks
              expectedProposalLocks =
                pconcat # stakeInputDatumF.lockedBy
                  #$ pmap # (mkProposalLock # proposalOutputDatum.proposalId) # possibleVoteResults

              expectedStakeOutputDatum =
                pdata $
                  mkRecordConstr
                    PStakeDatum
                    ( #stakedAmount .= stakeInputDatumF.stakedAmount
                        .& #owner .= stakeInputDatumF.owner
                        .& #lockedBy .= pdata expectedProposalLocks
                    )

          pguardC "Unexpected stake output datum" $ expectedStakeOutputDatum #== stakeOutputDatum

          pure $ popaque $ pconstant ()

        --------------------------------------------------------------------------

        PMintGATs _ -> unTermCont $ do
          pguardC "Governor state should not be changed" $ newGovernorDatum #== oldGovernorDatum

          -- Filter out proposal inputs and ouputs using PST and the address of proposal validator.

          pguardC "The governor can only process one proposal at a time" $
            (psymbolValueOf # ppstSymbol #$ pvalueSpent # txInfoF.inputs) #== 1

          proposalInputF <-
            pletFieldsC @'["datumHash"] $
              pfield @"resolved"
                #$ pfromData
                $ mustBePJust
                  # "Proposal input not found"
                    #$ pfind
                  # plam
                    ( \((pfield @"resolved" #) -> txOut) -> unTermCont $ do
                        txOutF <- pletFieldsC @'["address", "value"] txOut

                        pure $
                          psymbolValueOf # ppstSymbol # txOutF.value #== 1
                            #&& txOutF.address #== pdata pproposalValidatorAddress
                    )
                  # pfromData txInfoF.inputs

          proposalOutputF <-
            pletFieldsC @'["datumHash"] $
              mustBePJust # "Proposal output not found"
                #$ pfind
                  # plam
                    ( \txOut -> unTermCont $ do
                        txOutF <- pletFieldsC @'["address", "value"] txOut
                        pure $
                          psymbolValueOf # ppstSymbol # txOutF.value #== 1
                            #&& txOutF.address #== pdata pproposalValidatorAddress
                    )
                  # pfromData txInfoF.outputs

          proposalInputDatum <-
            pletC $
              mustFindDatum' @PProposalDatum
                # proposalInputF.datumHash
                # txInfoF.datums
          proposalOutputDatum <-
            pletC $
              mustFindDatum' @PProposalDatum
                # proposalOutputF.datumHash
                # txInfoF.datums

          pguardC "Proposal datum must be valid" $
            proposalDatumValid' # proposalInputDatum
              #&& proposalDatumValid' # proposalOutputDatum

          proposalInputDatumF <-
            pletFieldsC @'["proposalId", "effects", "status", "cosigners", "thresholds", "votes", "timingConfig", "startingTime"]
              proposalInputDatum

          -- Check that the proposal state is advanced so that a proposal cannot be executed twice.

          pguardC "Proposal must be in locked(executable) state in order to execute effects" $
            proposalInputDatumF.status #== pconstantData Locked

          let expectedOutputProposalDatum =
                mkRecordConstr
                  PProposalDatum
                  ( #proposalId .= proposalInputDatumF.proposalId
                      .& #effects .= proposalInputDatumF.effects
                      .& #status .= pdata (pcon $ PFinished pdnil)
                      .& #cosigners .= proposalInputDatumF.cosigners
                      .& #thresholds .= proposalInputDatumF.thresholds
                      .& #votes .= proposalInputDatumF.votes
                      .& #timingConfig .= proposalInputDatumF.timingConfig
                      .& #startingTime .= proposalInputDatumF.startingTime
                  )

          pguardC "Unexpected output proposal datum" $
            pdata proposalOutputDatum #== pdata expectedOutputProposalDatum

          -- TODO: anything else to check here?

          -- Find the highest votes and the corresponding tag.
          let quorum = pto $ pto $ pfromData $ pfield @"execute" # proposalInputDatumF.thresholds
              neutralOption = pneutralOption # proposalInputDatumF.effects
              finalResultTag = pwinner # proposalInputDatumF.votes # quorum # neutralOption

          -- The effects of the winner outcome.
          effectGroup <- pletC $ plookup' # finalResultTag #$ proposalInputDatumF.effects

          gatCount <- pletC $ plength #$ pto $ pto effectGroup

          pguardC "Required amount of GATs should be minted" $
            psymbolValueOf # patSymbol # txInfoF.mint #== gatCount

          -- Ensure that every GAT goes to one of the effects in the winner effect group.
          outputsWithGAT <-
            pletC $
              pfilter
                # phoistAcyclic
                  ( plam
                      ( \((pfield @"value" #) -> value) ->
                          0 #< psymbolValueOf # patSymbol # value
                      )
                  )
                # pfromData txInfoF.outputs

          pguardC "Output GATs is more than minted GATs" $
            plength # outputsWithGAT #== gatCount

          let gatOutputValidator' :: Term s (PMap _ PValidatorHash PDatumHash :--> PAsData PTxOut :--> PBool)
              gatOutputValidator' =
                phoistAcyclic $
                  plam
                    ( \effects (pfromData -> output') -> unTermCont $ do
                        output <- pletFieldsC @'["address", "datumHash"] $ output'

                        let scriptHash =
                              mustBePJust # "GAT receiver is not a script"
                                #$ scriptHashFromAddress # output.address
                            datumHash =
                              mustBePDJust # "Output to effect should have datum"
                                #$ output.datumHash

                            expectedDatumHash =
                              mustBePJust # "Receiver is not in the effect list"
                                #$ plookup # scriptHash # effects

                        pure $
                          foldr1
                            (#&&)
                            [ ptraceIfFalse "GAT must be tagged by the effect hash" $ authorityTokensValidIn # patSymbol # output'
                            , ptraceIfFalse "Unexpected datum" $ datumHash #== expectedDatumHash
                            ]
                    )

              gatOutputValidator = gatOutputValidator' # effectGroup

          pure $
            popaque $
              pfoldr
                # plam
                  ( \txOut r ->
                      let value = pfield @"value" # txOut
                          atValue = psymbolValueOf # patSymbol # value
                       in pif (atValue #== 0) r $
                            pif (atValue #== 1) (r #&& gatOutputValidator # txOut) $ pconstant False
                  )
                # pconstant True
                # pfromData txInfoF.outputs

        --------------------------------------------------------------------------

        PMutateGovernor _ -> unTermCont $ do
          -- Check that a GAT is burnt.
          pure $ popaque $ singleAuthorityTokenBurned patSymbol ctxF.txInfo txInfoF.mint
  where
    -- Get th amount of governance tokens in a value.
    pgtValueOf :: Term s (PValue _ _ :--> PDiscrete GTTag)
    pgtValueOf = phoistAcyclic $ pvalueDiscrete' gov.gtClassRef

    -- The currency symbol of authority token.
    patSymbol :: Term s PCurrencySymbol
    patSymbol = phoistAcyclic $ pconstant $ authorityTokenSymbolFromGovernor gov

    -- The currency symbol of the proposal state token.
    ppstSymbol :: Term s PCurrencySymbol
    ppstSymbol =
      let AssetClass (sym, _) = proposalSTAssetClassFromGovernor gov
       in phoistAcyclic $ pconstant sym

    -- Is a proposal state datum valid?
    proposalDatumValid' :: Term s (PProposalDatum :--> PBool)
    proposalDatumValid' =
      let params = proposalFromGovernor gov
       in phoistAcyclic $ proposalDatumValid params

    -- The address of the proposal validator.
    pproposalValidatorAddress :: Term s PAddress
    pproposalValidatorAddress =
      let vh = proposalValidatorHashFromGovernor gov
       in phoistAcyclic $ pconstant $ validatorHashToAddress vh

    -- The address of the stake validator.
    pstakeValidatorAddress :: Term s PAddress
    pstakeValidatorAddress =
      let vh = stakeValidatorHashFromGovernor gov
       in phoistAcyclic $ pconstant $ validatorHashToAddress vh

    -- The currency symbol of the stake state token.
    psstSymbol :: Term s PCurrencySymbol
    psstSymbol =
      let sym = stakeSTSymbolFromGovernor gov
       in phoistAcyclic $ pconstant sym

    -- The currency symbol of the governor state token.
    pgstSymbol :: Term s PCurrencySymbol
    pgstSymbol =
      let sym = governorSTSymbolFromGovernor gov
       in phoistAcyclic $ pconstant sym

--------------------------------------------------------------------------------

{- | Get the 'CurrencySymbol' of GST.

     @since 0.1.0
-}
governorSTSymbolFromGovernor :: Governor -> CurrencySymbol
governorSTSymbolFromGovernor gov = mintingPolicySymbol policy
  where
    policy :: MintingPolicy
    policy = mkMintingPolicy $ governorPolicy gov

{- | Get the 'AssetClass' of GST.

     @since 0.1.0
-}
governorSTAssetClassFromGovernor :: Governor -> AssetClass
governorSTAssetClassFromGovernor gov = AssetClass (symbol, "")
  where
    symbol :: CurrencySymbol
    symbol = governorSTSymbolFromGovernor gov

{- | Get the 'CurrencySymbol' of the proposal state token.

     @since 0.1.0
-}
proposalSTSymbolFromGovernor :: Governor -> CurrencySymbol
proposalSTSymbolFromGovernor gov = symbol
  where
    gstAC = governorSTAssetClassFromGovernor gov
    policy = mkMintingPolicy $ proposalPolicy gstAC
    symbol = mintingPolicySymbol policy

{- | Get the 'AssetClass' of the proposal state token.

     @since 0.1.0
-}
proposalSTAssetClassFromGovernor :: Governor -> AssetClass
proposalSTAssetClassFromGovernor gov = AssetClass (symbol, "")
  where
    symbol = proposalSTSymbolFromGovernor gov

{- | Get the 'CurrencySymbol' of the stake token/

     @since 0.1.0
-}
stakeSTSymbolFromGovernor :: Governor -> CurrencySymbol
stakeSTSymbolFromGovernor gov = mintingPolicySymbol policy
  where
    policy = mkMintingPolicy $ stakePolicy gov.gtClassRef

{- | Get the 'AssetClass' of the stake token.

   Note that the token is tagged with the hash of the stake validator.
   See 'Agora.Stake.Script.stakePolicy'.

    @since 0.1.0
-}
stakeSTAssetClassFromGovernor :: Governor -> AssetClass
stakeSTAssetClassFromGovernor gov = AssetClass (symbol, tokenName)
  where
    symbol = stakeSTSymbolFromGovernor gov

    -- Tag with the address where the token is being sent to.
    tokenName = validatorHashToTokenName $ stakeValidatorHashFromGovernor gov

{- | Get the 'Stake' parameter, given the 'Governor' parameter.

     @since 0.1.0
-}
stakeFromGovernor :: Governor -> Stake
stakeFromGovernor gov =
  Stake gov.gtClassRef $
    proposalSTAssetClassFromGovernor gov

{- | Get the hash of 'Agora.Stake.Script.stakePolicy'.

     @since 0.1.0
-}
stakeValidatorHashFromGovernor :: Governor -> ValidatorHash
stakeValidatorHashFromGovernor gov = validatorHash validator
  where
    params = stakeFromGovernor gov
    validator = mkValidator $ stakeValidator params

{- | Get the 'Proposal' parameter, given the 'Governor' parameter.

     @since 0.1.0
-}
proposalFromGovernor :: Governor -> Proposal
proposalFromGovernor gov = Proposal gstAC sstAC mc
  where
    gstAC = governorSTAssetClassFromGovernor gov
    mc = gov.maximumCosigners
    sstAC = stakeSTAssetClassFromGovernor gov

{- | Get the hash of 'Agora.Proposal.proposalPolicy'.

     @since 0.1.0
-}
proposalValidatorHashFromGovernor :: Governor -> ValidatorHash
proposalValidatorHashFromGovernor gov = validatorHash validator
  where
    params = proposalFromGovernor gov
    validator = mkValidator $ proposalValidator params

{- | Get the hash of 'Agora.Proposal.proposalValidator'.

     @since 0.1.0
-}
governorValidatorHash :: Governor -> ValidatorHash
governorValidatorHash gov = validatorHash validator
  where
    validator = mkValidator $ governorValidator gov

{- | Get the 'AuthorityToken' parameter given the 'Governor' parameter.

     @since 0.1.0
-}
authorityTokenFromGovernor :: Governor -> AuthorityToken
authorityTokenFromGovernor gov = AuthorityToken $ governorSTAssetClassFromGovernor gov

{- | Get the 'CurrencySymbol' of the authority token.

     @since 0.1.0
-}
authorityTokenSymbolFromGovernor :: Governor -> CurrencySymbol
authorityTokenSymbolFromGovernor gov = mintingPolicySymbol policy
  where
    policy = mkMintingPolicy $ authorityTokenPolicy params
    params = authorityTokenFromGovernor gov
