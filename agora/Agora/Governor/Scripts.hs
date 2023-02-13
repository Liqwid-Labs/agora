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
  ProposalStatus (Draft, Locked),
  phasNeutralEffect,
  pisEffectsVotesCompatible,
  pisVotesEmpty,
  pneutralOption,
  pwinner,
 )
import Agora.Proposal.Time (pvalidateProposalStartingTime)
import Agora.SafeMoney (AuthorityTokenTag, GovernorSTTag, ProposalSTTag, StakeSTTag)
import Agora.Stake (
  pnumCreatedProposals,
  presolveStakeInputDatum,
 )
import Agora.Utils (phashDatum, ptaggedSymbolValueOf, ptoScottEncodingT, puntag)
import Data.Function (on)
import Plutarch.Api.V1 (PCurrencySymbol)
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V2 (PDatum, PMintingPolicy, PScriptHash, PScriptPurpose (PMinting, PSpending), PTxOut, PTxOutRef, PValidator)
import Plutarch.Api.V2.Tx (POutputDatum (..))
import Plutarch.Extra.AssetClass (PAssetClassData, passetClass)
import Plutarch.Extra.Field (pletAll, pletAllC)
import Plutarch.Extra.Maybe (passertPJust, pfromJust, pjust, pmaybeData, pnothing)
import Plutarch.Extra.Ord (POrdering (..), pcompareBy, pfromOrd, psort)
import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import Plutarch.Extra.ScriptContext (
  pfindTxInByTxOutRef,
  pisUTXOSpent,
  pscriptHashFromAddress,
  pscriptHashToTokenName,
  ptryFromOutputDatum,
  pvalueSpent,
 )
import Plutarch.Extra.Tagged (PTagged)
import Plutarch.Extra.Value (passetClassValueOf, psymbolValueOf)
import "liqwid-plutarch-extra" Plutarch.Extra.List (pfindJust, plistEqualsBy, pmapMaybe)
import "liqwid-plutarch-extra" Plutarch.Extra.Map (pkeys, ptryLookup)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pguardC,
  pletC,
  pletFieldsC,
  pmatchC,
  ptryFromC,
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

  @since 1.0.0
-}
governorPolicy :: ClosedTerm (PTxOutRef :--> PMintingPolicy)
governorPolicy =
  plam $ \initialSpend _ ctx -> unTermCont $ do
    PMinting ((pfield @"_0" #) -> gstSymbol) <-
      pmatchC (pfromData $ pfield @"purpose" # ctx)

    let txInfo = pfromData $ pfield @"txInfo" # ctx

    txInfoF <-
      pletFieldsC
        @'[ "mint"
          , "inputs"
          , "outputs"
          , "datums"
          , "validRange"
          ]
        txInfo

    pguardC "Referenced utxo should be spent" $
      pisUTXOSpent # initialSpend # txInfoF.inputs

    pguardC "Exactly one token should be minted" $
      let vMap = pfromData $ pto txInfoF.mint
          tnMap =
            passertPJust
              # "GST symbol entry"
              #$ plookup
              # gstSymbol
              # vMap
       in tnMap #== AssocMap.psingleton # pconstant "" # 1

    let governorOutputDatum =
          passertPJust
            # "Governor output should present"
            #$ pfindJust
            # plam
              ( flip (pletFields @'["value", "datum"]) $ \txOutF ->
                  let isGovernorUTxO =
                        psymbolValueOf
                          # gstSymbol
                          # txOutF.value
                          #== 1

                      governorDatum =
                        ptrace "Resolve governor datum" $
                          ptryFromOutputDatum @PGovernorDatum
                            # txOutF.datum
                            # txInfoF.datums
                   in pif isGovernorUTxO (pjust # governorDatum) pnothing
              )
            # pfromData txInfoF.outputs

    pguardC "Governor output datum valid" $
      pisGovernorDatumValid # governorOutputDatum

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

     == Arguments

     Following arguments should be provided(in this order):
     1. proposal validator address
     2. state ST symbol
     3. governor ST symbol
     4. proposal ST symbol
     5. authority token symbol.

     @since 1.0.0
-}
governorValidator ::
  -- | Lazy precompiled scripts.
  ClosedTerm
    ( PScriptHash
        :--> PTagged StakeSTTag PAssetClassData
        :--> PTagged GovernorSTTag PCurrencySymbol
        :--> PTagged ProposalSTTag PCurrencySymbol
        :--> PTagged AuthorityTokenTag PCurrencySymbol
        :--> PValidator
    )
governorValidator =
  plam $ \proposalScriptHash sstClass gstSymbol pstSymbol atSymbol datum redeemer ctx -> unTermCont $ do
    ctxF <- pletAllC ctx
    txInfo <- pletC $ pfromData ctxF.txInfo
    txInfoF <-
      pletFieldsC
        @'[ "mint"
          , "inputs"
          , "outputs"
          , "datums"
          , "signatories"
          , "validRange"
          ]
        txInfo

    ----------------------------------------------------------------------------

    governorInputDatum <- fst <$> ptryFromC @PGovernorDatum datum
    governorInputDatumF <- pletAllC governorInputDatum

    PSpending ((pfield @"_0" #) -> governorInputRef) <-
      pmatchC $ pfromData ctxF.purpose

    let governorInput =
          pfield @"resolved"
            #$ passertPJust
            # "Malformed script context: own input not found"
            #$ pfindTxInByTxOutRef
            # governorInputRef
            # txInfoF.inputs

    governorInputF <- pletFieldsC @'["address", "value"] governorInput

    ----------------------------------------------------------------------------

    governorOutputDatum <-
      pletC $
        passertPJust
          # "Own output should present"
          #$ pfindJust
          # plam
            ( flip pletAll $ \outputF ->
                let isGovernorUTxO =
                      foldl1
                        (#&&)
                        [ ptraceIfFalse "Own by governor validator" $
                            ((#==) `on` (pscriptHashFromAddress #))
                              outputF.address
                              governorInputF.address
                        , ptraceIfFalse "Has governor ST" $
                            ptaggedSymbolValueOf # gstSymbol # outputF.value #== 1
                        ]

                    datum =
                      ptrace "Resolve governor datum" $
                        ptryFromOutputDatum @PGovernorDatum
                          # outputF.datum
                          # txInfoF.datums
                 in pif
                      isGovernorUTxO
                      (pjust # datum)
                      pnothing
            )
          # pfromData txInfoF.outputs

    ----------------------------------------------------------------------------

    pstClass <- pletC $ passetClass # pto pstSymbol # pconstant ""

    getProposalDatum :: Term _ (PTxOut :--> PMaybe PProposalDatum) <-
      pletC $
        plam $
          flip (pletFields @'["value", "datum", "address"]) $ \txOutF ->
            let isProposalUTxO =
                  (pfromJust #$ pscriptHashFromAddress # pfromData txOutF.address)
                    #== proposalScriptHash
                    #&& passetClassValueOf
                    # pstClass
                    # txOutF.value
                    #== 1

                proposalDatum =
                  ptrace "Resolve proposal output datum" $
                    pfromData $
                      ptryFromOutputDatum
                        # txOutF.datum
                        # txInfoF.datums
             in pif isProposalUTxO (pjust # proposalDatum) pnothing

    ----------------------------------------------------------------------------

    governorRedeemer <- pfromData . fst <$> ptryFromC redeemer

    pure $
      pmatch governorRedeemer $ \case
        PCreateProposal -> unTermCont $ do
          -- Check that the transaction advances proposal id.

          let expectedNextProposalId =
                pgetNextProposalId
                  # governorInputDatumF.nextProposalId
              expectedNewDatum =
                mkRecordConstr
                  PGovernorDatum
                  ( #proposalThresholds
                      .= governorInputDatumF.proposalThresholds
                      .& #nextProposalId
                      .= pdata expectedNextProposalId
                      .& #proposalTimings
                      .= governorInputDatumF.proposalTimings
                      .& #createProposalTimeRangeMaxWidth
                      .= governorInputDatumF.createProposalTimeRangeMaxWidth
                      .& #maximumCreatedProposalsPerStake
                      .= governorInputDatumF.maximumCreatedProposalsPerStake
                  )

          pguardC "Only next proposal id gets advanced" $
            governorOutputDatum #== expectedNewDatum

          -- Check that exactly one proposal token is being minted.

          pguardC "Exactly one proposal token must be minted" $
            passetClassValueOf # pstClass # txInfoF.mint #== 1

          -- Check that a stake is spent to create the propsal,
          --   and the value it contains meets the requirement.

          let stakeInputDatum =
                passertPJust
                  # "Stake input should present"
                  #$ pfindJust
                  # ( presolveStakeInputDatum
                        # (ptoScottEncodingT # sstClass)
                        # txInfoF.datums
                    )
                  # pfromData txInfoF.inputs

          stakeInputDatumF <- pletAllC stakeInputDatum

          pguardC "Proposals created by the stake must not exceed the limit" $
            pnumCreatedProposals
              # stakeInputDatumF.lockedBy
              #< governorInputDatumF.maximumCreatedProposalsPerStake

          let gtThreshold =
                pfromData $
                  pfield @"create"
                    # governorInputDatumF.proposalThresholds

          pguardC "Require minimum amount of GTs" $
            gtThreshold #<= stakeInputDatumF.stakedAmount

          -- Check that the newly minted PST is sent to the proposal validator,
          --   and the datum it carries is legal.

          let proposalOutputDatum =
                passertPJust
                  # "Proposal output should present"
                  #$ pfindJust
                  # getProposalDatum
                  # pfromData txInfoF.outputs

          proposalOutputDatumF <- pletAllC proposalOutputDatum

          let expectedCosigners = psingleton @PBuiltinList # stakeInputDatumF.owner

          pguardC "Proposal datum correct" $
            foldl1
              (#&&)
              [ ptraceIfFalse "has neutral effect" $
                  phasNeutralEffect # proposalOutputDatumF.effects
              , ptraceIfFalse "votes have valid shape" $
                  pisEffectsVotesCompatible # proposalOutputDatumF.effects # proposalOutputDatumF.votes
              , ptraceIfFalse "votes are empty" $
                  pisVotesEmpty # proposalOutputDatumF.votes
              , ptraceIfFalse "id correct" $
                  proposalOutputDatumF.proposalId #== governorInputDatumF.nextProposalId
              , ptraceIfFalse "status is Draft" $
                  proposalOutputDatumF.status #== pconstantData Draft
              , ptraceIfFalse "cosigners correct" $
                  plistEquals # pfromData proposalOutputDatumF.cosigners # expectedCosigners
              , ptraceIfFalse "starting time valid" $
                  pvalidateProposalStartingTime
                    # governorInputDatumF.createProposalTimeRangeMaxWidth
                    # txInfoF.validRange
                    # proposalOutputDatumF.startingTime
              , ptraceIfFalse "copy over configurations" $
                  proposalOutputDatumF.thresholds
                    #== governorInputDatumF.proposalThresholds
                    #&& proposalOutputDatumF.timingConfig
                    #== governorInputDatumF.proposalTimings
              ]

          pure $ popaque $ pconstant ()

        ------------------------------------------------------------------------

        PMintGATs -> unTermCont $ do
          pguardC "Governor state should not be changed" $ governorOutputDatum #== governorInputDatum

          -- Filter out proposal inputs and ouputs using PST and the address of proposal validator.

          pguardC "The governor can only process one proposal at a time" $
            (ptaggedSymbolValueOf # pstSymbol #$ pvalueSpent # txInfoF.inputs) #== 1

          let proposalInputDatum =
                passertPJust
                  # "Proposal input not found"
                  #$ pfindJust
                  # plam ((getProposalDatum #) . (pfield @"resolved" #))
                  # pfromData txInfoF.inputs

          proposalInputDatumF <-
            pletFieldsC @'["effects", "status", "thresholds", "votes"]
              proposalInputDatum

          -- Check that the proposal state is advanced so that a proposal cannot be executed twice.

          pguardC "Proposal must be in locked(executable) state in order to execute effects" $
            proposalInputDatumF.status #== pconstantData Locked

          -- Find the highest votes and the corresponding tag.
          let quorum = pto $ pfromData $ pfield @"execute" # proposalInputDatumF.thresholds
              neutralOption = pneutralOption # proposalInputDatumF.effects
              finalResultTag = pwinner # proposalInputDatumF.votes # quorum # neutralOption

          -- The effects of the winner outcome.
          effectGroup <- pletC $ ptryLookup # finalResultTag #$ proposalInputDatumF.effects

          let
            -- For a given output, check if it contains a single valid GAT.
            getReceiverScriptHash =
              plam
                ( \output -> unTermCont $ do
                    outputF <- pletFieldsC @'["address", "datum", "value"] output

                    let atAmount =
                          ptaggedSymbolValueOf
                            # atSymbol
                            # outputF.value

                        handleAuthorityUTxO =
                          do
                            receiverScriptHash <-
                              pletC $
                                passertPJust
                                  # "GAT receiver should be a script"
                                  #$ pscriptHashFromAddress
                                  # outputF.address

                            effect <-
                              pletAllC $
                                passertPJust
                                  # "Receiver should be in the effect group"
                                  #$ AssocMap.plookup
                                  # receiverScriptHash
                                  # effectGroup

                            let tagToken =
                                  pmaybeData
                                    # pconstant ""
                                    # plam (pscriptHashToTokenName . pfromData)
                                    # effect.scriptHash
                                gatAssetClass = passetClass # puntag atSymbol # tagToken
                                valueGATCorrect =
                                  passetClassValueOf
                                    # gatAssetClass
                                    # outputF.value
                                    #== 1

                            let outputDatumHash = pmatch outputF.datum $ \case
                                  POutputDatum d -> phashDatum #$ pfield @"outputDatum" @PDatum # d
                                  POutputDatumHash h -> pfield @"datumHash" # h
                                  _ -> ptraceError "expcted effect datum, got nothing"

                                hasCorrectDatum =
                                  effect.datumHash #== outputDatumHash

                            pguardC "Authority output valid" $
                              foldr1
                                (#&&)
                                [ ptraceIfFalse "GAT valid" $ authorityTokensValidIn # atSymbol # output
                                , ptraceIfFalse "Correct datum" hasCorrectDatum
                                , ptraceIfFalse "Value correctly encodes Auth Check script" valueGATCorrect
                                ]

                            pure $ pjust # receiverScriptHash

                    pmatchC
                      ( pcompareBy
                          # pfromOrd
                          # atAmount
                          # 1
                      )
                      >>= \case
                        -- atAmount == 1
                        PEQ -> handleAuthorityUTxO
                        -- atAmount < 1
                        PLT -> pure pnothing
                        -- atAmount > 1
                        PGT -> pure $ ptraceError "More than one GAT in one UTxO"
                )

            -- The sorted hashes of all the GAT receivers.
            actualReceivers =
              psort
                #$ pmapMaybe @PList
                # getReceiverScriptHash
                # pfromData txInfoF.outputs

            expectedReceivers = pkeys @PList # effectGroup

          -- This check ensures that it's impossible to send more than one GATs
          -- to a validator in the winning effect group.
          pguardC "Each script in the effect group gets a GAT" $
            plistEqualsBy
              # plam (\(pfromData -> x) y -> x #== y)
              # expectedReceivers
              # actualReceivers

          pure $ popaque $ pconstant ()

        ------------------------------------------------------------------------

        PMutateGovernor -> unTermCont $ do
          pguardC "Governor output datum is valid" $
            pisGovernorDatumValid # governorOutputDatum

          -- Check that a GAT is burnt.
          pguardC "One valid GAT burnt" $
            singleAuthorityTokenBurned atSymbol txInfoF.inputs txInfoF.mint

          pure $ popaque $ pconstant ()
