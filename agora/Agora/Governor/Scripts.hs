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
  atSymbolFromGovernor,
  proposalSTAssetClassFromGovernor,
  stakeSTSymbolFromGovernor,
  stakeFromGovernor,
  stakeValidatorHashFromGovernor,
  proposalFromGovernor,
  proposalValidatorHashFromGovernor,
) where

--------------------------------------------------------------------------------

import Data.Coerce (coerce)

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
  pgetNextProposalId,
 )
import Agora.Proposal (
  PProposalDatum (..),
  PProposalId (..),
  PProposalStatus (PFinished),
  PResultTag,
  Proposal (..),
  ProposalStatus (Draft, Locked),
  proposalDatumValid,
 )
import Agora.Proposal.Scripts (
  proposalPolicy,
  proposalValidator,
 )
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
  passert,
  pfindDatum,
  pfindTxInByTxOutRef,
  pisDJust,
  pisJust,
  pisUTXOSpent,
  psymbolValueOf,
  ptxSignedBy,
  pvalueSpent,
  scriptHashFromAddress,
  validatorHashToAddress,
 )

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
import Plutarch.Api.V1.Extra (
  passetClass,
  passetClassValueOf,
 )
import Plutarch.Builtin (pforgetData)
import Plutarch.Map.Extra (
  pkeys,
  plookup,
  plookup',
 )
import Plutarch.Monadic qualified as P
import Plutarch.SafeMoney (
  PDiscrete,
  puntag,
  pvalueDiscrete',
 )
import Plutarch.TryFrom (ptryFrom)

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Api (
  CurrencySymbol (..),
  MintingPolicy,
  TokenName (..),
 )
import Plutus.V1.Ledger.Scripts (ValidatorHash (..))
import Plutus.V1.Ledger.Value (
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

  NOTE: It's user's responsibility to make sure the token is sent to the corresponding governor validator.
        We /can't/ really check this in the policy, otherwise we create a cyclic reference issue.
-}
governorPolicy :: Governor -> ClosedTerm PMintingPolicy
governorPolicy gov =
  plam $ \_ ctx' -> P.do
    let oref = pconstant gov.gstOutRef

    PMinting ((pfield @"_0" #) -> ownSymbol) <- pmatch (pfromData $ pfield @"purpose" # ctx')
    let ownAssetClass = passetClass # ownSymbol # pconstant ""
        txInfo = pfromData $ pfield @"txInfo" # ctx'

    txInfoF <- pletFields @'["mint", "inputs"] txInfo

    passert "Referenced utxo should be spent" $
      pisUTXOSpent # oref # txInfoF.inputs

    passert "Exactly one token should be minted" $
      psymbolValueOf # ownSymbol # txInfoF.mint #== 1
        #&& passetClassValueOf # txInfoF.mint # ownAssetClass #== 1

    popaque (pconstant ())

{- | Validator for Governors.

  == Common checks

  The validator always ensures:

    - The UTXO which holds the GST must be spent.
    - The GST always stays at the validator's address.
    - The new state UTXO has a valid datum of type 'GovernorDatum'.

  == Creating a Proposal

  When the redeemer is 'CreateProposal', the script will check:

  - For governor's state datum:

      * 'nextProposalId' is advanced.
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

  When the redeemer is 'MintGATs', the script will check:

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

  Redeemer 'MutateGovernor' allows the state datum to be changed by an external effect.

  In this case, the script will check

  - Exactly one GAT is burnt in the transaction.
  - Said GAT is tagged by the effect.
-}
governorValidator :: Governor -> ClosedTerm PValidator
governorValidator gov =
  plam $ \datum' redeemer' ctx' -> P.do
    (pfromData -> redeemer, _) <- ptryFrom redeemer'
    ctx <- pletFields @'["txInfo", "purpose"] ctx'

    txInfo' <- plet $ pfromData $ ctx.txInfo
    txInfo <- pletFields @'["mint", "inputs", "outputs", "datums", "signatories"] txInfo'

    valueSpent <- plet $ pvalueSpent # txInfo.inputs

    PSpending ((pfield @"_0" #) -> txOutRef') <- pmatch $ pfromData ctx.purpose
    let txOutRef = pfromData txOutRef'

    PJust ((pfield @"resolved" #) -> ownInput') <- pmatch $ pfindTxInByTxOutRef # txOutRef # txInfo.inputs
    ownInput <- pletFields @'["address", "value"] ownInput'
    let selfAddress = pfromData $ ownInput.address

    (pfromData -> (oldParams' :: Term _ PGovernorDatum), _) <- ptryFrom datum'
    oldParams <- pletFields @'["proposalThresholds", "nextProposalId"] oldParams'

    let ownInputGSTAmount = psymbolValueOf # pgstSymbol # ownInput.value
    passert "Own input should have exactly one state token" $
      ownInputGSTAmount #== 1

    ownOutputs <- plet $ findOutputsToAddress # txInfo.outputs # selfAddress
    passert "Exactly one utxo should be sent to the governor" $
      plength # ownOutputs #== 1

    ownOutput <- pletFields @'["value", "datumHash"] $ phead # ownOutputs
    let ownOuputGSTAmount = psymbolValueOf # pgstSymbol # ownOutput.value
    passert "State token should stay at governor's address" $
      ownOuputGSTAmount #== 1
    passert "Output utxo to governor should have datum" $
      pisDJust # ownOutput.datumHash

    let outputGovernorStateDatumHash = mustBePDJust # "Output governor state datum hash not found" # ownOutput.datumHash

    newDatumData <-
      plet $
        pforgetData $
          pdata $
            mustBePJust # "Ouput governor state datum not found"
              #$ pfindDatum # outputGovernorStateDatumHash # txInfo.datums

    pmatch redeemer $ \case
      PCreateProposal _ -> P.do
        let expectedNextProposalId = pgetNextProposalId # oldParams.nextProposalId
            expectedNewDatum =
              pcon $
                PGovernorDatum $
                  pdcons @"proposalThresholds" # oldParams.proposalThresholds
                    #$ pdcons @"nextProposalId" # pdata expectedNextProposalId # pdnil

        passert "Unexpected governor state datum" $
          newDatumData #== pforgetData (pdata expectedNewDatum)

        passert "Exactly one proposal token must be minted" $
          hasOnlyOneTokenOfCurrencySymbol # ppstSymbol # txInfo.mint

        --

        inputsFromStakeValidatorWithStateToken <-
          plet $
            pfilter
              # phoistAcyclic
                ( plam $
                    \((pfield @"resolved" #) -> txOut') -> P.do
                      txOut <- pletFields @'["address", "value"] txOut'

                      txOut.address #== pdata pstakeValidatorAddress
                        #&& psymbolValueOf # psstSymbol # txOut.value #== 1
                )
              # pfromData txInfo.inputs

        passert "Exactly one input from the stake validator" $
          plength # inputsFromStakeValidatorWithStateToken #== 1

        stakeInputDatumHash <-
          plet $
            pfield @"datumHash"
              #$ pfield @"resolved"
              #$ phead # inputsFromStakeValidatorWithStateToken

        passert "Stake input must have datum" $
          pisDJust # stakeInputDatumHash

        let stakeInputDatum' = mustFindDatum' @PStakeDatum # stakeInputDatumHash # txInfo.datums

        stakeInputDatum <-
          pletFields @["stakedAmount", "owner", "lockedBy"] stakeInputDatum'

        passert "Required amount of stake GT should be spent" $
          stakeInputDatum.stakedAmount #< (pgtValueOf # valueSpent)

        passert "Tx should be signed by the stake owner" $
          ptxSignedBy # txInfo.signatories # stakeInputDatum.owner

        --

        outputsToProposalValidatorWithStateToken <-
          plet $
            pfilter
              # phoistAcyclic
                ( plam $
                    \txOut' -> P.do
                      txOut <- pletFields @'["address", "value"] txOut'

                      txOut.address #== pdata pproposalValidatorAddress
                        #&& psymbolValueOf # ppstSymbol # txOut.value #== 1
                )
              # pfromData txInfo.outputs

        passert "Exactly one UTXO with proposal state token should be sent to the proposal validator" $
          plength # outputsToProposalValidatorWithStateToken #== 1

        outputDatumHash <- plet $ pfield @"datumHash" #$ phead # outputsToProposalValidatorWithStateToken

        passert "The utxo paid to the proposal validator must have datum" $
          pisDJust # outputDatumHash

        outputProposalDatum' <-
          plet $
            mustFindDatum' @PProposalDatum
              # outputDatumHash
              # txInfo.datums

        passert "Proposal datum must be valid" $
          proposalDatumValid' # outputProposalDatum'

        outputProposalDatum <-
          pletFields
            @'["proposalId", "status", "cosigners", "thresholds", "votes"]
            outputProposalDatum'

        passert "Invalid proposal id in proposal datum" $
          outputProposalDatum.proposalId #== oldParams.nextProposalId

        passert "Invalid thresholds in proposal datum" $
          outputProposalDatum.thresholds #== oldParams.proposalThresholds

        passert "Initial proposal votes should be empty" $
          pnull #$ pto $ pto $ pfromData outputProposalDatum.votes

        passert "Proposal state should be draft" $
          outputProposalDatum.status #== pconstantData Draft

        passert "Proposal should have only one cosigner" $
          plength # pfromData outputProposalDatum.cosigners #== 1

        let cosigner = phead # pfromData outputProposalDatum.cosigners

        passert "Cosigner should be the stake owner" $
          pdata stakeInputDatum.owner #== cosigner

        --

        outputToStakeValidatorWithStateToken <-
          plet $
            pfilter
              # phoistAcyclic
                ( plam $
                    \txOut' -> P.do
                      txOut <- pletFields @'["address", "value"] txOut'

                      txOut.address #== pdata pstakeValidatorAddress
                        #&& psymbolValueOf # psstSymbol # txOut.value #== 1
                )
              # pfromData txInfo.outputs

        passert "Exactly one UTXO with stake state token should be sent to the stake validator" $
          plength # outputToStakeValidatorWithStateToken #== 1

        let stakeOutputDatumHash' =
              pfield @"datumHash"
                #$ pfromData
                $ phead # outputToStakeValidatorWithStateToken

            stakeOutputDatumHash = mustBePDJust # "Stake output should have datum" # stakeOutputDatumHash'

            stakeOutputDatum =
              pforgetData $
                pdata $
                  mustBePJust # "Stake output not found" #$ pfindDatum # stakeOutputDatumHash # txInfo.datums

        let possibleVoteResults = pkeys #$ pto $ pfromData outputProposalDatum.votes

            mkProposalLock :: Term _ (PProposalId :--> PAsData PResultTag :--> PAsData PProposalLock)
            mkProposalLock =
              phoistAcyclic $
                plam
                  ( \pid rt' ->
                      let fields =
                            pdcons @"vote" # rt'
                              #$ pdcons @"proposalTag" # pdata pid # pdnil
                       in pdata $ pcon $ PProposalLock fields
                  )

            expectedProposalLocks =
              pconcat # stakeInputDatum.lockedBy
                #$ pmap # (mkProposalLock # outputProposalDatum.proposalId) # possibleVoteResults

            expectedOutputDatum =
              pforgetData $
                pdata $
                  pcon $
                    PStakeDatum $
                      pdcons @"stakedAmount" # pdata stakeInputDatum.stakedAmount
                        #$ pdcons @"owner" # pdata stakeInputDatum.owner
                        #$ pdcons @"lockedBy" # pdata expectedProposalLocks # pdnil

        passert "Unexpected stake output datum" $ expectedOutputDatum #== stakeOutputDatum

        popaque $ pconstant ()
      PMintGATs _ -> P.do
        passert "Governor state should not be changed" $ newDatumData #== datum'

        inputsWithProposalStateToken <-
          plet $
            pfilter
              # plam
                ( \((pfield @"value" #) . (pfield @"resolved" #) -> value) ->
                    psymbolValueOf # ppstSymbol # value #== 1
                )
              #$ pfromData txInfo.inputs

        outputsWithProposalStateToken <-
          plet $
            pfilter
              # plam
                ( \((pfield @"value" #) -> value) ->
                    psymbolValueOf # ppstSymbol # value #== 1
                )
              #$ pfromData txInfo.outputs

        passert "The governor can only process one proposal at a time" $
          plength # inputsWithProposalStateToken #== 1
            #&& (psymbolValueOf # ppstSymbol #$ pvalueSpent # txInfo.inputs) #== 1

        proposalInputTxOut <-
          pletFields @'["address", "value", "datumHash"] $
            pfield @"resolved" #$ phead # inputsWithProposalStateToken
        proposalOutputTxOut <-
          pletFields @'["datumHash", "address"] $
            phead # outputsWithProposalStateToken

        passert "Proposal state token must be sent back to the proposal validator" $
          proposalOutputTxOut.address #== pdata pproposalValidatorAddress

        inputProposalDatum' <-
          plet $
            mustFindDatum' @PProposalDatum
              # proposalInputTxOut.datumHash
              # txInfo.datums
        outputProposalDatum' <-
          plet $
            mustFindDatum' @PProposalDatum
              # proposalOutputTxOut.datumHash
              # txInfo.datums

        passert "Proposal datum must be valid" $
          proposalDatumValid' # inputProposalDatum'
            #&& proposalDatumValid' # outputProposalDatum'

        inputProposalDatum <-
          pletFields @'["proposalId", "effects", "status", "cosigners", "thresholds", "votes"]
            inputProposalDatum'

        passert "Proposal must be in locked(executable) state in order to execute effects" $
          inputProposalDatum.status #== pconstantData Locked

        let expectedOutputProposalDatum =
              pforgetData $
                pdata $
                  pcon $
                    PProposalDatum $
                      pdcons @"proposalId" # inputProposalDatum.proposalId
                        #$ pdcons @"effects" # inputProposalDatum.effects
                        #$ pdcons @"status" # pdata (pcon $ PFinished pdnil)
                        #$ pdcons @"cosigners" # inputProposalDatum.cosigners
                        #$ pdcons @"thresholds" # inputProposalDatum.thresholds
                        #$ pdcons @"votes" # inputProposalDatum.votes # pdnil

        passert "Unexpected output proposal datum" $
          pforgetData (pdata outputProposalDatum') #== expectedOutputProposalDatum

        -- TODO: anything else to check here?

        let highestVoteFolder =
              phoistAcyclic $
                plam
                  ( \pair last' ->
                      pif
                        (pisJust # last')
                        ( P.do
                            PJust last <- pmatch last'
                            let lastHighestVote = pfromData $ psndBuiltin # last
                                thisVote = pfromData $ psndBuiltin # pair
                            pif (lastHighestVote #< thisVote) (pcon $ PJust pair) last'
                        )
                        (pcon $ PJust pair)
                  )

            votesList = pto $ pto $ pfromData inputProposalDatum.votes

            winner' =
              pfoldr # highestVoteFolder # pcon PNothing # votesList

        winner <- plet $ mustBePJust # "Empty votes" # winner'

        let highestVote = pfromData $ psndBuiltin # winner
            minimumVotes = puntag $ pfromData $ pfield @"execute" # inputProposalDatum.thresholds

        passert "Higgest vote doesn't meet the minimum requirement" $ minimumVotes #<= highestVote

        let finalResultTag = pfromData $ pfstBuiltin # winner

        effectGroup <- plet $ plookup' # finalResultTag #$ inputProposalDatum.effects

        gatCount <- plet $ plength #$ pto $ pto effectGroup

        passert "Required amount of GATs should be minted" $
          psymbolValueOf # ppstSymbol # txInfo.mint #== gatCount

        outputsWithGAT <-
          plet $
            pfilter
              # phoistAcyclic
                ( plam
                    ( \((pfield @"value" #) -> value) ->
                        0 #< psymbolValueOf # patSymbol # value
                    )
                )
              # pfromData txInfo.outputs

        passert "Output GATs is more than minted GATs" $
          plength # outputsWithGAT #== gatCount

        let gatOutputValidator' :: Term s (PMap PValidatorHash PDatumHash :--> PAsData PTxOut :--> PUnit :--> PUnit)
            gatOutputValidator' =
              phoistAcyclic $
                plam
                  ( \effects (pfromData -> output') _ -> P.do
                      output <- pletFields @'["address", "datumHash"] $ output'

                      let scriptHash =
                            mustBePJust # "GAT receiver is not a script"
                              #$ scriptHashFromAddress # output.address
                          datumHash =
                            mustBePDJust # "Output to effect should have datum"
                              #$ output.datumHash

                          expectedDatumHash =
                            mustBePJust # "Receiver is not in the effect list"
                              #$ plookup # scriptHash # effects

                      passert "GAT must be tagged by the effect hash" $ authorityTokensValidIn # patSymbol # output'
                      passert "Unexpected datum" $ datumHash #== expectedDatumHash
                      pconstant ()
                  )

            gatOutputValidator = gatOutputValidator' # effectGroup

        popaque $
          pfoldr
            # gatOutputValidator
            # pconstant ()
            # outputsWithGAT
      PMutateGovernor _ -> P.do
        popaque $ singleAuthorityTokenBurned patSymbol ctx.txInfo txInfo.mint
  where
    pgtValueOf :: Term s (PValue :--> PDiscrete GTTag)
    pgtValueOf = phoistAcyclic $ pvalueDiscrete' gov.gtClassRef

    patSymbol :: Term s PCurrencySymbol
    patSymbol = phoistAcyclic $ pconstant $ atSymbolFromGovernor gov

    ppstSymbol :: Term s PCurrencySymbol
    ppstSymbol =
      let AssetClass (sym, _) = proposalSTAssetClassFromGovernor gov
       in phoistAcyclic $ pconstant sym

    proposalDatumValid' :: Term s (PProposalDatum :--> PBool)
    proposalDatumValid' =
      let params = proposalFromGovernor gov
       in phoistAcyclic $ proposalDatumValid params

    pproposalValidatorAddress :: Term s PAddress
    pproposalValidatorAddress =
      let vh = proposalValidatorHashFromGovernor gov
       in phoistAcyclic $ pconstant $ validatorHashToAddress vh

    pstakeValidatorAddress :: Term s PAddress
    pstakeValidatorAddress =
      let vh = stakeValidatorHashFromGovernor gov
       in phoistAcyclic $ pconstant $ validatorHashToAddress vh

    psstSymbol :: Term s PCurrencySymbol
    psstSymbol =
      let sym = stakeSTSymbolFromGovernor gov
       in phoistAcyclic $ pconstant sym

    pgstSymbol :: Term s PCurrencySymbol
    pgstSymbol =
      let sym = governorSTSymbolFromGovernor gov
       in phoistAcyclic $ pconstant sym

--------------------------------------------------------------------------------

governorSTSymbolFromGovernor :: Governor -> CurrencySymbol
governorSTSymbolFromGovernor gov = mintingPolicySymbol policy
  where
    policy :: MintingPolicy
    policy = mkMintingPolicy $ governorPolicy gov

{- | Get the 'AssetClass' of GST from 'Governor'.
 TODO: tag GST?
-}
governorSTAssetClassFromGovernor :: Governor -> AssetClass
governorSTAssetClassFromGovernor gov = AssetClass (symbol, "")
  where
    symbol :: CurrencySymbol
    symbol = governorSTSymbolFromGovernor gov

-- | Get the `CurrencySymbol` of GAT from 'Governor'.
atSymbolFromGovernor :: Governor -> CurrencySymbol
atSymbolFromGovernor gov = mintingPolicySymbol policy
  where
    at = AuthorityToken $ governorSTAssetClassFromGovernor gov
    policy = mkMintingPolicy $ authorityTokenPolicy at

proposalSTAssetClassFromGovernor :: Governor -> AssetClass
proposalSTAssetClassFromGovernor gov = AssetClass (symbol, "")
  where
    gstAC = governorSTAssetClassFromGovernor gov
    -- JUSTIFICATIONL: the PST policy doesn't care about the following two fields at all.
    -- FIXME: refactor PST policy, parameterize it only with GST assetclass or something.
    sstAC = AssetClass ("", "")
    mc = -1
    params = Proposal gstAC sstAC mc

    policy = mkMintingPolicy $ proposalPolicy params
    symbol = mintingPolicySymbol policy

stakeSTSymbolFromGovernor :: Governor -> CurrencySymbol
stakeSTSymbolFromGovernor gov = mintingPolicySymbol policy
  where
    policy = mkMintingPolicy $ stakePolicy gov.gtClassRef

stakeFromGovernor :: Governor -> Stake
stakeFromGovernor gov =
  Stake gov.gtClassRef $
    proposalSTAssetClassFromGovernor gov

stakeValidatorHashFromGovernor :: Governor -> ValidatorHash
stakeValidatorHashFromGovernor gov = validatorHash validator
  where
    params = stakeFromGovernor gov
    validator = mkValidator $ stakeValidator params

proposalFromGovernor :: Governor -> Proposal
proposalFromGovernor gov = Proposal gstAC sstAC mc
  where
    gstAC = governorSTAssetClassFromGovernor gov
    mc = gov.maximumCosigners

    sstS = stakeSTSymbolFromGovernor gov
    -- The stake state token is tagged with the address which it's sent to.
    sstTN :: TokenName
    sstTN = coerce $ stakeValidatorHashFromGovernor gov
    sstAC = AssetClass (sstS, sstTN)

proposalValidatorHashFromGovernor :: Governor -> ValidatorHash
proposalValidatorHashFromGovernor gov = validatorHash validator
  where
    params = proposalFromGovernor gov
    validator = mkValidator $ proposalValidator params
