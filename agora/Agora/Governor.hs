{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Agora.Governor
Maintainer : chfanghr@gmail.com
Description: Governor entity scripts acting as authority of entire system.

Governor entity scripts acting as authority of entire system.
-}
module Agora.Governor (
  -- * GST
  -- $gst

  -- * Haskell-land
  GovernorDatum (..),
  GovernorRedeemer (..),
  Governor (..),

  -- * Plutarch-land
  PGovernorDatum (..),
  PGovernorRedeemer (..),

  -- * Scripts
  governorPolicy,
  governorValidator,

  -- * Utilities
  gstAssetClass,
  gatSymbol,
) where

--------------------------------------------------------------------------------

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

--------------------------------------------------------------------------------

import Agora.AuthorityToken (
  AuthorityToken (..),
  authorityTokenPolicy,
  authorityTokensValidIn,
  singleAuthorityTokenBurned,
 )
import Agora.Proposal (
  PProposalDatum,
  PProposalId,
  PProposalStatus (PDraft, PExecutable, PFinished),
  PProposalThresholds,
  Proposal (..),
  ProposalId,
  ProposalThresholds,
  pnextProposalId,
  proposalDatumValid,
  proposalPolicy,
  proposalValidator,
 )
import Agora.Utils (
  findOutputsToAddress,
  hasOnlyOneTokenOfCurrencySymbol,
  mustBePDJust,
  mustBePJust,
  mustFindDatum',
  passert,
  passetClassValueOf,
  passetClassValueOf',
  pfindTxInByTxOutRef,
  pisDJust,
  pisJust,
  pisUxtoSpent,
  pownCurrencySymbol,
  psymbolValueOf,
  pvalueSpent,
  scriptHashFromAddress,
 )

--------------------------------------------------------------------------------

import Plutarch (popaque)
import Plutarch.Api.V1 (
  PAddress,
  PCurrencySymbol,
  PDatumHash,
  PMap,
  PMintingPolicy,
  PScriptPurpose (PSpending),
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
  pownMintValue,
 )
import Plutarch.Builtin (pforgetData)
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Lift (PUnsafeLiftDecl (..))
import Plutarch.Map.Extra (plookup, plookup')
import Plutarch.Monadic qualified as P
import Plutarch.SafeMoney (puntag)
import Plutarch.Unsafe (punsafeCoerce)

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Api (
  Address (Address),
  Credential (ScriptCredential),
  CurrencySymbol (..),
  MintingPolicy,
  TxOutRef,
 )
import Plutus.V1.Ledger.Value (
  AssetClass (..),
  TokenName (..),
 )
import PlutusTx qualified

--------------------------------------------------------------------------------

{- $gst
   Governance state token, aka. GST, it's a NFT that identifies an UTXO that carries the state datum of the Governance script.

   This token is minted by a one-shot monetary policy 'governorPolicy', meaning that the token has guaranteed uniqueness.

   The 'governorValidator' ensures that exactly one GST stays at the address of itself forever.
-}

--------------------------------------------------------------------------------

-- | State datum for the Governor script.
data GovernorDatum = GovernorDatum
  { proposalThresholds :: ProposalThresholds
  -- ^ Gets copied over upon creation of a 'Agora.Proposal.ProposalDatum'.
  , nextProposalId :: ProposalId
  -- ^ What tag the next proposal will get upon creating.
  }
  deriving stock (Show, GHC.Generic)

PlutusTx.makeIsDataIndexed ''GovernorDatum [('GovernorDatum, 0)]

{- | Redeemer for Governor script. The governor has two primary
     responsibilities:

     1. The gating of Proposal creation.
     2. The gating of minting authority tokens.

     Parameters of the governor can also be mutated by an effect.
-}
data GovernorRedeemer
  = -- | Checks that a proposal was created lawfully, and allows it.
    CreateProposal
  | -- | Checks that a SINGLE proposal finished correctly,
    --   and allows minting GATs for each effect script.
    MintGATs
  | -- | Allows effects to mutate the parameters.
    MutateGovernor
  deriving stock (Show, GHC.Generic)

PlutusTx.makeIsDataIndexed
  ''GovernorRedeemer
  [ ('CreateProposal, 0)
  , ('MintGATs, 1)
  , ('MutateGovernor, 2)
  ]

-- | Parameters for creating Governor scripts.
data Governor = Governor
  { gstORef :: TxOutRef
  -- ^ Referenced utxo will be spent to mint the GST.
  , gstName :: TokenName
  -- ^ Name of the GST token.
  }

--------------------------------------------------------------------------------

-- | Plutarch-level datum for the Governor script.
newtype PGovernorDatum (s :: S) = PGovernorDatum
  { getGovernorDatum ::
    Term
      s
      ( PDataRecord
          '[ "proposalThresholds" ':= PProposalThresholds
           , "nextProposalId" ':= PProposalId
           ]
      )
  }
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PGovernorDatum

instance PUnsafeLiftDecl PGovernorDatum where type PLifted PGovernorDatum = GovernorDatum
deriving via (DerivePConstantViaData GovernorDatum PGovernorDatum) instance (PConstant GovernorDatum)

-- | Plutarch-level version of 'GovernorRedeemer'.
data PGovernorRedeemer (s :: S)
  = PCreateProposal (Term s (PDataRecord '[]))
  | PMintGATs (Term s (PDataRecord '[]))
  | PMutateGovernor (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PGovernorRedeemer

instance PUnsafeLiftDecl PGovernorRedeemer where type PLifted PGovernorRedeemer = GovernorRedeemer
deriving via (DerivePConstantViaData GovernorRedeemer PGovernorRedeemer) instance (PConstant GovernorRedeemer)

--------------------------------------------------------------------------------

{- | Policy for minting GSTs.

   This policy perform the following checks:

    - The UTXO referenced in the parameter is spent in the transaction.
    - Exactly one GST is minted.
    - Ensure the token name is 'gstName'.

  NOTE: It's user's responsibility to make sure the token is sent to the corresponding governor validator.
        We /can't/ really check this in the policy, otherwise we create a cyclic reference issue.
-}
governorPolicy :: Governor -> ClosedTerm PMintingPolicy
governorPolicy gov =
  plam $ \_ ctx' -> P.do
    let oref = pconstant gov.gstORef
        ownSymbol = pownCurrencySymbol # ctx'

    mintValue <- plet $ pownMintValue # ctx'

    passert "Referenced utxo should be spent" $
      pisUxtoSpent # oref #$ pfield @"txInfo" # ctx'

    passert "Exactly one token should be minted" $
      psymbolValueOf # ownSymbol # mintValue #== 1
        #&& passetClassValueOf # ownSymbol # pconstant gov.gstName # mintValue #== 1

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

- Exactly one new proposal state token is minted.
- Exactly one UTXO is sent to the proposal validator, this UTXO must:

    * Hold the newly minted proposal state token.
    * Have a valid datum of type 'Agora.Proposal.ProposalDatum', the datum must:

        - Copy its id and thresholds from the governor's state.
        - Have status set to 'Proposal.Draft'.
        - Have zero votes.
        - TODO: should we check cosigners?

== Minting GATs

When the redeemer is 'MintGATs', the script will check:

- Governor's state is not changed.
- Exactly only one proposal is in the inputs. Let's call this the /input proposal/.
- The proposal is in the 'Proposal.Executable' state.

NOTE: The input proposal is found by looking for the UTXO with a proposal state token in the inputs.

=== Effect Group Selection

Currently a proposal can two or more than two options to vote on,
  meaning that it can conatinas two or more effect groups,
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
    -- TODO: use ptryFrom
    redeemer <- pmatch $ pfromData @PGovernorRedeemer $ punsafeCoerce redeemer'
    ctx <- pletFields @'["txInfo", "purpose"] ctx'

    txInfo' <- plet $ pfromData $ ctx.txInfo
    txInfo <- pletFields @'["mint", "inputs", "outputs"] txInfo'

    PSpending ((pfield @"_0" #) -> txOutRef') <- pmatch $ pfromData ctx.purpose
    let txOutRef = pfromData txOutRef'

    PJust ((pfield @"resolved" #) -> ownInput') <- pmatch $ pfindTxInByTxOutRef # txOutRef # txInfo'
    ownInput <- pletFields @'["address", "value"] ownInput'
    let selfAddress = pfromData $ ownInput.address

    -- TODO: use ptryFrom
    let oldParams' = pfromData @PGovernorDatum $ punsafeCoerce datum'
    oldParams <- pletFields @'["proposalThresholds", "nextProposalId"] oldParams'

    let ownInputDatumNFTAmount = stateTokenValueOf # ownInput.value
    passert "Own input should have exactly one state token" $
      ownInputDatumNFTAmount #== 1

    ownOutputs <- plet $ findOutputsToAddress # txInfo' # selfAddress
    passert "Exactly one utxo should be sent to the governor" $
      plength # ownOutputs #== 1

    ownOutput <- pletFields @'["value", "datumHash"] $ phead # ownOutputs
    let ownOuputDatumNFTAmount = stateTokenValueOf # ownOutput.value
    passert "State token should stay at governor's address" $
      ownOuputDatumNFTAmount #== 1
    passert "Output utxo to governor should have datum" $
      pisDJust # ownOutput.datumHash

    -- TODO: use `PTryFrom` and reject bad datum
    newDatum' <- plet $ mustFindDatum' @PGovernorDatum # ownOutput.datumHash # ctx.txInfo
    newParams <- pletFields @'["proposalThresholds", "nextProposalId"] newDatum'

    case redeemer of
      PCreateProposal _ -> P.do
        passert "Proposal id should be advanced by 1" $
          pnextProposalId # oldParams.nextProposalId #== newParams.nextProposalId

        -- TODO: check other fields of the state datum

        passert "Exactly one proposal token must be minted" $
          hasOnlyOneTokenOfCurrencySymbol # pproposalSymbol # txInfo.mint

        outputs <- plet $ findOutputsToAddress # ctx.txInfo # pproposalValidatorAddress
        passert "Exactly one utxo should be sent to the proposal validator" $
          plength # outputs #== 1

        output <- pletFields @'["value", "datumHash"] $ phead # outputs
        passert "The proposal state token must be sent to the proposal validator" $
          psymbolValueOf # pproposalSymbol # output.value #== 1

        passert "The utxo paid to the proposal validator must have datum" $
          pisDJust # output.datumHash

        outputProposalDatum' <-
          plet $
            mustFindDatum' @PProposalDatum
              # output.datumHash
              # ctx.txInfo

        passert "Proposal datum must be valid" $
          proposalDatumValid # outputProposalDatum'

        proposalDatum <-
          pletFields
            @'["id", "status", "cosigners", "thresholds", "votes"]
            outputProposalDatum'

        passert "Invalid proposal id in proposal datum" $
          proposalDatum.id #== oldParams.nextProposalId

        passert "Invalid thresholds in proposal datum" $
          proposalDatum.thresholds #== oldParams.proposalThresholds

        passert "Initial proposal votes should be empty" $
          pnull #$ pto $ pto $ pfromData proposalDatum.votes

        -- TODO: should we check cosigners here?

        let isProposalDraft = pmatch (pfromData proposalDatum.status) $ \case
              PDraft _ -> pconstant True
              _ -> pconstant False

        passert "Proposal state should be draft" isProposalDraft

        popaque $ pconstant ()
      PMintGATs _ -> P.do
        passert "Governor state should not be changed" $
          -- FIXME: There should be a better way to do this
          (pforgetData $ pdata newDatum') #== datum'

        inputsWithProposalStateToken <-
          plet $
            pfilter
              # plam
                ( \((pfield @"value" #) . (pfield @"resolved" #) -> value) ->
                    psymbolValueOf # pproposalSymbol # value #== 1
                )
              #$ pfromData txInfo.inputs

        outputsWithProposalStateToken <-
          plet $
            pfilter
              # plam
                ( \((pfield @"value" #) -> value) ->
                    psymbolValueOf # pproposalSymbol # value #== 1
                )
              #$ pfromData txInfo.outputs

        passert "The governor can only process one proposal at a time" $
          plength # inputsWithProposalStateToken #== 1
            #&& (psymbolValueOf # pproposalSymbol #$ pvalueSpent # txInfo') #== 1

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
              # txInfo'
        outputProposalDatum' <-
          plet $
            mustFindDatum' @PProposalDatum
              # proposalOutputTxOut.datumHash
              # txInfo'

        passert "Proposal datum must be valid" $
          proposalDatumValid # inputProposalDatum'
            #&& proposalDatumValid # outputProposalDatum'

        inputProposalDatum <-
          pletFields @'["id", "effects", "status", "cosigners", "thresholds", "votes"]
            inputProposalDatum'

        let isProposalExecutable = pmatch (pfromData inputProposalDatum.status) $ \case
              PExecutable _ -> pconstant True
              _ -> pconstant False

        passert "Proposal must be in executable state in order to execute effects" isProposalExecutable

        -- TODO: not sure if I did the right thing, can't use haskell level constructor here
        let fields =
              pdcons @"id" # inputProposalDatum.id
                #$ pdcons @"effects" # inputProposalDatum.effects
                #$ pdcons @"status" # pdata (pcon $ PFinished pdnil)
                #$ pdcons @"cosigners" # inputProposalDatum.cosigners
                #$ pdcons @"thresholds" # inputProposalDatum.thresholds
                #$ pdcons @"votes" # inputProposalDatum.votes # pdnil

            expectedOutputDatum = pforgetData $ pdata fields

        passert "Unexpected output proposal datum" $
          pforgetData (pdata outputProposalDatum') #== expectedOutputDatum

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

            winner' =
              pfoldr # highestVoteFolder # (pcon $ PNothing) #$ pto $ pto $ pfromData inputProposalDatum.votes

        winner <- plet $ mustBePJust # "Empty votes" # winner'

        let highestVote = pfromData $ psndBuiltin # winner
            minimumVotes = puntag $ pfromData $ pfield @"execute" # inputProposalDatum.thresholds

        passert "Higgest vote doesn't meet the minimum requirement" $ minimumVotes #<= highestVote

        let finalResultTag = pfromData $ pfstBuiltin # winner

        effectGroup <- plet $ plookup' # finalResultTag #$ inputProposalDatum.effects

        gatCount <- plet $ plength #$ pto $ pto effectGroup

        passert "Required amount of GATs should be minted" $
          psymbolValueOf # pproposalSymbol # txInfo.mint #== gatCount

        outputsWithGAT <-
          plet $
            pfilter
              # ( phoistAcyclic $
                    plam
                      ( \((pfield @"value" #) -> value) ->
                          0 #< psymbolValueOf # pgatSym # value
                      )
                )
              #$ pfromData txInfo.outputs

        passert "Output GATs is more than minted GATs" $
          plength # outputsWithGAT #== gatCount

        let gatOutputValidator' :: Term s ((PMap PValidatorHash PDatumHash) :--> (PAsData PTxOut) :--> PUnit :--> PUnit)
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

                      passert "GAT must be tagged by the effect hash" $ authorityTokensValidIn # pgatSym # output'
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
        popaque $ singleAuthorityTokenBurned pgatSym ctx.txInfo txInfo.mint
  where
    stateTokenAssetClass :: AssetClass
    stateTokenAssetClass = gstAssetClass gov

    proposalDatum :: Proposal
    proposalDatum =
      Proposal
        { governorSTAssetClass = stateTokenAssetClass
        }

    proposalSymbol :: CurrencySymbol
    proposalSymbol = mintingPolicySymbol policy
      where
        policy = mkMintingPolicy $ proposalPolicy proposalDatum

    pproposalSymbol :: Term s PCurrencySymbol
    pproposalSymbol = phoistAcyclic $ pconstant proposalSymbol

    proposalValidatorAddress :: Address
    proposalValidatorAddress = Address (ScriptCredential hash) Nothing
      where
        hash = validatorHash validator
        validator = mkValidator $ proposalValidator proposalDatum

    pproposalValidatorAddress :: Term s PAddress
    pproposalValidatorAddress = phoistAcyclic $ pconstant proposalValidatorAddress

    stateTokenValueOf :: Term s (PValue :--> PInteger)
    stateTokenValueOf = passetClassValueOf' stateTokenAssetClass

    pgatSym :: Term s PCurrencySymbol
    pgatSym = phoistAcyclic $ pconstant $ gatSymbol gov

--------------------------------------------------------------------------------

-- | Get the assetclass of GST from governor parameters.
gstAssetClass :: Governor -> AssetClass
gstAssetClass gov = AssetClass (symbol, gov.gstName)
  where
    policy :: MintingPolicy
    policy = mkMintingPolicy $ governorPolicy gov

    symbol :: CurrencySymbol
    symbol = mintingPolicySymbol policy

-- | Get the currency symbol of GAT from governor parameters.
gatSymbol :: Governor -> CurrencySymbol
gatSymbol gov = mintingPolicySymbol policy
  where
    at = AuthorityToken $ gstAssetClass gov
    policy = mkMintingPolicy $ authorityTokenPolicy at
