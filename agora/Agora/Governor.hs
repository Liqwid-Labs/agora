{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Agora.Governor
Maintainer : emi@haskell.fyi
Description: Governor entity scripts acting as authority of entire system.

Governor entity scripts acting as authority of entire system.
-}
module Agora.Governor (
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
  governorStateTokenAssetClass,
  authorityTokenSymbolFromGovernor,
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
  PProposalStatus (PExecutable, PFinished, PDraft),
  PProposalThresholds,
  PProposalVotes (PProposalVotes),
  PResultTag (PResultTag),
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
  PMintingPolicy,
  PScriptPurpose (PSpending),
  PValidator,
  PValue,
  mintingPolicySymbol,
  mkMintingPolicy,
  mkValidator,
  validatorHash,
 )
import Plutarch.Api.V1.Extra (
  passetClass,
  passetClassValueOf,
  pownMintValue,
 )
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Lift (PUnsafeLiftDecl (..))
import Plutarch.Monadic qualified as P
import Plutarch.Unsafe (punsafeCoerce)

--------------------------------------------------------------------------------

import Plutarch.Builtin (pforgetData)
import Plutarch.Map.Extra (plookup, plookup')
import Plutarch.SafeMoney (puntag)
import Plutus.V1.Ledger.Api (
  Address (Address),
  Credential (ScriptCredential),
  CurrencySymbol (..),
  MintingPolicy,
  TxOutRef,
  Validator,
  ValidatorHash,
 )
import Plutus.V1.Ledger.Value (
  AssetClass (..),
  TokenName (..),
 )
import PlutusTx qualified

--------------------------------------------------------------------------------

-- | Datum for the Governor script.
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
    MutateMutateGovernor
  deriving stock (Show, GHC.Generic)

PlutusTx.makeIsDataIndexed
  ''GovernorRedeemer
  [ ('CreateProposal, 0)
  , ('MintGATs, 1)
  , ('MutateMutateGovernor, 2)
  ]

{- | Parameters for creating Governor scripts. 

   Governance State Token, aka GST, is an NFT which idetifies the governance state utxo.
-}
data Governor = Governor
  { gstORef :: TxOutRef
  -- ^ Referenced utxo will be spent to mint the GST
  , gstName :: TokenName
  -- ^ Name of the GST token
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

{- | Policy for Governors.
   This policy mints a GST. It perform the following checks:

    - The utxo specified in the Governor parameter is spent.
    - Only one token is minted.
    - Ensure the token name is 'gstName'.
-}
governorPolicy :: Governor -> ClosedTerm PMintingPolicy
governorPolicy params =
  plam $ \_ ctx' -> P.do
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    let oref = pconstant params.gstORef
        ownSymbol = pownCurrencySymbol # ctx'
        ownAssetClass = passetClass # ownSymbol # pconstant params.gstName

    mintValue <- plet $ pownMintValue # ctx'

    passert "Referenced utxo should be spent" $ pisUxtoSpent # oref # ctx.txInfo

    passert "Exactly one token should be minted" $
      psymbolValueOf # ownSymbol # mintValue #== 1
        #&& passetClassValueOf # ownSymbol # pconstant governorStateTokenName # mintValue #== 1

    popaque (pconstant ())

{- Validator for Governors.

   No matter what redeemer it receives, it will always check:
    - The utxo which has the GST must be spent.
    - The GST always stays at the script address.
    - The state utxo has a valid 'GovernorDatum' datum.

   For 'CreateProposal' redeemers, it will check:
    - Governance state 'nextProposalId' must be advanced.
    - Exactly one proposal state token is minted, or rather, only one proposal is created.
    - Exactly one utxo should be sent to the proposal validator. This utxo must contain the proposal state token, and has a valid datum of type 'ProposalDatum'.
    - Said proposal copies its id and thresholds from the governor, is in draft state, and has zero votes.

   For 'MintGATs' redeemers, it will check:
    - Governance state datum is not changed.
    - Exactly one proposal(the input proposal) is being processed.
    - The input proposal must be in executable state and have required amount of votes.
    - An appropriate effect group is selected to be executed.
    - A valid GAT is minted and sent to every effect, 
    - Exactly one utxo should be sent back to the proposal validator. This utxo must contain the proposal state token, and also has a valid datum of type 'ProposalDatum'(the output proposal).
    - Said output proposal's status should be `Finished`. Other than that, nothing should be changed compare to the input proposal.

   For 'MutateGovernors' redeemers, it will check:
    - Exactly one GAT is burnt.
    - Said GAT must be valid.
-}
governorValidator :: Governor -> ClosedTerm PValidator
governorValidator params =
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

        passert "Exactly one proposal token must be minted" $
          hasOnlyOneTokenOfCurrencySymbol # pproposalSym # txInfo.mint

        outputs <- plet $ findOutputsToAddress # ctx.txInfo # pproposalValidatorAddress
        passert "Exactly one utxo should be sent to the proposal validator" $
          plength # outputs #== 1

        output <- pletFields @'["value", "datumHash"] $ phead # outputs
        passert "The proposal state token must be sent to the proposal validator" $
          psymbolValueOf # pproposalSym # output.value #== 1

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

        passert "Proposal state should be draft" $ isProposalDraft

        popaque $ pconstant ()
      PMintGATs _ -> P.do
        passert "Governor state should not be changed" $
          -- FIXME: There should be a better way to do this
          (pforgetData $ pdata newDatum') #== datum'

        inputsWithProposalStateToken <-
          plet $
            pfilter
              # ( plam $ \(((pfield @"value" #) . (pfield @"resolved" #)) -> value) ->
                    psymbolValueOf # pproposalSym # value #== 1
                )
              #$ pfromData txInfo.inputs

        outputsWithProposalStateToken <-
          plet $
            pfilter
              # ( plam $ \((pfield @"value" #) -> value) ->
                    psymbolValueOf # pproposalSym # value #== 1
                )
              #$ pfromData txInfo.outputs

        passert "The governor can only process one proposal at a time" $
          plength # inputsWithProposalStateToken #== 1
            #&& (psymbolValueOf # pproposalSym #$ pvalueSpent # txInfo') #== 1

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

        passert "Proposal must be in executable state in order to execute effects" $ isProposalExecutable

        -- TODO: not sure if I did the right thing, can't use haskell level constructor here
        let fields =
              pdcons @"id" # inputProposalDatum.id
                #$ pdcons @"effects" # inputProposalDatum.effects
                #$ pdcons @"status" # (pdata $ pcon $ PFinished pdnil)
                #$ pdcons @"cosigners" # inputProposalDatum.cosigners
                #$ pdcons @"thresholds" # inputProposalDatum.thresholds
                #$ pdcons @"votes" # inputProposalDatum.votes # pdnil

            expectedOutputDatum = pforgetData $ pdata fields

        passert "Unexpected output proposal datum" $
          (pforgetData $ pdata outputProposalDatum') #== expectedOutputDatum

        -- TODO: anything else to check here?

        PProposalVotes votes' <- pmatch $ pfromData inputProposalDatum.votes
        votes <- plet votes'

        let minimumVotes = puntag $ pfromData $ pfield @"execute" # inputProposalDatum.thresholds

            yesVotes = plookup' # pyesResultTag # votes
            noVotes = plookup' # pnoResultTag # votes
            biggerVotes = pif (yesVotes #< noVotes) noVotes yesVotes

        passert "Number of votes doesn't meet the minimum requirement" $
          minimumVotes #< biggerVotes

        let finalResultTag = pif (yesVotes #< noVotes) pnoResultTag pyesResultTag

        effects <- plet $ plookup' # finalResultTag #$ inputProposalDatum.effects

        gatCount <- plet $ plength #$ pto $ pto effects

        passert "Required amount of GATs should be minted" $
          psymbolValueOf # pproposalSym # txInfo.mint #== gatCount

        outputsWithGAT <-
          plet $
            pfilter
              # ( plam $ \((pfield @"value" #) -> value) ->
                    0 #< psymbolValueOf # pgatSym # value
                )
              #$ pfromData txInfo.outputs

        passert "Output GATs is more than minted GATs" $
          plength # outputsWithGAT #== gatCount

        popaque $
          pfoldr
            # ( plam $ \(pfromData -> output') _ -> P.do
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
                  (pconstant ())
              )
            # (pconstant ())
            # outputsWithGAT
      PMutateGovernor _ -> P.do
        popaque $ singleAuthorityTokenBurned pgatSym ctx.txInfo txInfo.mint
  where
    stateTokenAssetClass :: AssetClass
    stateTokenAssetClass = governorStateTokenAssetClass params

    proposalDatum :: Proposal
    proposalDatum =
      Proposal
        { governorSTAssetClass = stateTokenAssetClass
        }

    proposalSymbol :: CurrencySymbol
    proposalSymbol = mintingPolicySymbol policy
      where
        policy :: MintingPolicy
        policy = mkMintingPolicy $ proposalPolicy proposalDatum

    pproposalSym :: Term s PCurrencySymbol
    pproposalSym = phoistAcyclic $ pconstant proposalSymbol

    proposalValidatorAddress :: Address
    proposalValidatorAddress = Address (ScriptCredential hash) Nothing
      where
        hash :: ValidatorHash
        hash = validatorHash validator

        validator :: Validator
        validator = mkValidator $ proposalValidator proposalDatum

    pproposalValidatorAddress :: Term s PAddress
    pproposalValidatorAddress = phoistAcyclic $ pconstant proposalValidatorAddress

    stateTokenValueOf :: Term s (PValue :--> PInteger)
    stateTokenValueOf = passetClassValueOf' stateTokenAssetClass

    pgatSym :: Term s PCurrencySymbol
    pgatSym = phoistAcyclic $ pconstant $ authorityTokenSymbolFromGovernor params

    pyesResultTag :: Term s PResultTag
    pyesResultTag = phoistAcyclic $ pcon $ PResultTag $ pconstant 1

    pnoResultTag :: Term s PResultTag
    pnoResultTag = phoistAcyclic $ pcon $ PResultTag $ pconstant 0

--------------------------------------------------------------------------------

governorStateTokenAssetClass :: Governor -> AssetClass
governorStateTokenAssetClass gov = AssetClass (symbol, gov.gstName)
  where
    policy :: MintingPolicy
    policy = mkMintingPolicy $ governorPolicy gov

    symbol :: CurrencySymbol
    symbol = mintingPolicySymbol policy

authorityTokenSymbolFromGovernor :: Governor -> CurrencySymbol
authorityTokenSymbolFromGovernor gov = mintingPolicySymbol policy
  where
    params = AuthorityToken $ governorStateTokenAssetClass gov
    policy = mkMintingPolicy $ authorityTokenPolicy params
