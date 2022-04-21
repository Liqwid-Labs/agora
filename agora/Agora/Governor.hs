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
  governorStateTokenName,

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
  PProposalStatus (PFinished, PLocked),
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
  containsSingleCurrencySymbol,
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
import Plutarch.Api.V1.Extra (passetClass, passetClassValueOf, pownMintValue)
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

-- | Parameters for creating Governor scripts.
data Governor = Governor
  { stORef :: TxOutRef
  -- ^ An utxo, which will be spent to mint the state token for the governor validator.
  }

governorStateTokenName :: TokenName
governorStateTokenName = TokenName ""

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
   This policy mints a state token for the 'governorValidator'.
   It will check:

    - The utxo specified in the Governor parameter is spent.
    - Only one token is minted.
    - Ensure the token name is "".
-}
governorPolicy :: Governor -> ClosedTerm PMintingPolicy
governorPolicy params =
  plam $ \_ ctx' -> P.do
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    let oref = pconstant params.stORef
        ownSymbol = pownCurrencySymbol # ctx'

    mintValue <- plet $ pownMintValue # ctx'

    passert "Referenced utxo should be spent" $ pisUxtoSpent # oref # ctx.txInfo

    passert "Exactly one token should be minted" $
      psymbolValueOf # ownSymbol # mintValue #== 1
        #&& passetClassValueOf # ownSymbol # pconstant governorStateTokenName # mintValue #== 1

    passert "Nothing is minted other than the state token" $
      containsSingleCurrencySymbol # mintValue

    popaque (pconstant ())

{- Validator for Governors.

   A state token, minted by 'governorPolicy' is used to identify the datum utxo.

   No matter what redeemer it receives, it will always check:
    - The utxo which has the state token must be spent.
    - The state token always stays at the script address.
    - The utxo which holds the state token, has a well well-formed 'GovernorDatum' datum.

   For 'CreateProposal' redeemer, it will check:
    - Exactly one proposal state token is minted.
    - Exactly one utxo should be sent to the proposal validator.
    - The utxo must contain the proposal state token.
    - The datum of said utxo must be correct.
    - Proposal id in the governor datum must be advanced.

   For 'MintGATs' redeemer, it will check:
    - State datum is not changed.
    - Exactly one proposal is being processed.
    - Select the right effect group.
    - Mint one GAT for every effect.
    - The GATs is properly tagged. (Should we do this?)
    - The GATs are sent to the appropraite effects. (Should we do this?)

   For 'MutateGovernor', it will check:
    - A GAT is burnt.
    - Said GAT must be tagged by the effect that is spending it.
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
          hasOnlyOneTokenOfCurrencySymbol # pProposalSym # txInfo.mint

        outputs <- plet $ findOutputsToAddress # ctx.txInfo # pconstant proposalValidatorAddress
        passert "Exactly one utxo should be sent to the proposal validator" $
          plength # outputs #== 1

        output <- pletFields @'["value", "datumHash"] $ phead # outputs
        passert "The proposal state token must be sent to the proposal validator" $
          psymbolValueOf # pProposalSym # output.value #== 1

        passert "The utxo paid to the proposal validator must have datum" $
          pisDJust # output.datumHash

        let inputProposalDatum' =
              mustFindDatum' @PProposalDatum
                # output.datumHash
                # ctx.txInfo

        proposalParams <-
          pletFields
            @'["id", "status", "cosigners", "thresholds", "votes"]
            inputProposalDatum'

        passert "Invalid proposal id in proposal parameters" $
          proposalParams.id #== oldParams.nextProposalId

        passert "Invalid thresholds in proposal parameters" $
          proposalParams.thresholds #== oldParams.proposalThresholds

        -- passert "Initial proposal votes should be empty" $
        --   pnull #$ pto $ pto $ pfromData proposalParams.votes

        -- passert "Initial proposal status should be Draft" $ P.do
        --   s <- pmatch $ proposalParams.status
        --   case s of
        --     PDraft _ -> pconstant True
        --     _ -> pconstant False

        passert "Proposal datum must be valid" $
          proposalDatumValid # inputProposalDatum'

        -- TODO: proposal impl not done yet
        ptraceError "Not implemented yet"
      PMintGATs _ -> P.do
        passert "Datum should not be changed" $
          -- FIXME: There should be a better way to do this
          (pforgetData $ pdata newDatum') #== datum'

        inputsWithProposalStateToken <-
          plet $
            pfilter
              # ( plam $ \(((pfield @"value" #) . (pfield @"resolved" #)) -> value) ->
                    psymbolValueOf # pProposalSym # value #== 1
                )
              #$ pfromData txInfo.inputs

        outputsWithProposalStateToken <-
          plet $
            pfilter
              # ( plam $ \((pfield @"value" #) -> value) ->
                    psymbolValueOf # pProposalSym # value #== 1
                )
              #$ pfromData txInfo.outputs

        passert "One proposal at a time" $
          plength # inputsWithProposalStateToken #== 1
            #&& (psymbolValueOf # pProposalSym #$ pvalueSpent # txInfo') #== 1

        proposalInputTxOut <-
          pletFields @'["address", "value", "datumHash"] $
            pfield @"resolved" #$ phead # inputsWithProposalStateToken
        proposalOutputTxOut <-
          pletFields @'["address", "value", "datumHash"] $
            phead # outputsWithProposalStateToken

        inputProposalDatum' <- plet $ mustFindDatum' @PProposalDatum # proposalInputTxOut.datumHash # txInfo'
        outputProposalDatum' <- plet $ mustFindDatum' @PProposalDatum # proposalOutputTxOut.datumHash # txInfo'

        passert "Proposal datum must be valid" $
          proposalDatumValid # inputProposalDatum' #&& proposalDatumValid # outputProposalDatum'

        inputProposalDatum <- pletFields @'["id", "effects", "status", "cosigners", "thresholds", "votes"] inputProposalDatum'

        let isInputLocked = pmatch (pfromData inputProposalDatum.status) $ \case
              PLocked _ -> pconstant False
              _ -> pconstant False

        passert "Input proposal status must be locked" $ isInputLocked

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

        -- TODO: something else to check here?

        PProposalVotes votes' <- pmatch $ pfromData inputProposalDatum.votes
        votes <- plet votes'

        let minimumVotes = puntag $ pfromData $ pfield @"execute" # inputProposalDatum.thresholds

            yesVotes = plookup' # pyesResultTag # votes
            noVotes = plookup' # pnoResultTag # votes
            biggerVotes = pif (yesVotes #< noVotes) noVotes yesVotes

        passert "Votes should be more than mininum votes" $ minimumVotes #< biggerVotes

        let finalResultTag = pif (yesVotes #< noVotes) pnoResultTag pyesResultTag

        effects <- plet $ plookup' # finalResultTag #$ inputProposalDatum.effects

        gatCount <- plet $ plength #$ pto $ pto effects

        passert "Required amount of GATs should be minted" $
          psymbolValueOf # pProposalSym # txInfo.mint #== gatCount

        passert "No token should be minted other than GAT" $
          containsSingleCurrencySymbol # txInfo.mint

        outputsWithGAT <-
          plet $
            pfilter
              # ( plam $ \((pfield @"value" #) -> value) ->
                    0 #< psymbolValueOf # pGATSym # value
                )
              #$ pfromData txInfo.outputs
        passert "Minted GAT amount should equal to amount of output GAT" $
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
                        mustBePJust # "Receiver is not in effect list"
                          #$ plookup # scriptHash # effects

                  passert "GAT must be tagged by the effect hash" $ authorityTokensValidIn # pGATSym # output'
                  passert "Unexpected datum" $ datumHash #== expectedDatumHash
                  (pconstant ())
              )
            # (pconstant ())
            # outputsWithGAT

      -- TODO: waiting for impl of proposal
      PMutateGovernor _ -> P.do
        passert "No token should be burnt other than GAT" $
          containsSingleCurrencySymbol # txInfo.mint

        popaque $ singleAuthorityTokenBurned pGATSym ctx.txInfo txInfo.mint
  where
    stateTokenAssetClass :: AssetClass
    stateTokenAssetClass = governorStateTokenAssetClass params

    proposalParams :: Proposal
    proposalParams =
      Proposal
        { governorSTAssetClass = stateTokenAssetClass
        }

    proposalSymbol :: CurrencySymbol
    proposalSymbol = mintingPolicySymbol policy
      where
        policy :: MintingPolicy
        policy = mkMintingPolicy $ proposalPolicy proposalParams

    pProposalSym :: Term s PCurrencySymbol
    pProposalSym = phoistAcyclic $ pconstant proposalSymbol

    proposalValidatorAddress :: Address
    proposalValidatorAddress = Address (ScriptCredential hash) Nothing
      where
        hash :: ValidatorHash
        hash = validatorHash validator

        validator :: Validator
        validator = mkValidator $ proposalValidator proposalParams

    stateTokenValueOf :: Term s (PValue :--> PInteger)
    stateTokenValueOf = passetClassValueOf' stateTokenAssetClass

    pGATSym :: Term s PCurrencySymbol
    pGATSym = phoistAcyclic $ pconstant $ authorityTokenSymbolFromGovernor params

    pyesResultTag :: Term s PResultTag
    pyesResultTag = phoistAcyclic $ pcon $ PResultTag $ pconstant 1

    pnoResultTag :: Term s PResultTag
    pnoResultTag = phoistAcyclic $ pcon $ PResultTag $ pconstant 0

--------------------------------------------------------------------------------

governorStateTokenAssetClass :: Governor -> AssetClass
governorStateTokenAssetClass gov = AssetClass (symbol, governorStateTokenName)
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
