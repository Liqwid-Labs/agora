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
) where

--------------------------------------------------------------------------------

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

--------------------------------------------------------------------------------

import Agora.AuthorityToken (
  AuthorityToken (..),
  authorityTokenPolicy,
  singleAuthorityTokenBurned,
 )
import Agora.Proposal (
  PProposalDatum,
  PProposalId,
  PProposalStatus (PDraft),
  PProposalThresholds,
  Proposal (..),
  ProposalId,
  ProposalThresholds,
  pnextProposalId,
  proposalPolicy,
  proposalValidator,
 )
import Agora.Utils (
  containsSingleCurrencySymbol,
  findOutputsToAddress,
  hasOnlyOneTokenOfCurrencySymbol,
  mustFindDatum',
  passert,
  passetClassValueOf,
  passetClassValueOf',
  pfindTxInByTxOutRef,
  pisDJust,
  pisUxtoSpent,
  pownCurrencySymbol,
  psymbolValueOf,
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
import Plutarch.Api.V1.Extra (pownMintValue)
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
  , gatSymbol :: CurrencySymbol
  -- ^ The symbol of the Governance Authority Token.
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

   TODO: PMintGATs

   For 'PMutateGovernor', it will check:
    - A GAT is burnt.
    - Said GAT must be tagged by the effect that is spending it.
-}
governorValidator :: Governor -> ClosedTerm PValidator
governorValidator params =
  plam $ \datum' redeemer' ctx' -> P.do
    -- TODO: use ptryFrom
    redeemer <- pmatch $ pfromData @PGovernorRedeemer $ punsafeCoerce redeemer'
    ctx <- pletFields @'["txInfo", "purpose"] ctx'

    txInfo <- plet $ pfromData $ ctx.txInfo

    PSpending ((pfield @"_0" #) -> txOutRef') <- pmatch $ pfromData ctx.purpose
    let txOutRef = pfromData txOutRef'

    PJust ((pfield @"resolved" #) -> ownInput') <- pmatch $ pfindTxInByTxOutRef # txOutRef # txInfo
    ownInput <- pletFields @'["address", "value"] ownInput'
    let selfAddress = pfromData $ ownInput.address

    -- TODO: use ptryFrom
    let oldParams' = pfromData @PGovernorDatum $ punsafeCoerce datum'
    oldParams <- pletFields @'["proposalThresholds", "nextProposalId"] oldParams'

    let ownInputDatumNFTAmount = stateTokenValueOf # ownInput.value
    passert "Own input should have exactly one state token" $
      ownInputDatumNFTAmount #== 1

    ownOutputs <- plet $ findOutputsToAddress # txInfo # selfAddress
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

    mint <- plet $ pfromData $ pfield @"mint" # txInfo

    case redeemer of
      PCreateProposal _ -> P.do
        pSym <- plet $ pconstant proposalSymbol

        passert "Proposal id should be advanced by 1" $
          pnextProposalId # oldParams.nextProposalId #== newParams.nextProposalId

        passert "Exactly one proposal token must be minted" $
          hasOnlyOneTokenOfCurrencySymbol # pSym # mint

        outputs <- plet $ findOutputsToAddress # ctx.txInfo # pconstant proposalValidatorAddress
        passert "Exactly one utxo should be sent to the proposal validator" $
          plength # outputs #== 1

        output <- pletFields @'["value", "datumHash"] $ phead # outputs
        passert "The proposal state token must be sent to the proposal validator" $
          psymbolValueOf # pSym # output.value #== 1

        passert "The utxo paid to the proposal validator must have datum" $
          pisDJust # output.datumHash

        let proposalDatum' =
              mustFindDatum' @PProposalDatum
                # output.datumHash
                # ctx.txInfo

        proposalParams <-
          pletFields
            @'["id", "status", "cosigners", "thresholds", "votes"]
            proposalDatum'

        passert "Invalid proposal id in proposal parameters" $
          proposalParams.id #== oldParams.nextProposalId

        passert "Invalid thresholds in proposal parameters" $
          proposalParams.thresholds #== oldParams.proposalThresholds

        passert "Initial proposal votes should be zero" $
          pnull #$ pto $ pto $ pfromData proposalParams.votes

        passert "Initial proposal status should be Draft" $ P.do
          s <- pmatch $ proposalParams.status
          case s of
            PDraft _ -> pconstant True
            _ -> pconstant False

        passert "Initial proposal cosigners should be empty" $
          pnull #$ pfromData proposalParams.cosigners

        -- TODO: proposal impl not done yet
        ptraceError "Not implemented yet"
      PMintGATs _ -> P.do
        -- check datum is not changed
        passert "Datum should not be changed" $
          (pforgetData $ pdata newDatum') #== datum'

        -- TODO: any need to check the proposal datum here?

        -- check exactly one(?) authority token is minted

        -- TODO: waiting for impl of proposal
        ptraceError "Not implemented yet"
      PMutateGovernor _ -> P.do
        passert "No token should be minted/burnt other than GAT" $
          containsSingleCurrencySymbol # mint

        popaque $ singleAuthorityTokenBurned gatSym ctx.txInfo mint
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

    proposalValidatorAddress :: Address
    proposalValidatorAddress = Address (ScriptCredential hash) Nothing
      where
        hash :: ValidatorHash
        hash = validatorHash validator

        validator :: Validator
        validator = mkValidator $ proposalValidator proposalParams

    stateTokenValueOf :: Term s (PValue :--> PInteger)
    stateTokenValueOf = passetClassValueOf' stateTokenAssetClass

    authorityTokenParams :: AuthorityToken
    authorityTokenParams =
      AuthorityToken
        { authority = stateTokenAssetClass
        }

    authorityTokenSymbol :: CurrencySymbol
    authorityTokenSymbol = undefined
      where
        policy = authorityTokenPolicy authorityTokenParams

    gatSym :: Term s PCurrencySymbol
    gatSym = pconstant params.gatSymbol

--------------------------------------------------------------------------------

governorStateTokenAssetClass :: Governor -> AssetClass
governorStateTokenAssetClass gov = AssetClass (symbol, governorStateTokenName)
  where
    policy :: MintingPolicy
    policy = mkMintingPolicy $ governorPolicy gov

    symbol :: CurrencySymbol
    symbol = mintingPolicySymbol policy
