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

import Agora.AuthorityToken (authorityTokensValidIn)
import Agora.Proposal (
  PProposalId,
  PProposalThresholds,
  ProposalId,
  ProposalThresholds,
  pnextProposalId,
 )
import Agora.Utils (
  allInputs,
  findOutputsToAddress,
  findTxOutDatum,
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

import Plutus.V1.Ledger.Api (MintingPolicy, TxOutRef)
import Plutus.V1.Ledger.Value (AssetClass (..), CurrencySymbol, TokenName (..))
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
  -- ^ The state token that identifies the governor datum will be minted using this utxo.
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
      (plength #$ pto $ pto $ pto mintValue) #== 1

    popaque (pconstant ())

-- | Validator for Governors.
governorValidator :: Governor -> ClosedTerm PValidator
governorValidator params =
  plam $ \datum' redeemer' ctx' -> P.do
    -- TODO: use `PTryFrom`
    redeemer <- pmatch $ pfromData @PGovernorRedeemer $ punsafeCoerce redeemer'
    ctx <- pletFields @'["txInfo", "purpose"] ctx'

    txInfo <- plet $ pfromData $ ctx.txInfo

    PSpending ((pfield @"_0" #) -> txOutRef') <- pmatch $ pfromData ctx.purpose
    let txOutRef = pfromData txOutRef'

    PJust ((pfield @"resolved" #) -> ownInput') <- pmatch $ pfindTxInByTxOutRef # txOutRef # txInfo
    ownInput <- pletFields @'["address", "value", "datumHash"] ownInput'
    let selfAddress = pfromData $ ownInput.address

    PJust oldDatum'' <- pmatch $ findTxOutDatum # txInfo # ownInput'
    oldDatum' <- plet $ pto oldDatum''
    let oldParams' = pfromData @PGovernorDatum $ punsafeCoerce oldDatum'
    oldParams <- pletFields @'["proposalThresholds", "nextProposalId"] oldParams'

    let ownInputDatumNFTAmount = datumNFTValueOf # ownInput.value
    passert "Own input should have exactly one datum NFT" $ ownInputDatumNFTAmount #== 1

    ownOutputs <- plet $ findOutputsToAddress # txInfo # selfAddress
    passert "Exactly one utxo should be sent to the governor" $ plength # ownOutputs #== 1

    ownOutput <- pletFields @'["value", "datumHash"] $ phead # ownOutputs
    let ownOuputDatumNFTAmount = datumNFTValueOf # ownOutput.value
    passert "Datum NFT should stay at governor's address" $ ownOuputDatumNFTAmount #== 1
    passert "Output utxo to governor should have datum" $ pisDJust # ownOutput.datumHash

    -- TODO: use `PTryFrom` and reject bad datum
    let newDatum' = pfromData @PGovernorDatum $ punsafeCoerce datum'
    newParams <- pletFields @'["proposalThresholds", "nextProposalId"] newDatum'

    mint <- plet $ pfromData $ pfield @"mint" # txInfo
    mint' <- plet $ pto $ pto $ pto mint

    case redeemer of
      PCreateProposal _ -> P.do
        -- check that nothing is minted
        passert "Nothing should be minted" $ plength # mint' #== 0

        -- check that proposal is advanced
        passert "Proposal id should be advanced by 1" $
          pnextProposalId # oldParams.nextProposalId #== newParams.nextProposalId

        -- TODO: waiting for impl of proposal
        ptraceError "Not implemented yet"
      PMintGATs _ -> P.do
        -- check datum is not changed
        passert "Datum should not be changed" $ oldDatum' #== datum'

        -- check exactly one(?) authority token is minted

        -- TODO: waiting for impl of proposal
        ptraceError "Not implemented yet"
      PMutateGovernor _ -> P.do
        -- check that input has exactly one GAT and will be burnt
        let gatAmount = psymbolValueOf # gatS # mint
        passert "One GAT should be burnt" $ gatAmount #== -1

        -- nothing should be minted/burnt other than GAT
        passert "No token should be minted/burnt other than GAT" $ plength # mint' #== 1

        -- check that GAT is tagged by the address
        passert "all input GATs are valid" $
          allInputs @PUnit # txInfo #$ plam $ \txOut _ _ _ ->
            authorityTokensValidIn # gatS # txOut

        popaque $ pconstant ()
  where
    datumNFTValueOf :: Term s (PValue :--> PInteger)
    datumNFTValueOf = passetClassValueOf' $ governorStateTokenAssetClass params

    gatS :: Term s PCurrencySymbol
    gatS = pconstant params.gatSymbol

--------------------------------------------------------------------------------

governorStateTokenAssetClass :: Governor -> AssetClass
governorStateTokenAssetClass gov = AssetClass (symbol, governorStateTokenName)
  where
    policy :: MintingPolicy
    policy = mkMintingPolicy $ governorPolicy gov

    symbol :: CurrencySymbol
    symbol = mintingPolicySymbol policy
