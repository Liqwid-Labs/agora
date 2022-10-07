{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Agora.Effect.GovernorMutation
Maintainer : connor@mlabs.city
Description: An effect that mutates governor settings.

An effect for mutating governor settings.
-}
module Agora.Effect.GovernorMutation (
  -- * Haskell-land
  MutateGovernorDatum (..),

  -- * Plutarch-land
  PMutateGovernorDatum (..),

  -- * Scripts
  mutateGovernorValidator,
) where

import Agora.Effect (makeEffect)
import Agora.Governor (
  GovernorDatum,
  PGovernorDatum,
  pisGovernorDatumValid,
 )
import Agora.Plutarch.Orphans ()
import Plutarch.Api.V1 (PCurrencySymbol)
import Plutarch.Api.V2 (
  PTxOutRef,
  PValidator,
 )
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Extra.AssetClass (PAssetClass, passetClassValueOf)
import Plutarch.Extra.Maybe (
  passertPJust,
 )
import Plutarch.Extra.ScriptContext (pfromOutputDatum, pisScriptAddress)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pguardC, pletFieldsC)
import Plutarch.Lift (PConstantDecl, PLifted, PUnsafeLiftDecl)
import PlutusLedgerApi.V1 (TxOutRef)
import PlutusTx qualified

--------------------------------------------------------------------------------

{- | Haskell-level datum for the governor mutation effect script.

     @since 0.1.0
-}
data MutateGovernorDatum = MutateGovernorDatum
  { governorRef :: TxOutRef
  -- ^ Referenced governor state UTXO should be updated by the effect.
  , newDatum :: GovernorDatum
  -- ^ The new settings for the governor.
  }
  deriving stock
    ( -- | @since 0.1.ç
      Show
    , -- | @since 0.1.ç
      Generic
    )

PlutusTx.makeIsDataIndexed ''MutateGovernorDatum [('MutateGovernorDatum, 0)]

--------------------------------------------------------------------------------

{- | Plutarch-level version of 'MutateGovernorDatum'.

     @since 0.1.0
-}
newtype PMutateGovernorDatum (s :: S)
  = PMutateGovernorDatum
      ( Term
          s
          ( PDataRecord
              '[ "governorRef" ':= PTxOutRef
               , "newDatum" ':= PGovernorDatum
               ]
          )
      )
  deriving stock
    ( -- | @since 0.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      PlutusType
    , -- | @since 0.1.0
      PIsData
    , -- | @since 0.1.0
      PDataFields
    , -- | @since 0.1.0
      PEq
    )

instance DerivePlutusType PMutateGovernorDatum where
  type DPTStrat _ = PlutusTypeData

-- | @since 0.1.0
instance PUnsafeLiftDecl PMutateGovernorDatum where type PLifted PMutateGovernorDatum = MutateGovernorDatum

-- | @since 0.1.0
deriving via (DerivePConstantViaData MutateGovernorDatum PMutateGovernorDatum) instance (PConstantDecl MutateGovernorDatum)

-- | @since 0.1.0
deriving anyclass instance PTryFrom PData PMutateGovernorDatum

--------------------------------------------------------------------------------

{- | Validator for the governor mutation effect.

     This effect is implemented using the 'Agora.Effect.makeEffect' wrapper,
     meaning that the burning of GAT is checked in said wrapper.

     In order to locate the governor, the validator is parametrized with a 'Agora.Governor.Governor'.

     All the information it needs to validate the effect is encoded in the 'MutateGovernorDatum',
      so regardless what redeemer it's given, it will check:

     - No token is minted/burnt other than GAT.
     - Nothing is being paid to the the effect validator.
     - The governor's state UTXO must be spent:

         * It carries exactly one GST.
         * It's referenced by 'governorRef' in the effect's datum.

     - A new state UTXO is paid to the governor:

         * It contains the GST.
         * It has valid governor state datum.
         * The datum is exactly the same as the 'newDatum'.

     @since 0.1.0
-}
mutateGovernorValidator ::
  ClosedTerm (PAssetClass :--> PCurrencySymbol :--> PValidator)
mutateGovernorValidator =
  plam $ \gtAssetClass -> makeEffect @PMutateGovernorDatum $
    \_ datum _ txInfo -> unTermCont $ do
      datumF <- pletFieldsC @'["newDatum", "governorRef"] datum
      txInfoF <- pletFieldsC @'["mint", "inputs", "outputs", "datums"] txInfo

      let gstValueOf =
            plam $ \v -> passetClassValueOf # pfromData v # gtAssetClass

      let mint :: Term _ (PBuiltinList _)
          mint = pto $ pto $ pto $ pfromData txInfoF.mint

      pguardC "Nothing should be minted/burnt other than GAT" $
        plength # mint #== 1

      -- Only two script inputs are alloed: one from the effect, one from the governor.
      pguardC "Only self and governor script inputs are allowed" $
        pfoldr
          # phoistAcyclic
            ( plam $ \inInfo count ->
                let address = pfield @"address" #$ pfield @"resolved" # inInfo
                 in pif
                      (pisScriptAddress # address)
                      (count + 1)
                      count
            )
          # (0 :: Term _ PInteger)
          # pfromData txInfoF.inputs
          #== 2

      -- Find the governor input by looking for GST.
      let inputWithGST =
            passertPJust # "Governor input not found" #$ pfind
              # plam
                ( \inInfo ->
                    let value = pfield @"value" #$ pfield @"resolved" # inInfo
                     in gstValueOf # value #== 1)
              # pfromData txInfoF.inputs

      govInInfo <- pletFieldsC @'["outRef", "resolved"] $ inputWithGST

      -- The effect can only modify the governor UTXO referenced in the datum.
      pguardC "Can only modify the pinned governor" $
        govInInfo.outRef #== datumF.governorRef

      -- The transaction can only have one output, which should be sent to the governor.
      pguardC "Only governor output is allowed" $
        plength # pfromData txInfoF.outputs #== 1

      let govAddress = pfield @"address" #$ govInInfo.resolved
          govOutput' = phead # pfromData txInfoF.outputs

      govOutput <- pletFieldsC @'["address", "value", "datum"] govOutput'

      pguardC "No output to the governor" $
        govOutput.address #== govAddress

      pguardC "Governor output doesn't carry the GST" $
        gstValueOf # govOutput.value #== 1

      let governorOutputDatum =
            ptrace "Governor output datum not found" $
              pfromOutputDatum @PGovernorDatum # govOutput.datum # txInfoF.datums

      -- Ensure the output governor datum is what we want.
      pguardC "Unexpected governor datum" $
        datumF.newDatum #== governorOutputDatum
      pguardC "New governor datum should be valid" $
        pisGovernorDatumValid # governorOutputDatum

      return $ popaque $ pconstant ()
