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

--------------------------------------------------------------------------------

import Control.Applicative (Const)
import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

--------------------------------------------------------------------------------

import Plutarch.Api.V1 (
  PTxOutRef,
  PValidator,
  PValue,
 )
import Plutarch.Api.V1.Extra (pvalueOf)
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Lift (PConstantDecl, PLifted, PUnsafeLiftDecl)
import Plutarch.TryFrom (PTryFrom (..))
import Plutarch.Unsafe (punsafeCoerce)

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Api (TxOutRef)
import Plutus.V1.Ledger.Value (AssetClass (..))
import PlutusTx qualified

--------------------------------------------------------------------------------

import Agora.Effect (makeEffect)
import Agora.Governor (
  Governor,
  GovernorDatum,
  PGovernorDatum,
  governorDatumValid,
 )
import Agora.Governor.Scripts (
  authorityTokenSymbolFromGovernor,
  governorSTAssetClassFromGovernor,
 )
import Agora.Utils (
  isScriptAddress,
  mustBePDJust,
  mustBePJust,
  ptryFindDatum,
  tcassert,
 )

--------------------------------------------------------------------------------

-- | Haskell-level datum for the governor mutation effect script.
data MutateGovernorDatum = MutateGovernorDatum
  { governorRef :: TxOutRef
  -- ^ Referenced governor state UTXO should be updated by the effect.
  , newDatum :: GovernorDatum
  -- ^ The new settings for the governor.
  }
  deriving stock (Show, GHC.Generic)
  deriving anyclass (Generic)

PlutusTx.makeIsDataIndexed ''MutateGovernorDatum [('MutateGovernorDatum, 0)]

--------------------------------------------------------------------------------

-- | Plutarch-level version of 'MutateGovernorDatum'.
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
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields, PEq)
    via (PIsDataReprInstances PMutateGovernorDatum)

instance PUnsafeLiftDecl PMutateGovernorDatum where type PLifted PMutateGovernorDatum = MutateGovernorDatum
deriving via (DerivePConstantViaData MutateGovernorDatum PMutateGovernorDatum) instance (PConstantDecl MutateGovernorDatum)

-- TODO: Derive this.
instance PTryFrom PData (PAsData PMutateGovernorDatum) where
  type PTryFromExcess PData (PAsData PMutateGovernorDatum) = Const ()
  ptryFrom' d k =
    k (punsafeCoerce d, ())

--------------------------------------------------------------------------------

{- | Validator for the governor mutation effect.

  This effect is implemented using the 'Agora.Effect.makeEffect' wrapper,
   meaning that the burning of GAT is checked in the said wrapper.

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
-}
mutateGovernorValidator :: Governor -> ClosedTerm PValidator
mutateGovernorValidator gov = makeEffect (authorityTokenSymbolFromGovernor gov) $
  \_gatCs (datum' :: Term _ PMutateGovernorDatum) _ txInfo' -> unTermCont $ do
    datum <- tcont $ pletFields @'["newDatum", "governorRef"] datum'
    txInfo <- tcont $ pletFields @'["mint", "inputs", "outputs", "datums"] txInfo'

    let mint :: Term _ (PBuiltinList _)
        mint = pto $ pto $ pto $ pfromData txInfo.mint

    tcassert "Nothing should be minted/burnt other than GAT" $
      plength # mint #== 1

    tcassert "Only self and governor script inputs are allowed" $
      pfoldr
        # phoistAcyclic
          ( plam $ \inInfo count ->
              let address = pfield @"address" #$ pfield @"resolved" # inInfo
               in pif
                    (isScriptAddress # address)
                    (count + 1)
                    count
          )
        # (0 :: Term _ PInteger)
        # pfromData txInfo.inputs
        #== 2

    let inputWithGST =
          mustBePJust # "Governor input not found" #$ pfind
            # phoistAcyclic
              ( plam $ \inInfo ->
                  let value = pfield @"value" #$ pfield @"resolved" # inInfo
                   in gstValueOf # value #== 1
              )
            # pfromData txInfo.inputs

    govInInfo <- tcont $ pletFields @'["outRef", "resolved"] $ inputWithGST

    tcassert "Can only modify the pinned governor" $
      govInInfo.outRef #== datum.governorRef

    tcassert "Only governor ouput is allowed" $
      plength # pfromData txInfo.outputs #== 1

    let govAddress = pfield @"address" #$ govInInfo.resolved
        govOutput' = pfromData $ phead # pfromData txInfo.outputs

    govOutput <- tcont $ pletFields @'["address", "value", "datumHash"] govOutput'

    tcassert "No output to the governor" $
      govOutput.address #== govAddress

    tcassert "Governor output doesn't carry the GST" $
      gstValueOf # govOutput.value #== 1

    let governorOutputDatumHash =
          mustBePDJust # "Governor output doesn't have datum" # govOutput.datumHash
        governorOutputDatum =
          pfromData @PGovernorDatum $
            mustBePJust # "Governor output datum not found"
              #$ ptryFindDatum # governorOutputDatumHash # txInfo.datums

    tcassert "Unexpected governor datum" $ datum.newDatum #== governorOutputDatum
    tcassert "New governor datum should be valid" $ governorDatumValid # governorOutputDatum

    return $ popaque $ pconstant ()
  where
    gstValueOf :: Term s (PValue :--> PInteger)
    gstValueOf = phoistAcyclic $ plam $ \v -> pvalueOf # v # pconstant cs # pconstant tn
      where
        AssetClass (cs, tn) = governorSTAssetClassFromGovernor gov
