{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Agora.Effects.GovernorMutation
Maintainer : chfanghr@gmail.com
Description: An effect that mutates governor settings

An effect for mutating governor settings
-}
module Agora.Effects.GovernorMutation (
  -- * Haskell-land
  MutateGovernorDatum (..),

  -- * Plutarch-land
  PMutateGovernorDatum (..),
  
  -- * Scripts
  mutateGovernorValidator,
) where

--------------------------------------------------------------------------------

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))
import Prelude

--------------------------------------------------------------------------------

import Plutarch (popaque)
import Plutarch.Api.V1 (
  PMaybeData (PDJust),
  PTxOutRef,
  PValidator,
  PValue,
 )
import Plutarch.Builtin (pforgetData)
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Lift (PLifted, PUnsafeLiftDecl)
import Plutarch.Monadic qualified as P

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Api (TxOutRef)
import PlutusTx qualified

--------------------------------------------------------------------------------

import Agora.Effect (makeEffect)
import Agora.Governor (
  Governor,
  GovernorDatum,
  PGovernorDatum,
  gatSymbol,
  gstAssetClass,
 )
import Agora.Utils (
  findOutputsToAddress,
  passert,
  passetClassValueOf',
  pfindDatum,
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
    (PlutusType, PIsData, PDataFields)
    via (PIsDataReprInstances PMutateGovernorDatum)

instance PUnsafeLiftDecl PMutateGovernorDatum where type PLifted PMutateGovernorDatum = MutateGovernorDatum
deriving via (DerivePConstantViaData MutateGovernorDatum PMutateGovernorDatum) instance (PConstant MutateGovernorDatum)

--------------------------------------------------------------------------------

{- | Validator for the governor mutation effect. 

   This effect is implemented using the 'Agora.Effect.makeEffect' wrapper, 
    meaning that the burning of GAT is checked in the said wrapper.

   In order to locate the governor, the validator is parametrized with a 'Agora.Governor.Governor'.

   All the information it need to validate the effect is encoded in the 'MutateGovernorDatum',
    so regardless what redeemer it's given, it will check:

   - No token is minted/burnt other than GAT.
   - The reference UTXO in the datum should be spent.
   - Said UTXO carries the GST.
   - A new UTXO, containing the GST and the new governor state datum, is paid to the governor.
-}
mutateGovernorValidator :: Governor -> ClosedTerm PValidator
mutateGovernorValidator gov = makeEffect (gatSymbol gov) $
  \_gatCs (datum :: Term _ PMutateGovernorDatum) _txOutRef txInfo' -> P.do
    let newDatum = pforgetData $ pfield @"newDatum" # datum
        pinnedGovernor = pfield @"governorRef" # datum

    txInfo <- pletFields @'["mint", "inputs", "outputs"] txInfo'

    let mint :: Term _ (PBuiltinList _)
        mint = pto $ pto $ pto $ pfromData txInfo.mint

    passert "Nothing should be minted/burnt other than GAT" $
      plength # mint #== 1

    filteredInputs <-
      plet $
        pfilter
          # ( plam $ \inInfo ->
                let value = pfield @"value" #$ pfield @"resolved" # inInfo
                 in gstValueOf # value #== 1
            )
          # pfromData txInfo.inputs

    passert "Governor's state token must be moved" $
      plength # filteredInputs #== 1

    input <- plet $ phead # filteredInputs

    passert "Can only modify the pinned governor" $
      pfield @"outRef" # input #== pinnedGovernor

    let govAddress =
          pfield @"address"
            #$ pfield @"resolved"
            #$ pfromData input

    filteredOutputs <- plet $ findOutputsToAddress # pfromData txInfo' # govAddress

    passert "Exactly one output to the governor" $
      plength # filteredOutputs #== 1

    outputToGovernor <- plet $ phead # filteredOutputs

    passert "Governor's state token must stay at governor's address" $
      (gstValueOf #$ pfield @"value" # outputToGovernor) #== 1

    outputDatumHash' <- pmatch $ pfromData $ pfield @"datumHash" # outputToGovernor

    case outputDatumHash' of
      PDJust ((pfromData . (pfield @"_0" #)) -> outputDatumHash) -> P.do
        datum' <- pmatch $ pfindDatum # outputDatumHash # pfromData txInfo'
        case datum' of
          PJust datum -> P.do
            passert "Unexpected output datum" $
              pto datum #== newDatum

            popaque $ pconstant ()
          _ -> ptraceError "Output datum not found"
      _ -> ptraceError "Ouput to governor should have datum"
  where
    gstValueOf :: Term s (PValue :--> PInteger)
    gstValueOf = passetClassValueOf' $ gstAssetClass gov
