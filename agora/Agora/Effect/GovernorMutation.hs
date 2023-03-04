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
  GovernorRedeemer (MutateGovernor),
  PGovernorDatum,
  PGovernorRedeemer,
 )
import Agora.SafeMoney (AuthorityTokenTag, GovernorSTTag)
import Agora.Utils (ptaggedSymbolValueOf)
import Plutarch.Api.V1 (PCurrencySymbol)
import Plutarch.Api.V2 (
  PScriptHash,
  PScriptPurpose (PSpending),
  PTxOutRef,
  PValidator,
 )
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Extra.Field (pletAll, pletAllC)
import Plutarch.Extra.Maybe (passertPJust, pfromJust)
import Plutarch.Extra.Record (mkRecordConstr, (.=))
import Plutarch.Extra.ScriptContext (
  pisScriptAddress,
  pscriptHashFromAddress,
  ptryFromOutputDatum,
  ptryFromRedeemer,
 )
import Plutarch.Extra.Tagged (PTagged)
import Plutarch.Lift (PConstantDecl, PLifted, PUnsafeLiftDecl)
import PlutusLedgerApi.V1 (TxOutRef)
import PlutusTx qualified
import "liqwid-plutarch-extra" Plutarch.Extra.List (ptryFromSingleton)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC)

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
instance PUnsafeLiftDecl PMutateGovernorDatum where
  type PLifted PMutateGovernorDatum = MutateGovernorDatum

-- | @since 0.1.0
deriving via
  (DerivePConstantViaData MutateGovernorDatum PMutateGovernorDatum)
  instance
    (PConstantDecl MutateGovernorDatum)

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

     @since 1.0.0
-}
mutateGovernorValidator ::
  ClosedTerm
    ( PAsData PScriptHash
        :--> PAsData (PTagged GovernorSTTag PCurrencySymbol)
        :--> PAsData (PTagged AuthorityTokenTag PCurrencySymbol)
        :--> PValidator
    )
mutateGovernorValidator =
  plam $ \govValidatorHash gstSymbol -> makeEffect @PMutateGovernorDatum $
    \_gatCs (effectDatum :: Term _ PMutateGovernorDatum) _ txInfo -> unTermCont $ do
      effectDatumF <- pletAllC effectDatum
      txInfoF <- pletFieldsC @'["inputs", "outputs", "datums", "redeemers"] txInfo

      --------------------------------------------------------------------------

      scriptInputs <-
        pletC $
          pfilter
            # plam
              ( \inInfo ->
                  pisScriptAddress
                    #$ pfield @"address"
                    #$ pfield @"resolved"
                    # inInfo
              )
            # pfromData txInfoF.inputs

      -- Only two script inputs are alloed: one from the effect script, another from the governor.
      pguardC "Only self and governor script inputs are allowed" $
        plength # scriptInputs #== 2

      pguardC "Governor input should present" $
        pany
          # plam
            ( flip pletAll $ \inputF ->
                let isGovernorInput =
                      foldl1
                        (#&&)
                        [ ptraceIfFalse "Governor UTxO should carry GST" $
                            ptaggedSymbolValueOf
                              # pfromData gstSymbol
                              # (pfield @"value" # inputF.resolved)
                              #== 1
                        , ptraceIfFalse "Can only modify the pinned governor" $
                            inputF.outRef #== effectDatumF.governorRef
                        , ptraceIfFalse "Governor validator run" $
                            let inputScriptHash =
                                  pfromJust
                                    #$ pscriptHashFromAddress
                                    #$ pfield @"address"
                                    # inputF.resolved
                             in inputScriptHash #== pfromData govValidatorHash
                        ]
                 in isGovernorInput
            )
          # scriptInputs

      let governorRedeemer =
            pfromData $
              passertPJust
                # "Govenor redeemer should be resolved"
                #$ ptryFromRedeemer @(PAsData PGovernorRedeemer)
                # mkRecordConstr PSpending (#_0 .= effectDatumF.governorRef)
                # txInfoF.redeemers

      pguardC "Spend governor with redeemer MutateGovernor" $
        governorRedeemer #== pconstant MutateGovernor

      ----------------------------------------------------------------------------

      let governorOutput =
            ptrace "Only governor output is allowed" $
              ptryFromSingleton # pfromData txInfoF.outputs

          governorOutputDatum =
            ptrace "Resolve governor outoput datum" $
              ptryFromOutputDatum @PGovernorDatum
                # (pfield @"datum" # governorOutput)
                # txInfoF.datums

      pguardC "New governor datum correct" $
        governorOutputDatum #== effectDatumF.newDatum

      return $ popaque $ pconstant ()
