{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Agora.Effect.TreasuryWithdrawal
Maintainer : seungheon.ooh@gmail.com
Description: An Effect that withdraws treasury deposit

An Effect that withdraws treasury deposit
-}
module Agora.Effect.TreasuryWithdrawal (
  TreasuryWithdrawalDatum (..),
  PTreasuryWithdrawalDatum (PTreasuryWithdrawalDatum),
  treasuryWithdrawalValidator,
) where

import Agora.Effect (makeEffect)
import Agora.SafeMoney (AuthorityTokenTag)
import Agora.Utils (psubtractSortedValue, puncurryTuple)
import Generics.SOP qualified as SOP
import Plutarch.Api.Internal.Hashing (hashData)
import Plutarch.Api.V1 (PCredential, PCurrencySymbol, PValue)
import Plutarch.Api.V1.Address (PCredential (PPubKeyCredential))
import Plutarch.Api.V1.Value (pforgetPositive)
import Plutarch.Api.V2 (
  AmountGuarantees (Positive),
  KeyGuarantees (Sorted),
  PTuple,
  PTxInInfo,
  PTxOut,
  PValidator,
 )
import Plutarch.Api.V2.Tx (POutputDatum (..))
import Plutarch.DataRepr (
  PDataFields,
 )
import Plutarch.Extra.Field (pletAllC)
import Plutarch.Extra.IsData (
  DerivePConstantViaDataList (
    DerivePConstantViaDataList
  ),
  ProductIsData (ProductIsData),
 )
import Plutarch.Extra.ScriptContext (pisPubKey)
import Plutarch.Extra.Tagged (PTagged)
import Plutarch.Extra.Traversable (pfoldMap)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import PlutusLedgerApi.V1.Credential (Credential)
import PlutusLedgerApi.V1.Scripts (DatumHash (DatumHash))
import PlutusLedgerApi.V1.Value (Value)
import PlutusTx qualified
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pguardC,
  pletC,
  pletFieldsC,
 )

{- | Datum that encodes behavior of Treasury Withdrawal effect.

     Note: This Datum acts like a "predefined redeemer". Which is to say that
     it encodes the properties a redeemer would, but is locked in-place until
     spend.

     @since 0.1.0
-}
data TreasuryWithdrawalDatum = TreasuryWithdrawalDatum
  { receivers :: [(Credential, Value)]
  -- ^ AssocMap for Value sent to each receiver from the treasury.
  , treasuries :: [Credential]
  -- ^ What Credentials is spending from legal.
  }
  deriving stock
    ( -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 1.0.0
      SOP.Generic
    )
  deriving
    ( -- | @since 1.0.0
      PlutusTx.ToData
    , -- | @since 1.0.0
      PlutusTx.FromData
    )
    via (ProductIsData TreasuryWithdrawalDatum)

{- | Haskell-level version of 'TreasuryWithdrawalDatum'.

     @since 0.1.0
-}
newtype PTreasuryWithdrawalDatum (s :: S)
  = PTreasuryWithdrawalDatum
      ( Term
          s
          ( PDataRecord
              '[ "receivers" ':= PBuiltinList (PAsData (PTuple PCredential (PValue 'Sorted 'Positive)))
               , "treasuries" ':= PBuiltinList (PAsData PCredential)
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
    )

instance DerivePlutusType PTreasuryWithdrawalDatum where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 0.1.0
instance PUnsafeLiftDecl PTreasuryWithdrawalDatum where
  type PLifted PTreasuryWithdrawalDatum = TreasuryWithdrawalDatum

-- | @since 0.1.0
deriving via
  (DerivePConstantViaDataList TreasuryWithdrawalDatum PTreasuryWithdrawalDatum)
  instance
    (PConstantDecl TreasuryWithdrawalDatum)

-- | @since 0.1.0
instance PTryFrom PData (PAsData PTreasuryWithdrawalDatum)

{- | Withdraws given list of values to specific target addresses.
     It can be evoked by burning GAT. The transaction should have correct
     outputs to the users and any left overs should be paid back to the treasury.

     The validator does not accept any Redeemer as all "parameters" are provided
     via encoded Datum.

     NOTE: It should check...

       1. Transaction outputs should contain all of what Datum specified

       2. Left over assets should be redirected back to Treasury

    The output order should be:

      1. Receiver outputs. They should be in the same order as the 'receivers' field of the datum.

      2. Other outputs: treasury outputs, colleteral outputs, etc.

     @since 1.0.0
-}
treasuryWithdrawalValidator ::
  forall (s :: S).
  Term s (PAsData (PTagged AuthorityTokenTag PCurrencySymbol) :--> PValidator)
treasuryWithdrawalValidator = plam $
  makeEffect @(PAsData PTreasuryWithdrawalDatum) $
    \_cs (pfromData -> datum) effectInputRef txInfo -> unTermCont $ do
      datumF <- pletAllC datum
      txInfoF <- pletFieldsC @'["outputs", "inputs"] txInfo

      let
        -- Validate the input and if it's from one of the treasuries,
        --  return the value.
        --
        -- Only effect inputs, treasury inputs and public key inputs are
        --  allowed.
        extractTreasuryInputValue ::
          Term _ (PTxInInfo :--> PValue 'Sorted 'Positive)
        extractTreasuryInputValue = plam $ \input -> unTermCont $ do
          inputF <- pletAllC input
          resolvedF <- pletFieldsC @'["address", "value"] inputF.resolved

          cred <- pletC $ pfield @"credential" # resolvedF.address

          let isEffectInput =
                ptraceIfTrue "Effect input" $
                  inputF.outRef #== effectInputRef
              isTreasuryInput =
                ptraceIfTrue "Treasury input" $
                  pelem # pdata cred # datumF.treasuries
              isPubkeyInput =
                ptraceIfTrue "Pubkey input" $
                  pisPubKey # cred
          pure
            $ pif
              (isEffectInput #|| isPubkeyInput)
              mempty
            $ pif isTreasuryInput resolvedF.value
            $ ptraceError "Unknown input"

        treasuryInputAmount =
          pfoldMap
            # extractTreasuryInputValue
            # txInfoF.inputs

        sentAmout =
          pfoldMap
            # plam ((puncurryTuple # plam (const id) #) . pfromData)
            # pfromData datumF.receivers

        treasuryLeftOverAmount =
          psubtractSortedValue
            # treasuryInputAmount
            # sentAmout

        remainingOutputs =
          ptrace "Check receiver outputs" $
            checkReceiverOutputs
              # datumF.receivers
              # txInfoF.outputs

        extractTreasuryOutputValue ::
          Term _ (PTxOut :--> PValue 'Sorted 'Positive)
        extractTreasuryOutputValue = plam $
          flip (pletFields @'["address", "value", "datum"]) $ \outputF ->
            let cred = pfield @"credential" # outputF.address

                isTreasuryOutput =
                  ptraceIfFalse "Should sent to one of the treasuries" $
                    pelem # pdata cred # datumF.treasuries

                isDatumValid =
                  ptraceIfFalse "Valid output datum" $
                    checkOutputDatum # cred # outputF.datum
             in pif
                  (isTreasuryOutput #&& isDatumValid)
                  outputF.value
                  mempty

        -- Return the value if it'll be sent to one of the treasuries.
        treasuryOutputAmount =
          pfoldMap
            # extractTreasuryOutputValue
            # remainingOutputs

      pguardC "Unused treasury should stay at treasury validators" $
        treasuryLeftOverAmount #== pforgetPositive treasuryOutputAmount

      pure . popaque $ pconstant ()
  where
    -- Make sure that all the receivers get the correct payment, return the
    --  remaining outputs.
    --
    -- This function is not hoisted cause it's used only once.
    checkReceiverOutputs ::
      Term
        s
        ( PBuiltinList
            (PAsData (PTuple PCredential (PValue 'Sorted 'Positive)))
            :--> PBuiltinList PTxOut
            :--> PBuiltinList PTxOut
        )
    checkReceiverOutputs = pfix #$ plam $ \self receivers outputs ->
      pelimList
        ( \r rs ->
            pelimList
              ( \o os -> pletFields @'["value", "address", "datum"] o $ \oF ->
                  let isValidReceiverOutput =
                        puncurryTuple
                          # plam
                            ( \expCred expVal ->
                                foldl1
                                  (#&&)
                                  [ ptraceIfFalse "Valid credential" $
                                      expCred #== pfield @"credential" # oF.address
                                  , ptraceIfFalse "Valid value" $
                                      expVal #== oF.value
                                  , ptraceIfFalse "Valid output datum" $
                                      checkOutputDatum # expCred # oF.datum
                                  ]
                            )
                          # pfromData r
                   in pif
                        isValidReceiverOutput
                        (self # rs # os)
                        (ptraceError "Invalid receiver output")
              )
              (ptraceError "Unable to exhaust receivers")
              outputs
        )
        outputs
        receivers

    unitDatum = PlutusTx.toData ()

    unitDatumHash = DatumHash $ hashData unitDatum

    checkOutputDatum :: Term s (PCredential :--> POutputDatum :--> PBool)
    checkOutputDatum = phoistAcyclic $ plam $ \cred datum -> pmatch cred $
      \case
        PPubKeyCredential _ -> pcon PTrue
        _ -> pmatch datum $ \case
          PNoOutputDatum _ -> pcon PFalse
          POutputDatum _ -> pcon PTrue
          POutputDatumHash ((pfield @"datumHash" #) -> hash) ->
            pconstant unitDatumHash #== hash
