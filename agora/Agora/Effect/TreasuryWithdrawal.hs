{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Agora.Effect.TreasuryWithdrawal
Maintainer : seungheon.ooh@gmail.com
Description: An Effect that withdraws treasury deposit

An Effect that withdraws treasury deposit
-}
module Agora.Effect.TreasuryWithdrawal (
  TreasuryWithdrawalDatum (TreasuryWithdrawalDatum),
  PTreasuryWithdrawalDatum (PTreasuryWithdrawalDatum),
  treasuryWithdrawalValidator,
) where

import Agora.Effect (makeEffect)
import Agora.SafeMoney (AuthorityTokenTag)
import Plutarch.Api.V1 (
  PCredential,
  PCurrencySymbol,
  PValue,
  ptuple,
 )
import Plutarch.Api.V2 (
  AmountGuarantees (Positive),
  KeyGuarantees (Sorted),
  PTuple,
  PTxInInfo,
  PTxOut,
  PValidator,
 )
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Extra.Field (pletAllC)
import Plutarch.Extra.ScriptContext (pisPubKey)
import Plutarch.Extra.Tagged (PTagged)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import PlutusLedgerApi.V1.Credential (Credential)
import PlutusLedgerApi.V1.Value (Value)
import PlutusTx qualified
import "liqwid-plutarch-extra" Plutarch.Extra.List (pdeleteFirst)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC)

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

-- | @since 0.1.0
PlutusTx.makeLift ''TreasuryWithdrawalDatum

-- | @since 0.1.0
PlutusTx.makeIsDataIndexed ''TreasuryWithdrawalDatum [('TreasuryWithdrawalDatum, 0)]

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
  type DPTStrat _ = PlutusTypeData

-- | @since 0.1.0
instance PUnsafeLiftDecl PTreasuryWithdrawalDatum where
  type PLifted PTreasuryWithdrawalDatum = TreasuryWithdrawalDatum

-- | @since 0.1.0
deriving via
  (DerivePConstantViaData TreasuryWithdrawalDatum PTreasuryWithdrawalDatum)
  instance
    (PConstantDecl TreasuryWithdrawalDatum)

-- | @since 0.1.0
instance PTryFrom PData PTreasuryWithdrawalDatum

{- | Withdraws given list of values to specific target addresses.
     It can be evoked by burning GAT. The transaction should have correct
     outputs to the users and any left overs should be paid back to the treasury.

     The validator does not accept any Redeemer as all "parameters" are provided
     via encoded Datum.

     NOTE: It should check...

       1. Transaction outputs should contain all of what Datum specified

       2. Left over assets should be redirected back to Treasury

     It can be more flexiable over...

     - The number of outputs themselves

     @since 1.0.0
-}
treasuryWithdrawalValidator ::
  forall (s :: S).
  Term s (PTagged AuthorityTokenTag PCurrencySymbol :--> PValidator)
treasuryWithdrawalValidator = plam $
  makeEffect $
    \_cs (datum :: Term _ PTreasuryWithdrawalDatum) effectInputRef txInfo -> unTermCont $ do
      datumF <- pletAllC datum
      txInfoF <- pletFieldsC @'["outputs", "inputs"] txInfo

      let validateInput :: Term _ (PTxInInfo :--> PBool)
          validateInput = plam $ \input -> unTermCont $ do
            inputF <- pletAllC input

            cred <-
              pletC $
                pfield @"credential"
                  #$ pfield @"address"
                  # inputF.resolved

            pure $
              foldl1
                (#||)
                [ ptraceIfTrue "Effect input" $ inputF.outRef #== effectInputRef
                , ptraceIfTrue "Treasury input" $ pelem # cred # datumF.treasuries
                , ptraceIfTrue "Collateral input" $ pisPubKey # pfromData cred
                ]

          validateOutput ::
            Term
              _
              ( PBuiltinList (PAsData (PTuple PCredential (PValue 'Sorted 'Positive)))
                  :--> PTxOut
                  :--> PBuiltinList (PAsData (PTuple PCredential (PValue 'Sorted 'Positive)))
              )
          validateOutput = plam $ \receivers output -> unTermCont $ do
            outputF <- pletFieldsC @'["address", "value"] output
            cred <- pletC $ pfield @"credential" # pfromData outputF.address

            let credValue = pdata $ ptuple # cred # outputF.value

                shouldSendToTreasury =
                  pif
                    (pelem # cred # datumF.treasuries)
                    receivers
                    (ptraceError "Invalid receiver")

            pure $
              pmatch (pdeleteFirst # credValue # receivers) $ \case
                PJust updatedReceivers ->
                  ptrace "Receiver output" updatedReceivers
                PNothing ->
                  ptrace "Treasury output" shouldSendToTreasury

      pguardC "All input are valid" $
        pall # validateInput # txInfoF.inputs

      pguardC "All receiver get correct output" $
        pnull #$ pfoldl # validateOutput # datumF.receivers # txInfoF.outputs

      pure . popaque $ pconstant ()
