{- |
Module: Agora.Treasury
Maintainer: jack@mlabs.city
Description: Treasury scripts.

Contains the datum, redeemer and validator for a template DAO
treasury.
-}
module Agora.Treasury (treasuryV) where

import GHC.Generics qualified as GHC
import Generics.SOP
import Plutarch.Api.V1.Contexts (
  PScriptContext,
  PTuple,
  PTxInfo (PTxInfo),
 )
import Plutarch.Api.V1.Maybe (PMaybeData (PDJust))
import Plutarch.Api.V1.Scripts (PDatum, PDatumHash)
import Plutarch.Api.V1.Tx (
  PTxInInfo,
  PTxOut,
 )
import Plutarch.Api.V1.Value (PCurrencySymbol, PValue)
import Plutarch.Builtin (pforgetData)
import Plutarch.DataRepr (
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Monadic qualified as P

{- | Validator ensuring that transactions consuming the treasury
     do so in a valid manner.
-}
treasuryV ::
  Term
    s
    ( PAsData PTreasuryDatum
        :--> PAsData PTreasuryRedeemer
        :--> PAsData PScriptContext
        :--> PUnit
    )
treasuryV = plam $ \d r ctx' -> P.do
  -- Load txInfo and purpose fields from script context.
  ctx <- pletFields @["txInfo", "purpose"] ctx'

  -- Extract txInfo.
  PTxInfo txInfo' <- pmatch $ pfromData ctx.txInfo

  -- Pattern match on type of treasury redeemer.
  pmatch (pfromData r) $ \case
    -- Treasury is merely being witnessed. It's datum and value
    -- must be unchanged.
    PWitnessTreasury _ -> P.do
      txInfo <- pletFields @'["inputs", "outputs", "data"] txInfo'

      -- Get datum hash of datum supplied to validator.
      let dat = pfield @"data" # txInfo'
          dH = getTrDatumHash # d # dat

      -- Get inputs in TxOut form.
      let inputs = txInfo.inputs
          rs = pmap # toResolved # inputs

      -- Find the value the treasury had before being spent.
      let valueIn = getValAtDHash # dH # rs

      -- Find the value the treasury has after being spent.
      let outputs = txInfo.outputs
          valueOut = getValAtDHash # dH # outputs

      -- If the value in equals the value out, validate the
      -- transaction. Otherwise, fail.
      pif
        (valueIn #== valueOut)
        (pconstant ())
        $ ptraceError "Treasury is altered when witnessing transaction"

    -- Treasury is receiving amount of funds specified in the
    -- redeemer. It's datum must be unchanged but it's value
    -- must be increased by the specified amount.
    PReceiveFunds _ -> P.do
      pconstant ()

{- | Plutarch level function that, given a treasury datum and a
    list of the transaction's data, will find its hash.
-}
getTrDatumHash ::
  Term
    s
    ( PAsData PTreasuryDatum
        :--> PBuiltinList (PAsData (PTuple PDatumHash PDatum))
        :--> PDatumHash
    )
getTrDatumHash = plam $ \d l -> P.do
  let t = phead #$ pfilter # (matchDatums # d) # l
  pfield @"_0" # t
  where
    matchDatums ::
      Term
        s
        ( PAsData PTreasuryDatum
            :--> PAsData (PTuple PDatumHash PDatum)
            :--> PBool
        )
    matchDatums = plam $ \d t' ->
      let t = pfield @"_1" # t'
       in pforgetData d #== pforgetData t

-- | Get the "resolved" field of a TxInInfo.
toResolved :: Term s (PAsData PTxInInfo :--> PAsData PTxOut)
toResolved = plam $ \txIn -> pfield @"resolved" # txIn

-- | Gets the value kept at a given datum hash.
getValAtDHash ::
  Term
    s
    ( PDatumHash
        :--> PBuiltinList (PAsData PTxOut)
        :--> PAsData PValue
    )
getValAtDHash = plam $ \dh outs -> P.do
  let matchingOut = phead #$ pfilter # (matchHashes # dh) # outs
  pfield @"value" # matchingOut
  where
    matchHashes :: Term s (PDatumHash :--> PAsData PTxOut :--> PBool)
    matchHashes = plam $ \dh out' -> P.do
      PDJust dh' <- pmatch $ pfield @"datumHash" # pfromData out'
      dh #== pfield @"_0" # dh'

{- | Plutarch level type representing datum of the treasury.
     Contains:

       - @stateThread@ representing the asset class of the
         treasury's state thread token.
-}
newtype PTreasuryDatum (s :: S)
  = PTreasuryDatum
      ( Term
          s
          ( PDataRecord
              '[ "stateThread" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PTreasuryDatum

{- | Plutarch level type representing valid redeemers of the
     treasury.
-}
data PTreasuryRedeemer (s :: S)
  = -- | Receive funds and place them in the treasury.
    PReceiveFunds (Term s (PDataRecord '["_0" ':= PValue]))
  | -- | Serve as a witness for any transaction. Must remain unaltered.
    PWitnessTreasury (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PTreasuryRedeemer
