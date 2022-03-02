{-# OPTIONS_GHC -Wwarn #-}

{- |
Module: Agora.Treasury
Maintainer: jack@mlabs.city
Description: Treasury scripts.

Contains the datum, redeemer and validator for a template DAO
treasury.
-}
module Agora.Treasury where

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
  PTxInInfo (PTxInInfo),
  PTxOut (PTxOut),
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
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  PTxInfo txInfo' <- pmatch $ pfromData ctx.txInfo
  pmatch (pfromData r) $ \case
    PWitnessTreasury _ -> P.do
      txInfo <- pletFields @'["inputs", "outputs", "data"] txInfo'

      -- inputs :: Term s ( PAsData PBuiltinList )
      let inputs = txInfo.inputs

      -- dat :: Term s (PAsData PBuiltinList)
      let dat = pfield @"data" # txInfo'

      -- dh :: Term s PDatumHash
      let dH = getTrDatumHash # d # dat

      pconstant ()

    -- Validation for receiving funds.
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
       in (pforgetData d) #== (pforgetData t)

getValAtDHash ::
  Term
    s
    ( PDatumHash
        :--> PBuiltinList (PAsData PTxOut)
        :--> PValue
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

       - @reserves@ representing the current value kept in the
         treasury.
       - @stateThread@ representing the asset class of the
         treasury's state thread token.
-}
newtype PTreasuryDatum (s :: S)
  = PTreasuryDatum
      ( Term
          s
          ( PDataRecord
              '[ "reserves" ':= PValue
               , "stateThread" ':= PCurrencySymbol
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
