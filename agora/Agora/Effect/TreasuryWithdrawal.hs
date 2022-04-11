{- |
Module     : Agora.Effect.TreasuryWithdrawal
Maintainer : seungheon.ooh@gmail.com
Description: An Effect that withdraws treasury deposit
-}
module Agora.Effect.TreasuryWithdrawal (PTreasuryWithdrawalDatum, treasuryWithdrawalValidator) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

import Agora.Effect (makeEffect)
import Agora.Utils (passert)
import Plutarch (popaque)
import Plutarch.Api.V1 (
  PCredential,
  PTuple,
  PValidator,
  PValue,
  ptuple,
 )
import Plutarch.DataRepr (PDataFields, PIsDataReprInstances (..))
import Plutarch.Monadic qualified as P
import Plutus.V1.Ledger.Value (CurrencySymbol)

data PTreasuryWithdrawalDatum (s :: S)
  = PTreasuryWithdrawalDatum
      ( Term
          s
          ( PDataRecord
              '["receivers" ':= PBuiltinList (PAsData (PTuple PCredential PValue))]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PTreasuryWithdrawalDatum

treasuryWithdrawalValidator :: forall {s :: S}. CurrencySymbol -> Term s PValidator
treasuryWithdrawalValidator currSymbol = makeEffect currSymbol $
  \_cs (_datum :: Term _ PTreasuryWithdrawalDatum) _txOutRef _txInfo -> P.do
    let outputs =
          pmap
            # plam
              ( \_out -> P.do
                  out <- pletFields @'["address", "value"] $ pfromData _out
                  cred <- pletFields @'["credential"] $ pfromData out.address
                  pdata $ ptuple # cred.credential # out.value
              )
            #$ pfield @"outputs"
            # _txInfo
        recivers = pfromData $ pfield @"receivers" # _datum
        checkOutputs =
          pall # plam id #$ pmap
            # plam
              ( \_out -> P.do
                  pelem # _out # outputs
              )
            #$ recivers
    passert "Transaction output does not match receivers" checkOutputs
    popaque $ pconstant ()
