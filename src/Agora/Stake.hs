{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Vote-lockable stake UTXOs holding GT
module Agora.Stake (
  StakeDatum (..),
  StakeAction (..),
  Stake (..),
  stakePolicy,
) where

--------------------------------------------------------------------------------

import GHC.Generics qualified as GHC
import GHC.TypeLits (
  KnownSymbol,
 )
import Generics.SOP (Generic, I (I))
import Prelude

--------------------------------------------------------------------------------

import Plutarch.Api.V1
import Plutarch.DataRepr (
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Internal
import Plutarch.Monadic qualified as P

--------------------------------------------------------------------------------

import Agora.SafeMoney
import Agora.Utils

--------------------------------------------------------------------------------

data Stake (gt :: MoneyClass) = Stake

data StakeAction (gt :: MoneyClass) (s :: S)
  = -- | Deposit or withdraw a discrete amount of the staked governance token
    DepositWithdraw (Term s (PDataRecord '["delta" ':= Discrete gt]))
  | -- | Destroy a stake, retrieving its LQ, the minimum ADA and any other assets
    Destroy (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances (StakeAction gt)

newtype StakeDatum (gt :: MoneyClass) (s :: S) = StakeDatum
  { getStakeDatum ::
    ( Term
        s
        ( PDataRecord
            '[ "stakedAmount" ':= Discrete gt
             , "owner" ':= PPubKeyHash
             ]
        )
    )
  }
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via (PIsDataReprInstances (StakeDatum gt))

-- | Check if any output matches the predicate
anyOutput ::
  forall (datum :: PType) s.
  ( PIsData datum
  ) =>
  Term s (PTxInfo :--> (PValue :--> datum :--> PBool) :--> PBool)
anyOutput = phoistAcyclic $
  plam $ \txInfo' predicate -> P.do
    txInfo <- pletFields @'["outputs"] txInfo'
    pany
      # ( plam $ \txOut'' -> P.do
            PTxOut txOut' <- pmatch (pfromData txOut'')
            txOut <- pletFields @'["value", "datumHash"] txOut'
            PDJust dh <- pmatch txOut.datumHash
            pmatch (pfindDatum' @datum # (pfield @"_0" # dh) # txInfo') $ \case
              PJust datum -> P.do
                predicate # txOut.value # pfromData datum
              PNothing -> pcon PFalse
        )
      # pfromData txInfo.outputs

stakePolicy ::
  forall (gt :: MoneyClass) ac n scale s.
  ( KnownSymbol ac
  , KnownSymbol n
  , gt ~ '(ac, n, scale)
  ) =>
  Stake
    gt ->
  Term s (PData :--> PScriptContext :--> PUnit)
stakePolicy _stake =
  plam $ \_redeemer ctx'' -> P.do
    PScriptContext ctx' <- pmatch ctx''
    ctx <- pletFields @'["txInfo", "purpose"] ctx'

    PMinting ownSymbol <- pmatch $ pfromData ctx.purpose
    -- TODO: add this to 'valueCorrect'
    let stValue = psingletonValue # (pfield @"_0" # ownSymbol) # pconstant "ST" # 1

    passert "A UTXO must exist with the correct output" $
      anyOutput @(StakeDatum gt) # pfromData ctx.txInfo
        # ( plam $ \value stakeDatum' -> P.do
              stakeDatum <- pletFields @'["owner", "stakedAmount"] stakeDatum'
              let ownerSignsTransaction = ptxSignedBy # ctx.txInfo # stakeDatum.owner
              let valueCorrect = pdata value #== pdata (paddValue # (discreteValue # stakeDatum.stakedAmount) # stValue)
              ownerSignsTransaction #&& valueCorrect
          )

    pconstant ()
