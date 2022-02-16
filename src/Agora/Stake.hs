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

passert :: Term s PString -> Term s PBool -> Term s k -> Term s k
passert errorMessage check k = pif check k (ptraceError errorMessage)

stakePolicy ::
  forall (gt :: MoneyClass) s.
  Stake gt ->
  Term s (PData :--> PScriptContext :--> PUnit)
stakePolicy _stake =
  plam $ \_redeemer ctx -> P.do
    PScriptContext ctx' <- pmatch ctx
    ctx'' <- pletFields @'["txInfo", "purpose"] ctx'
    PTxInfo txInfo <- pmatch $ pfromData (hrecField @"txInfo" ctx'')
    txInfo' <- pletFields @'["signatories", "outputs"] txInfo
    let outputs = hrecField @"outputs" txInfo'

    passert "Created stake must be owned by a signer of this transaction" $
      pany
        # ( plam $ \txOut -> P.do
              PTxOut txOut' <- pmatch (pfromData txOut)
              _txOut'' <- pletFields @'["value", "datumHash"] txOut'
              pcon PTrue
          )
        # outputs

    pcon PUnit
