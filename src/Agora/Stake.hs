{-# LANGUAGE PolyKinds #-}

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
import Plutarch.Prelude

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

assert :: Term s PString -> Term s PBool -> TermCont @r s ()
assert errorMessage check = TermCont $ \k -> pif check (k ()) (ptraceError errorMessage)

pfindDatum :: Term s (PDatumHash :--> PTxInfo :--> PMaybe PDatum)
pfindDatum = phoistAcyclic $
  plam $ \_datumHash _txInfo -> unTermCont $ do
    pure (pcon PNothing)

stakePolicy ::
  forall (gt :: MoneyClass) s.
  Stake gt ->
  Term s (PData :--> PScriptContext :--> PUnit)
stakePolicy _stake =
  plam $ \_redeemer ctx -> unTermCont $ do
    PScriptContext ctx' <- tcont $ pmatch ctx
    ctx'' <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
    PTxInfo txInfo <- tcont $ pmatch $ pfromData (hrecField @"txInfo" ctx'')
    txInfo' <- tcont $ pletFields @'["signatories", "outputs"] txInfo
    let outputs = hrecField @"outputs" txInfo'

    assert "Created stake must be owned by a signer of this transaction" $
      pany
        # ( plam $ \txOut -> unTermCont $ do
              PTxOut txOut' <- tcont $ pmatch (pfromData txOut)
              _txOut'' <- tcont $ pletFields @'["value", "datumHash"] txOut'
              pure (pcon PTrue)
          )
        # outputs

    pure (pcon PUnit)
