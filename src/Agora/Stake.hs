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

--------------------------------------------------------------------------------
--
-- What this Policy does
--
-- - Check that exactly 1 state thread is minted
-- - Check that an output exists with a state thread and a valid datum
-- - Check that no state thread is an input
--
-- Question:
--
--------------------------------------------------------------------------------
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
    txInfo' <- plet ctx.txInfo
    txInfo <- pletFields @'["mint", "inputs"] txInfo'

    PMinting ownSymbol' <- pmatch $ pfromData ctx.purpose
    ownSymbol <- plet $ pfield @"_0" # ownSymbol'
    let stValue = psingletonValue # ownSymbol # pconstant "ST" # 1

    passert "ST at inputs must be 0" $
      (passetClassValueOf # ownSymbol # pconstant "ST" # (pvalueSpent # pfromData txInfo')) #== 0

    passert "Minted ST must be exactly 1" $
      pdata txInfo.mint #== pdata stValue

    passert "A UTXO must exist with the correct output" $
      anyOutput @(StakeDatum gt) # pfromData txInfo'
        # ( plam $ \value stakeDatum' -> P.do
              stakeDatum <- pletFields @'["owner", "stakedAmount"] stakeDatum'
              let expectedValue = paddValue # (discreteValue # stakeDatum.stakedAmount) # stValue
              let ownerSignsTransaction = ptxSignedBy # ctx.txInfo # stakeDatum.owner
              let valueCorrect = pdata value #== pdata expectedValue
              ownerSignsTransaction #&& valueCorrect
          )

    pconstant ()
