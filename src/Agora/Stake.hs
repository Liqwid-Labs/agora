{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Vote-lockable stake UTXOs holding GT
module Agora.Stake (
  StakeDatum (..),
  StakeAction (..),
  Stake (..),
  stakePolicy,
  stakeLocked,
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
  Term s (PTxInfo :--> (PValue :--> PAddress :--> datum :--> PBool) :--> PBool)
anyOutput = phoistAcyclic $
  plam $ \txInfo' predicate -> P.do
    txInfo <- pletFields @'["outputs"] txInfo'
    pany
      # ( plam $ \txOut'' -> P.do
            PTxOut txOut' <- pmatch (pfromData txOut'')
            txOut <- pletFields @'["value", "datumHash", "address"] txOut'
            PDJust dh <- pmatch txOut.datumHash
            pmatch (pfindDatum' @datum # (pfield @"_0" # dh) # txInfo') $ \case
              PJust datum -> P.do
                predicate # txOut.value # txOut.address # pfromData datum
              PNothing -> pcon PFalse
        )
      # pfromData txInfo.outputs

anyInput ::
  forall (datum :: PType) s.
  ( PIsData datum
  ) =>
  Term s (PTxInfo :--> (PValue :--> PAddress :--> datum :--> PBool) :--> PBool)
anyInput = phoistAcyclic $
  plam $ \txInfo' predicate -> P.do
    txInfo <- pletFields @'["inputs"] txInfo'
    pany
      # ( plam $ \txInInfo'' -> P.do
            PTxInInfo txInInfo' <- pmatch (pfromData txInInfo'')
            let txOut'' = pfield @"resolved" # txInInfo'
            PTxOut txOut' <- pmatch (pfromData txOut'')
            txOut <- pletFields @'["value", "datumHash", "address"] txOut'
            PDJust dh <- pmatch txOut.datumHash
            pmatch (pfindDatum' @datum # (pfield @"_0" # dh) # txInfo') $ \case
              PJust datum -> P.do
                predicate # txOut.value # txOut.address # pfromData datum
              PNothing -> pcon PFalse
        )
      # pfromData txInfo.inputs

--------------------------------------------------------------------------------
--
-- # What this Policy does
--
-- For minting:
--   Check that exactly 1 state thread is minted
--   Check that an output exists with a state thread and a valid datum
--   Check that no state thread is an input
--
-- FIXME: This doesn't check that it's paid to the right script address, can we?
--
--
-- For burning:
--   Check that exactly 1 state thread is burned
--   Check that datum at state thread is valid and not locked
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
    txInfo <- pletFields @'["mint", "inputs", "outputs"] txInfo'

    PMinting ownSymbol' <- pmatch $ pfromData ctx.purpose
    ownSymbol <- plet $ pfield @"_0" # ownSymbol'
    let stValue = psingletonValue # ownSymbol # pconstant "ST" # 1

    stOf <- plet $ plam $ \v -> passetClassValueOf # ownSymbol # pconstant "ST" # v
    mintedST <- plet $ stOf # txInfo.mint
    inputST <- plet $ stOf # (pvalueSpent # pfromData txInfo')

    let burning = P.do
          passert "ST at inputs must be 1" $
            inputST #== 1

          passert "ST burned" $
            mintedST #== -1

          passert "An unlocked input existed containing an ST" $
            anyInput @(StakeDatum gt) # pfromData txInfo'
              #$ plam
              $ \value _ stakeDatum' -> P.do
                let hasST = stOf # value #== 1
                let unlocked = pnot # (stakeLocked # stakeDatum')
                hasST #&& unlocked

          pconstant ()

    let minting = P.do
          passert "ST at inputs must be 0" $
            inputST #== 0

          passert "Minted ST must be exactly 1" $
            mintedST #== 1

          passert "A UTXO must exist with the correct output" $
            anyOutput @(StakeDatum gt) # pfromData txInfo'
              #$ plam
              $ \value _ stakeDatum' -> P.do
                stakeDatum <- pletFields @'["owner", "stakedAmount"] stakeDatum'
                let expectedValue = paddValue # (discreteValue # stakeDatum.stakedAmount) # stValue
                let ownerSignsTransaction = ptxSignedBy # ctx.txInfo # stakeDatum.owner
                let valueCorrect = pdata value #== pdata expectedValue -- TODO: Needs to be >=, rather than ==
                ownerSignsTransaction #&& valueCorrect

          pconstant ()

    pif (0 #< mintedST) minting burning

-- | Check whether a Stake is locked. If it is locked, various actions are unavailable.
stakeLocked :: forall (gt :: MoneyClass) s. Term s (StakeDatum gt :--> PBool)
stakeLocked = phoistAcyclic $
  plam $ \_stakeDatum ->
    -- TODO: when we extend this to support proposals, this will need to do something
    pcon PFalse
