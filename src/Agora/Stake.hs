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

-- | Assert a particular bool, trace on falsehood. Use in monadic context
passert :: Term s PString -> Term s PBool -> Term s k -> Term s k
passert errorMessage check k = pif check k (ptraceError errorMessage)

-- | Find a datum with the given hash.
pfindDatum :: Term s (PDatumHash :--> PTxInfo :--> PMaybe PDatum)
pfindDatum = phoistAcyclic $
  plam $ \datumHash txInfo'' -> P.do
    PTxInfo txInfo' <- pmatch txInfo''
    plookupTuple # datumHash #$ pfield @"data" # txInfo'

-- | Find a datum with the given hash. NOTE: this is unsafe in the sense that, if the data layout is wrong, this is UB.
pfindDatum' :: PIsData a => Term s (PDatumHash :--> PTxInfo :--> PMaybe (PAsData a))
pfindDatum' = phoistAcyclic $ plam $ \dh x -> punsafeCoerce $ pfindDatum # dh # x

-- | Check if a PubKeyHash signs this transaction
ptxSignedBy :: Term s (PTxInfo :--> PAsData PPubKeyHash :--> PBool)
ptxSignedBy = phoistAcyclic $
  plam $ \txInfo' pkh -> P.do
    txInfo <- pletFields @'["signatories"] txInfo'
    pelem @PBuiltinList # pkh # txInfo.signatories

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
    let _stValue = psingletonValue # (pfield @"_0" # ownSymbol) # pconstant "ST" # 1

    passert "A UTXO must exist with the correct output" $
      anyOutput @(StakeDatum gt) # pfromData ctx.txInfo
        # ( plam $ \value stakeDatum' -> P.do
              stakeDatum <- pletFields @'["owner", "stakedAmount"] stakeDatum'
              let ownerSignsTransaction = ptxSignedBy # ctx.txInfo # stakeDatum.owner
              let valueCorrect = pdata value #== pdata (discreteValue # stakeDatum.stakedAmount)
              ownerSignsTransaction #&& valueCorrect
          )

    pconstant ()
