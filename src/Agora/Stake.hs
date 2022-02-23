{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Vote-lockable stake UTXOs holding GT
module Agora.Stake (
  PStakeDatum (..),
  PStakeAction (..),
  Stake (..),
  stakePolicy,
  stakeValidator,
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

import Plutarch (popaque)
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

data PStakeAction (gt :: MoneyClass) (s :: S)
  = -- | Deposit or withdraw a discrete amount of the staked governance token
    PDepositWithdraw (Term s (PDataRecord '["delta" ':= Discrete gt]))
  | -- | Destroy a stake, retrieving its LQ, the minimum ADA and any other assets
    PDestroy (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances (PStakeAction gt)

newtype PStakeDatum (gt :: MoneyClass) (s :: S) = PStakeDatum
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
    via (PIsDataReprInstances (PStakeDatum gt))

--------------------------------------------------------------------------------
--
-- What this Policy does
--
-- For minting:
--   Check that exactly 1 state thread is minted
--   Check that an output exists with a state thread and a valid datum
--   Check that no state thread is an input
--   assert TokenName == ValidatorHash of the script that we pay to
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
  Stake gt ->
  Term s PMintingPolicy
stakePolicy _stake =
  plam $ \_redeemer ctx' -> P.do
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    txInfo' <- plet ctx.txInfo
    txInfo <- pletFields @'["mint", "inputs", "outputs"] txInfo'

    PMinting ownSymbol' <- pmatch $ pfromData ctx.purpose
    ownSymbol <- plet $ pfield @"_0" # ownSymbol'
    spentST <- plet $ psymbolValueOf # ownSymbol #$ pvalueSpent # pfromData txInfo'
    mintedST <- plet $ psymbolValueOf # ownSymbol # txInfo.mint

    let burning = P.do
          passert "ST at inputs must be 1" $
            spentST #== 1

          passert "ST burned" $
            mintedST #== -1

          passert "An unlocked input existed containing an ST" $
            anyInput @(PStakeDatum gt) # pfromData txInfo'
              #$ plam
              $ \value _ stakeDatum' -> P.do
                let hasST = psymbolValueOf # ownSymbol # value #== 1
                let unlocked = pnot # (stakeLocked # stakeDatum')
                hasST #&& unlocked

          popaque (pconstant ())

    let minting = P.do
          passert "ST at inputs must be 0" $
            spentST #== 0

          passert "Minted ST must be exactly 1" $
            mintedST #== 1

          passert "A UTXO must exist with the correct output" $
            anyOutput @(PStakeDatum gt) # pfromData txInfo'
              #$ plam
              $ \value address stakeDatum' -> P.do
                let cred = pfield @"credential" # address
                pmatch cred $ \case
                  -- Should pay to a script address
                  PPubKeyCredential _ -> pcon PFalse
                  PScriptCredential validatorHash' -> P.do
                    validatorHash <- pletFields @'["_0"] validatorHash'
                    stakeDatum <- pletFields @'["owner", "stakedAmount"] stakeDatum'
                    let stValue =
                          psingletonValue
                            # ownSymbol
                            -- This coerce is safe because the structure
                            -- of PValidatorHash is the same as PTokenName.
                            # punsafeCoerce validatorHash._0
                            # 1
                    let expectedValue =
                          paddValue
                            # (discreteValue # stakeDatum.stakedAmount)
                            # stValue
                    let ownerSignsTransaction = ptxSignedBy # ctx.txInfo # stakeDatum.owner

                    -- TODO: Needs to be >=, rather than ==
                    let valueCorrect = pdata value #== pdata expectedValue
                    ownerSignsTransaction #&& valueCorrect

          popaque (pconstant ())

    pif (0 #< mintedST) minting burning

--------------------------------------------------------------------------------
stakeValidator ::
  forall (gt :: MoneyClass) ac n scale s.
  ( KnownSymbol ac
  , KnownSymbol n
  , gt ~ '(ac, n, scale)
  ) =>
  Stake gt ->
  Term s PValidator
stakeValidator stake =
  plam $ \datum redeemer ctx' -> P.do
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    txInfo' <- plet ctx.txInfo
    txInfo <- pletFields @'["mint", "inputs", "outputs"] txInfo'
    let stakeAction = punsafeCoerce redeemer :: Term s (PStakeAction gt)
    let stakeDatum' = punsafeCoerce datum :: Term s (PStakeDatum gt)
    stakeDatum <- pletFields @'["owner", "stakedAmount"] stakeDatum'

    PSpending txOutRef <- pmatch $ pfromData ctx.purpose

    PJust txInInfo <- pmatch $ pfindTxInByTxOutRef # (pfield @"_0" # txOutRef) # txInfo'
    ownAddress <- plet $ pfield @"address" #$ pfield @"resolved" # txInInfo
    let continuingValue = pfield @"value" #$ pfield @"resolved" # txInInfo
    stCurrencySymbol <- plet $ pconstant $ mintingPolicySymbol $ mkMintingPolicy (stakePolicy stake)
    mintedST <- plet $ psymbolValueOf # stCurrencySymbol # txInfo.mint
    spentST <- plet $ psymbolValueOf # stCurrencySymbol #$ pvalueSpent # txInfo'

    pmatch stakeAction $ \case
      PDestroy _ -> P.do
        passert "ST at inputs must be 1" $
          spentST #== 1
        passert "Should burn ST" $
          mintedST #== -1
        passert "Stake unlocked" $
          pnot #$ stakeLocked # stakeDatum'
        popaque (pconstant ())
      PDepositWithdraw r -> P.do
        passert "ST at inputs must be 1" $
          spentST #== 1
        passert "Stake unlocked" $
          pnot #$ stakeLocked # stakeDatum'
        passert "A UTXO must exist with the correct output" $
          anyOutput @(PStakeDatum gt) # txInfo'
            #$ plam
            $ \value address newStakeDatum' -> P.do
              newStakeDatum <- pletFields @'["owner", "stakedAmount"] newStakeDatum'
              delta <- plet $ pfield @"delta" # r
              let isScriptAddress = pdata address #== ownAddress
              let correctOutputDatum =
                    stakeDatum.owner #== newStakeDatum.owner
                      #&& (paddDiscrete # stakeDatum.stakedAmount # delta) #== newStakeDatum.stakedAmount
              let expectedValue = paddValue # continuingValue # (discreteValue # delta)

              -- TODO: As above, needs to be >=, rather than ==
              let correctValue = pdata value #== pdata expectedValue
              isScriptAddress #&& correctOutputDatum #&& correctValue

        popaque (pconstant ())

--------------------------------------------------------------------------------

-- | Check whether a Stake is locked. If it is locked, various actions are unavailable.
stakeLocked :: forall (gt :: MoneyClass) s. Term s (PStakeDatum gt :--> PBool)
stakeLocked = phoistAcyclic $
  plam $ \_stakeDatum ->
    -- TODO: when we extend this to support proposals, this will need to do something
    pcon PFalse
