{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Agora.Stake
Maintainer : emi@haskell.fyi
Description: Vote-lockable stake UTXOs holding GT.

Vote-lockable stake UTXOs holding GT.
-}
module Agora.Stake (
  PStakeDatum (..),
  PStakeRedeemer (..),
  StakeDatum (..),
  StakeRedeemer (..),
  Stake (..),
  stakePolicy,
  stakeValidator,
  stakeLocked,
) where

--------------------------------------------------------------------------------

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))
import Prelude

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Api (PubKeyHash)
import PlutusTx qualified

--------------------------------------------------------------------------------

import Plutarch (popaque)
import Plutarch.Api.V1 (
  PCredential (PPubKeyCredential, PScriptCredential),
  PMintingPolicy,
  PPubKeyHash,
  PScriptPurpose (PMinting, PSpending),
  PTokenName,
  PValidator,
  mintingPolicySymbol,
  mkMintingPolicy,
 )
import Plutarch.DataRepr (
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Internal (punsafeCoerce)
import Plutarch.Monadic qualified as P
import Plutus.V1.Ledger.Value (AssetClass (AssetClass))

--------------------------------------------------------------------------------

import Agora.SafeMoney (
  AssetClassRef (..),
  GTTag,
  PDiscrete,
  paddDiscrete,
  pdiscreteValue,
  pgeqDiscrete,
  pzeroDiscrete,
 )
import Agora.Utils (
  anyInput,
  anyOutput,
  paddValue,
  passert,
  pfindTxInByTxOutRef,
  pgeqByClass,
  pgeqByClass',
  pgeqBySymbol,
  psingletonValue,
  psymbolValueOf,
  ptxSignedBy,
  pvalueSpent,
 )

--------------------------------------------------------------------------------

-- | Parameters for creating Stake scripts.
data Stake = Stake
  { gtClassRef :: AssetClassRef GTTag
  -- ^ Resolve governance token
  }

-- | Plutarch-level redeemer for Stake scripts.
data PStakeRedeemer (s :: S)
  = -- | Deposit or withdraw a discrete amount of the staked governance token.
    PDepositWithdraw (Term s (PDataRecord '["delta" ':= PDiscrete GTTag]))
  | -- | Destroy a stake, retrieving its LQ, the minimum ADA and any other assets.
    PDestroy (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PStakeRedeemer

-- | Haskell-level redeemer for Stake scripts.
data StakeRedeemer
  = -- | Deposit or withdraw a discrete amount of the staked governance token.
    DepositWithdraw Integer
  | -- | Destroy a stake, retrieving its LQ, the minimum ADA and any other assets.
    Destroy
  deriving stock (Show, GHC.Generic)

PlutusTx.makeIsDataIndexed ''StakeRedeemer [('DepositWithdraw, 0), ('Destroy, 1)]

-- | Plutarch-level datum for Stake scripts.
newtype PStakeDatum (s :: S) = PStakeDatum
  { getStakeDatum ::
    Term s (PDataRecord '["stakedAmount" ':= PDiscrete GTTag, "owner" ':= PPubKeyHash])
  }
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via (PIsDataReprInstances PStakeDatum)

-- | Haskell-level datum for Stake scripts.
data StakeDatum = StakeDatum
  { -- FIXME: This needs to be gt
  stakedAmount :: Integer
  , owner :: PubKeyHash
  }
  deriving stock (Show, GHC.Generic)

PlutusTx.makeIsDataIndexed ''StakeDatum [('StakeDatum, 0)]

--------------------------------------------------------------------------------
{- What this Policy does

   For minting:
     Check that exactly one state thread is minted
     Check that an output exists with a state thread and a valid datum
     Check that no state thread is an input
     assert TokenName == ValidatorHash of the script that we pay to

   For burning:
     Check that exactly one state thread is burned
     Check that datum at state thread is valid and not locked
-}
--------------------------------------------------------------------------------

-- | Policy for Stake state threads.
stakePolicy ::
  forall (s :: S).
  Stake ->
  Term s PMintingPolicy
stakePolicy stake =
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
            anyInput @PStakeDatum # pfromData txInfo'
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
            anyOutput @PStakeDatum # pfromData txInfo'
              #$ plam
              $ \value address stakeDatum' -> P.do
                let cred = pfield @"credential" # address
                pmatch cred $ \case
                  -- Should pay to a script address
                  PPubKeyCredential _ -> pcon PFalse
                  PScriptCredential validatorHash' -> P.do
                    validatorHash <- pletFields @'["_0"] validatorHash'
                    stakeDatum <- pletFields @'["owner", "stakedAmount"] stakeDatum'

                    -- TODO: figure out why this is required :/ (specifically, why `validatorHash._0` is `PData`)
                    tn <- plet (pfromData (punsafeCoerce validatorHash._0 :: Term _ (PAsData PTokenName)))

                    let stValue =
                          psingletonValue
                            # ownSymbol
                            -- This coerce is safe because the structure
                            -- of PValidatorHash is the same as PTokenName.
                            # tn
                            # 1
                    let expectedValue =
                          paddValue
                            # (pdiscreteValue stake.gtClassRef # stakeDatum.stakedAmount)
                            # stValue
                    let ownerSignsTransaction =
                          ptxSignedBy
                            # ctx.txInfo
                            # stakeDatum.owner

                    -- TODO: This is quite inefficient now, as it does two lookups
                    -- instead of a more efficient single pass,
                    -- but it doesn't really matter for this. At least it's correct.
                    let valueCorrect =
                          foldr1
                            (#&&)
                            [ pgeqByClass' (AssetClass ("", "")) # value # expectedValue
                            , pgeqByClass' stake.gtClassRef.getAssetClass
                                # value
                                # expectedValue
                            , pgeqByClass
                                # ownSymbol
                                # tn
                                # value
                                # expectedValue
                            ]

                    ownerSignsTransaction
                      #&& valueCorrect
          popaque (pconstant ())

    pif (0 #< mintedST) minting burning

--------------------------------------------------------------------------------

-- | Validator intended for Stake UTXOs to live in.
stakeValidator ::
  forall (s :: S).
  Stake ->
  Term s PValidator
stakeValidator stake =
  plam $ \datum redeemer ctx' -> P.do
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    txInfo' <- plet ctx.txInfo
    txInfo <- pletFields @'["mint", "inputs", "outputs"] txInfo'

    -- Coercion is safe in that if coercion fails we crash hard.
    let stakeRedeemer :: Term _ PStakeRedeemer
        stakeRedeemer = pfromData $ punsafeCoerce redeemer
        stakeDatum' :: Term _ PStakeDatum
        stakeDatum' = pfromData $ punsafeCoerce datum
    stakeDatum <- pletFields @'["owner", "stakedAmount"] stakeDatum'

    PSpending txOutRef <- pmatch $ pfromData ctx.purpose

    PJust txInInfo <- pmatch $ pfindTxInByTxOutRef # (pfield @"_0" # txOutRef) # txInfo'
    ownAddress <- plet $ pfield @"address" #$ pfield @"resolved" # txInInfo
    let continuingValue = pfield @"value" #$ pfield @"resolved" # txInInfo
    ownerSignsTransaction <- plet $ ptxSignedBy # ctx.txInfo # stakeDatum.owner
    stCurrencySymbol <- plet $ pconstant $ mintingPolicySymbol $ mkMintingPolicy (stakePolicy stake)
    mintedST <- plet $ psymbolValueOf # stCurrencySymbol # txInfo.mint
    spentST <- plet $ psymbolValueOf # stCurrencySymbol #$ pvalueSpent # txInfo'

    pmatch stakeRedeemer $ \case
      PDestroy _ -> P.do
        passert "ST at inputs must be 1" $
          spentST #== 1
        passert "Should burn ST" $
          mintedST #== -1
        passert "Stake unlocked" $
          pnot #$ stakeLocked # stakeDatum'
        passert
          "Owner signs this transaction"
          ownerSignsTransaction
        popaque (pconstant ())
      PDepositWithdraw r -> P.do
        passert "ST at inputs must be 1" $
          spentST #== 1
        passert "Stake unlocked" $
          pnot #$ stakeLocked # stakeDatum'
        passert
          "Owner signs this transaction"
          ownerSignsTransaction
        passert "A UTXO must exist with the correct output" $
          anyOutput @PStakeDatum # txInfo'
            #$ plam
            $ \value address newStakeDatum' -> P.do
              newStakeDatum <- pletFields @'["owner", "stakedAmount"] newStakeDatum'
              delta <- plet $ pfield @"delta" # r
              let isScriptAddress = pdata address #== ownAddress
              let correctOutputDatum =
                    foldr1
                      (#&&)
                      [ stakeDatum.owner #== newStakeDatum.owner
                      , (paddDiscrete # stakeDatum.stakedAmount # delta) #== newStakeDatum.stakedAmount
                      , -- We can't magically conjure GT anyway (no input to spend!)
                        -- do we need to check this, really?
                        pgeqDiscrete # (pfromData newStakeDatum.stakedAmount) # pzeroDiscrete
                      ]
              let expectedValue = paddValue # continuingValue # (pdiscreteValue stake.gtClassRef # delta)

              -- TODO: Same as above. This is quite inefficient now, as it does two lookups
              -- instead of a more efficient single pass,
              -- but it doesn't really matter for this. At least it's correct.
              let valueCorrect =
                    foldr1
                      (#&&)
                      [ pgeqByClass' (AssetClass ("", "")) # value # expectedValue
                      , pgeqByClass' stake.gtClassRef.getAssetClass
                          # value
                          # expectedValue
                      , pgeqBySymbol
                          # stCurrencySymbol
                          # value
                          # expectedValue
                      ]

              foldr1
                (#&&)
                [ ptraceIfFalse "isScriptAddress" isScriptAddress
                , ptraceIfFalse "correctOutputDatum" correctOutputDatum
                , ptraceIfFalse "valueCorrect" valueCorrect
                ]

        popaque (pconstant ())

--------------------------------------------------------------------------------

-- | Check whether a Stake is locked. If it is locked, various actions are unavailable.
stakeLocked :: forall (s :: S). Term s (PStakeDatum :--> PBool)
stakeLocked = phoistAcyclic $
  plam $ \_stakeDatum ->
    -- TODO: when we extend this to support proposals, this will need to do something
    pcon PFalse
