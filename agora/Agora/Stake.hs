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
  ProposalLock (..),
  PProposalLock (..),
  Stake (..),
  stakePolicy,
  stakeValidator,
  stakeLocked,
) where

--------------------------------------------------------------------------------

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))
import Prelude hiding (Num (..))

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Api (PubKeyHash)
import PlutusTx qualified

--------------------------------------------------------------------------------

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
  DerivePConstantViaData (..),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Internal (punsafeCoerce)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Monadic qualified as P
import Plutus.V1.Ledger.Value (AssetClass (AssetClass))

--------------------------------------------------------------------------------

import Agora.Proposal (PProposalId, PResultTag, ProposalId (..), ResultTag (..))
import Agora.SafeMoney (GTTag)
import Agora.Utils (
  anyInput,
  anyOutput,
  paddValue,
  passert,
  pfindTxInByTxOutRef,
  pgeqByClass,
  pgeqByClass',
  pgeqBySymbol,
  pnotNull,
  psingletonValue,
  psymbolValueOf,
  ptxSignedBy,
  pvalueSpent,
 )
import Plutarch.Numeric
import Plutarch.SafeMoney (
  PDiscrete,
  Tagged (..),
  pdiscreteValue',
  untag,
 )
import Plutarch.TryFrom (PTryFrom, ptryFrom)

--------------------------------------------------------------------------------

-- | Parameters for creating Stake scripts.
newtype Stake = Stake
  { gtClassRef :: Tagged GTTag AssetClass
  -- ^ Used when inlining the AssetClass of a 'PDiscrete' in the script code.
  }

{- | A lock placed on a Stake datum in order to prevent
     depositing and withdrawing when votes are in place.

     NOTE: Due to retracting votes always being possible,
     this lock will only lock with contention on the proposal.

     FIXME: Contention on Proposals could create contention
     on voting which in turn creates contention on stakers.

     Vaguely this is the dependency graph for this locking
     interaction. Both the stake validator and the proposal
     validator are only able to check for one another through
     the datum belonging to the ST:

     @
     ┌─────────────────┐   ┌────────────────────┐
     │ Stake Validator ├─┐ │ Proposal Validator │
     └────────┬────────┘ │ └──────┬─────┬───────┘
              │          │        │     │
              │        ┌─┼────────┘     │
              ▼        │ │              ▼
     ┌──────────────┐  │ │ ┌─────────────────┐
     │ Stake Policy │◄─┘ └►│ Proposal Policy │
     └──────────────┘      └─────────────────┘
     @
-}
data ProposalLock = ProposalLock
  { vote :: ResultTag
  -- ^ What was voted on. This allows retracting votes to
  --   undo their vote.
  , proposalId :: ProposalId
  -- ^ Identifies the proposal. See 'ProposalId' for further
  -- comments on its significance.
  }
  deriving stock (Show, GHC.Generic)

PlutusTx.makeIsDataIndexed ''ProposalLock [('ProposalLock, 0)]

-- | Haskell-level redeemer for Stake scripts.
data StakeRedeemer
  = -- | Deposit or withdraw a discrete amount of the staked governance token.
    --   Stake must be unlocked.
    DepositWithdraw (Tagged GTTag Integer)
  | -- | Destroy a stake, retrieving its LQ, the minimum ADA and any other assets.
    --   Stake must be unlocked.
    Destroy
  | -- | Permit a Vote to be added onto a 'Proposal'.
    --   This also adds a lock to the 'lockedBy' field. See 'ProposalLock'.
    --   This needs to be done in sync with casting a vote, otherwise
    --   it's possible for a lock to be permanently placed on the stake,
    --   and then the funds are lost.
    PermitVote ProposalLock
  | -- | Retract a vote, removing it from the 'lockedBy' field. See 'ProposalLock'.
    --   This action checks for permission of the 'Proposal'. Finished proposals are
    --   always allowed to have votes retracted and won't affect the Proposal datum,
    --   allowing 'Stake's to be unlocked.
    RetractVotes [ProposalLock]
  deriving stock (Show, GHC.Generic)

PlutusTx.makeIsDataIndexed
  ''StakeRedeemer
  [ ('DepositWithdraw, 0)
  , ('Destroy, 1)
  , ('PermitVote, 2)
  , ('RetractVotes, 3)
  ]

-- | Haskell-level datum for Stake scripts.
data StakeDatum = StakeDatum
  { stakedAmount :: Tagged GTTag Integer
  -- ^ Tracks the amount of governance token staked in the datum.
  -- This also acts as the voting weight for 'Proposal's.
  , owner :: PubKeyHash
  -- ^ The hash of the public key this stake belongs to.
  --
  -- TODO Support for MultiSig/Scripts is tracked here:
  --      https://github.com/Liqwid-Labs/agora/issues/45
  , lockedBy :: [ProposalLock]
  -- ^ The current proposals locking this stake. This field must be empty
  -- for the stake to be usable for deposits and withdrawals.
  }
  deriving stock (Show, GHC.Generic)

PlutusTx.makeIsDataIndexed ''StakeDatum [('StakeDatum, 0)]

--------------------------------------------------------------------------------

-- | Plutarch-level datum for Stake scripts.
newtype PStakeDatum (s :: S) = PStakeDatum
  { getStakeDatum ::
    Term
      s
      ( PDataRecord
          '[ "stakedAmount" ':= PDiscrete GTTag
           , "owner" ':= PPubKeyHash
           , "lockedBy" ':= PBuiltinList (PAsData PProposalLock)
           ]
      )
  }
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via (PIsDataReprInstances PStakeDatum)

instance PUnsafeLiftDecl PStakeDatum where type PLifted PStakeDatum = StakeDatum
deriving via (DerivePConstantViaData StakeDatum PStakeDatum) instance (PConstantDecl StakeDatum)

-- | Plutarch-level redeemer for Stake scripts.
data PStakeRedeemer (s :: S)
  = -- | Deposit or withdraw a discrete amount of the staked governance token.
    PDepositWithdraw (Term s (PDataRecord '["delta" ':= PDiscrete GTTag]))
  | -- | Destroy a stake, retrieving its LQ, the minimum ADA and any other assets.
    PDestroy (Term s (PDataRecord '[]))
  | PPermitVote (Term s (PDataRecord '["lock" ':= PProposalLock]))
  | PRetractVotes (Term s (PDataRecord '["locks" ':= PBuiltinList (PAsData PProposalLock)]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PStakeRedeemer

deriving via
  PAsData (PIsDataReprInstances PStakeRedeemer)
  instance
    PTryFrom PData (PAsData PStakeRedeemer)

instance PUnsafeLiftDecl PStakeRedeemer where type PLifted PStakeRedeemer = StakeRedeemer
deriving via (DerivePConstantViaData StakeRedeemer PStakeRedeemer) instance (PConstantDecl StakeRedeemer)

newtype PProposalLock (s :: S) = PProposalLock
  { getProposalLock ::
    Term
      s
      ( PDataRecord
          '[ "vote" ':= PResultTag
           , "proposalTag" ':= PProposalId
           ]
      )
  }
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via (PIsDataReprInstances PProposalLock)

deriving via
  PAsData (PIsDataReprInstances PProposalLock)
  instance
    PTryFrom PData (PAsData PProposalLock)

instance PUnsafeLiftDecl PProposalLock where type PLifted PProposalLock = ProposalLock
deriving via (DerivePConstantViaData ProposalLock PProposalLock) instance (PConstantDecl ProposalLock)

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
stakePolicy :: Stake -> ClosedTerm PMintingPolicy
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
                            # (pdiscreteValue' stake.gtClassRef # stakeDatum.stakedAmount)
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
                            , pgeqByClass' (untag stake.gtClassRef)
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
stakeValidator :: Stake -> ClosedTerm PValidator
stakeValidator stake =
  plam $ \datum redeemer ctx' -> P.do
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    txInfo' <- plet ctx.txInfo
    txInfo <- pletFields @'["mint", "inputs", "outputs"] txInfo'

    (pfromData -> stakeRedeemer, _) <- ptryFrom redeemer

    -- TODO: Use PTryFrom
    let stakeDatum' :: Term _ PStakeDatum
        stakeDatum' = pfromData $ punsafeCoerce datum
    stakeDatum <- pletFields @'["owner", "stakedAmount"] stakeDatum'

    PSpending txOutRef <- pmatch $ pfromData ctx.purpose

    PJust txInInfo <- pmatch $ pfindTxInByTxOutRef # (pfield @"_0" # txOutRef) # txInfo'
    ownAddress <- plet $ pfield @"address" #$ pfield @"resolved" # txInInfo
    let continuingValue = pfield @"value" #$ pfield @"resolved" # txInInfo

    -- Whether the owner signs this transaction or not.
    ownerSignsTransaction <- plet $ ptxSignedBy # ctx.txInfo # stakeDatum.owner

    stCurrencySymbol <- plet $ pconstant $ mintingPolicySymbol $ mkMintingPolicy (stakePolicy stake)
    mintedST <- plet $ psymbolValueOf # stCurrencySymbol # txInfo.mint
    spentST <- plet $ psymbolValueOf # stCurrencySymbol #$ pvalueSpent # txInfo'

    -- Is the stake currently locked?
    stakeIsLocked <- plet $ stakeLocked # stakeDatum'

    pmatch stakeRedeemer $ \case
      PDestroy _ -> P.do
        passert "ST at inputs must be 1" $
          spentST #== 1
        passert "Should burn ST" $
          mintedST #== -1
        passert "Stake unlocked" $ pnot # stakeIsLocked
        passert
          "Owner signs this transaction"
          ownerSignsTransaction
        popaque (pconstant ())
      --------------------------------------------------------------------------
      PRetractVotes _ -> P.do
        passert
          "Owner signs this transaction"
          ownerSignsTransaction
        -- TODO: check proposal constraints
        popaque (pconstant ())
      --------------------------------------------------------------------------
      PPermitVote _ -> P.do
        passert
          "Owner signs this transaction"
          ownerSignsTransaction
        -- TODO: check proposal constraints
        popaque (pconstant ())
      --------------------------------------------------------------------------
      PDepositWithdraw r -> P.do
        passert "ST at inputs must be 1" $
          spentST #== 1
        passert "Stake unlocked" $
          pnot #$ stakeIsLocked
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
                      , (stakeDatum.stakedAmount + delta) #== newStakeDatum.stakedAmount
                      , -- We can't magically conjure GT anyway (no input to spend!)
                        -- do we need to check this, really?
                        zero #<= pfromData newStakeDatum.stakedAmount
                      ]
              let expectedValue = paddValue # continuingValue # (pdiscreteValue' stake.gtClassRef # delta)

              -- TODO: Same as above. This is quite inefficient now, as it does two lookups
              -- instead of a more efficient single pass,
              -- but it doesn't really matter for this. At least it's correct.
              let valueCorrect =
                    foldr1
                      (#&&)
                      [ pgeqByClass' (AssetClass ("", "")) # value # expectedValue
                      , pgeqByClass' (untag stake.gtClassRef)
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
  plam $ \stakeDatum ->
    let locks :: Term _ (PBuiltinList (PAsData PProposalLock))
        locks = pfield @"lockedBy" # stakeDatum
     in pnotNull # locks
