{- |
Module     : Agora.Stake.Scripts
Maintainer : emi@haskell.fyi
Description: Plutus Scripts for Stakes.

Plutus Scripts for Stakes.
-}
module Agora.Stake.Scripts (stakePolicy, stakeValidator) where

import Agora.Record (mkRecordConstr, (.&), (.=))
import Agora.SafeMoney (GTTag)
import Agora.Stake
import Agora.Utils (
  anyInput,
  anyOutput,
  paddValue,
  pfindTxInByTxOutRef,
  pgeqByClass,
  pgeqByClass',
  pgeqBySymbol,
  psingletonValue,
  psymbolValueOf,
  ptokenSpent,
  ptxSignedBy,
  pvalidatorHashToTokenName,
  pvalueSpent,
  tcassert,
  tclet,
  tcmatch,
  tctryFrom,
 )
import Plutarch.Api.V1 (
  PCredential (PPubKeyCredential, PScriptCredential),
  PMintingPolicy,
  PScriptPurpose (PMinting, PSpending),
  PTokenName,
  PTxInfo,
  PValidator,
  mintingPolicySymbol,
  mkMintingPolicy,
 )
import Plutarch.Api.V1.Extra (passetClass, passetClassValueOf)
import Plutarch.Internal (punsafeCoerce)
import Plutarch.Numeric
import Plutarch.SafeMoney (
  Tagged (..),
  pdiscreteValue',
  untag,
 )
import Plutus.V1.Ledger.Value (AssetClass (AssetClass))
import Prelude hiding (Num (..))

{- | Policy for Stake state threads.

   == What this Policy does

   === For minting:

   - Check that exactly one state thread is minted.
   - Check that an output exists with a state thread and a valid datum.
   - Check that no state thread is an input.
   - assert @'Plutus.V1.Ledger.Api.TokenName' == 'Plutus.V1.Ledger.Api.ValidatorHash'@
     of the script that we pay to.

   === For burning:

   - Check that exactly one state thread is burned.
   - Check that datum at state thread is valid and not locked.
-}
stakePolicy ::
  -- | The (governance) token that a Stake can store.
  Tagged GTTag AssetClass ->
  ClosedTerm PMintingPolicy
stakePolicy gtClassRef =
  plam $ \_redeemer ctx' -> unTermCont $ do
    ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
    txInfo <- tclet $ ctx.txInfo
    let _a :: Term _ PTxInfo
        _a = txInfo
    txInfoF <- tcont $ pletFields @'["mint", "inputs", "outputs", "signatories"] txInfo

    PMinting ownSymbol' <- tcmatch $ pfromData ctx.purpose
    ownSymbol <- tclet $ pfield @"_0" # ownSymbol'
    spentST <- tclet $ psymbolValueOf # ownSymbol #$ pvalueSpent # txInfoF.inputs
    mintedST <- tclet $ psymbolValueOf # ownSymbol # txInfoF.mint

    let burning = unTermCont $ do
          tcassert "ST at inputs must be 1" $
            spentST #== 1

          tcassert "ST burned" $
            mintedST #== -1

          tcassert "An unlocked input existed containing an ST" $
            anyInput @PStakeDatum # txInfo
              #$ plam
              $ \value _ stakeDatum' ->
                let hasST = psymbolValueOf # ownSymbol # value #== 1
                    unlocked = pnot # (stakeLocked # stakeDatum')
                 in hasST #&& unlocked

          pure $ popaque (pconstant ())

    let minting = unTermCont $ do
          tcassert "ST at inputs must be 0" $
            spentST #== 0

          tcassert "Minted ST must be exactly 1" $
            mintedST #== 1

          tcassert "A UTXO must exist with the correct output" $
            anyOutput @PStakeDatum # txInfo
              #$ plam
              $ \value address stakeDatum' ->
                let cred = pfield @"credential" # address
                 in pmatch cred $ \case
                      -- Should pay to a script address
                      PPubKeyCredential _ -> pcon PFalse
                      PScriptCredential validatorHash -> unTermCont $ do
                        stakeDatum <- tcont $ pletFields @'["owner", "stakedAmount"] stakeDatum'

                        tn :: Term _ PTokenName <- tclet (pvalidatorHashToTokenName $ pfromData $ pfield @"_0" # validatorHash)

                        let stValue =
                              psingletonValue
                                # ownSymbol
                                -- This coerce is safe because the structure
                                -- of PValidatorHash is the same as PTokenName.
                                # tn
                                # 1
                        let expectedValue =
                              paddValue
                                # (pdiscreteValue' gtClassRef # stakeDatum.stakedAmount)
                                # stValue
                        let ownerSignsTransaction =
                              ptxSignedBy
                                # txInfoF.signatories
                                # stakeDatum.owner

                        -- TODO: This is quite inefficient now, as it does two lookups
                        -- instead of a more efficient single pass,
                        -- but it doesn't really matter for this. At least it's correct.
                        let valueCorrect =
                              foldr1
                                (#&&)
                                [ pgeqByClass' (AssetClass ("", "")) # value # expectedValue
                                , pgeqByClass' (untag gtClassRef)
                                    # value
                                    # expectedValue
                                , pgeqByClass
                                    # ownSymbol
                                    # tn
                                    # value
                                    # expectedValue
                                ]

                        pure $ ownerSignsTransaction #&& valueCorrect
          pure $ popaque (pconstant ())

    pure $ pif (0 #< mintedST) minting burning

--------------------------------------------------------------------------------

{- | Validator intended for Stake UTXOs to be locked by.

== What this Validator does:

=== 'DepositWithdraw'

Deposit or withdraw some GT to the stake.

- Tx must be signed by the owner.
- The 'stakedAmount' field must be updated.
- The stake must not be locked.
- The new UTXO must have the previous value plus the difference
  as stated by the redeemer.

=== 'PermitVote'

Allow a 'ProposalLock' to be put on the stake in order to vote
on a proposal.

- A proposal token must be spent alongside the stake.

  * Its total votes must be correctly updated to include this stake's
    contribution.

- Tx must be signed by the owner.

=== 'RetractVotes'

Remove a 'ProposalLock' set when voting on a proposal.

- A proposal token must be spent alongside the stake.
- Tx must be signed by the owner.

=== 'Destroy'

Destroy the stake in order to reclaim the min ADA.

- The stake must not be locked.
- Tx must be signed by the owner.

=== 'WitnessStake'

Allow this Stake to be included in a transaction without making
any changes to it. In the future,
this could use [CIP-31](https://cips.cardano.org/cips/cip31/) instead.

- Tx must be signed by the owner __or__ a proposal ST token must be spent
  alongside the stake.
- The datum and value must remain unchanged.
-}
stakeValidator :: Stake -> ClosedTerm PValidator
stakeValidator stake =
  plam $ \datum redeemer ctx' -> unTermCont $ do
    ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
    txInfo <- tclet $ pfromData ctx.txInfo
    txInfoF <- tcont $ pletFields @'["mint", "inputs", "outputs", "signatories"] txInfo

    (pfromData -> stakeRedeemer, _) <- tctryFrom redeemer

    -- TODO: Use PTryFrom
    let stakeDatum' :: Term _ PStakeDatum
        stakeDatum' = pfromData $ punsafeCoerce datum
    stakeDatum <- tcont $ pletFields @'["owner", "stakedAmount", "lockedBy"] stakeDatum'

    PSpending txOutRef <- tcmatch $ pfromData ctx.purpose

    PJust txInInfo <- tcmatch $ pfindTxInByTxOutRef # (pfield @"_0" # txOutRef) # txInfoF.inputs
    ownAddress <- tclet $ pfield @"address" #$ pfield @"resolved" # txInInfo
    let continuingValue = pfield @"value" #$ pfield @"resolved" # txInInfo

    -- Whether the owner signs this transaction or not.
    ownerSignsTransaction <- tclet $ ptxSignedBy # txInfoF.signatories # stakeDatum.owner

    stCurrencySymbol <- tclet $ pconstant $ mintingPolicySymbol $ mkMintingPolicy (stakePolicy stake.gtClassRef)
    mintedST <- tclet $ psymbolValueOf # stCurrencySymbol # txInfoF.mint
    valueSpent <- tclet $ pvalueSpent # txInfoF.inputs
    spentST <- tclet $ psymbolValueOf # stCurrencySymbol #$ valueSpent

    let AssetClass (propCs, propTn) = stake.proposalSTClass
        proposalSTClass = passetClass # pconstant propCs # pconstant propTn
    spentProposalST <- tclet $ passetClassValueOf # valueSpent # proposalSTClass

    -- Is the stake currently locked?
    stakeIsLocked <- tclet $ stakeLocked # stakeDatum'

    pure $
      pmatch stakeRedeemer $ \case
        PDestroy _ -> unTermCont $ do
          tcassert "ST at inputs must be 1" $
            spentST #== 1

          tcassert "Should burn ST" $
            mintedST #== -1

          tcassert "Stake unlocked" $ pnot # stakeIsLocked

          tcassert "Owner signs this transaction" ownerSignsTransaction

          pure $ popaque (pconstant ())
        --------------------------------------------------------------------------
        PRetractVotes _ -> unTermCont $ do
          tcassert
            "Owner signs this transaction"
            ownerSignsTransaction

          tcassert "ST at inputs must be 1" $
            spentST #== 1

          -- This puts trust into the Proposal. The Proposal must necessarily check
          -- that this is not abused.
          tcassert "Proposal ST spent" $
            spentProposalST #== 1

          tcassert "A UTXO must exist with the correct output" $
            anyOutput @PStakeDatum # txInfo
              #$ plam
              $ \value address newStakeDatum' ->
                let isScriptAddress = pdata address #== ownAddress
                    _correctOutputDatum = pdata newStakeDatum' #== pdata stakeDatum'
                    valueCorrect = pdata continuingValue #== pdata value
                 in pif
                      isScriptAddress
                      ( foldl1
                          (#&&)
                          [ ptraceIfFalse "valueCorrect" valueCorrect
                          ]
                      )
                      (pcon PFalse)

          pure $ popaque (pconstant ())
        --------------------------------------------------------------------------
        PPermitVote l -> unTermCont $ do
          tcassert
            "Owner signs this transaction"
            ownerSignsTransaction

          -- This puts trust into the Proposal. The Proposal must necessarily check
          -- that this is not abused.
          tcassert "Proposal ST spent" $
            spentProposalST #== 1

          -- Update the stake datum, but only the 'lockedBy' field.

          let -- We actually don't know whether the given lock is valid or not.
              -- This is checked in the proposal validator.
              newLock = pfield @"lock" # l
              -- Prepend the new lock to the existing locks.
              expectedLocks = pcons # newLock # stakeDatum.lockedBy

          expectedDatum <-
            tclet $
              pdata $
                mkRecordConstr
                  PStakeDatum
                  ( #stakedAmount .= stakeDatum.stakedAmount
                      .& #owner .= stakeDatum.owner
                      .& #lockedBy .= pdata expectedLocks
                  )

          tcassert "A UTXO must exist with the correct output" $
            -- FIXME: no need to pass the whole txInfo to 'anyOutput'.
            anyOutput @PStakeDatum # txInfo
              #$ plam
              $ \value address newStakeDatum' ->
                let isScriptAddress = pdata address #== ownAddress
                    correctOutputDatum = pdata newStakeDatum' #== expectedDatum
                    -- TODO: Is this correct? I think We only need to ensure
                    --       correct amount of GT/SST in the continuing output.
                    valueCorrect = pdata continuingValue #== pdata value
                 in pif
                      isScriptAddress
                      ( foldl1
                          (#&&)
                          [ ptraceIfFalse "valueCorrect" valueCorrect
                          , ptraceIfFalse "datumCorrect" correctOutputDatum
                          ]
                      )
                      (pcon PFalse)

          pure $ popaque (pconstant ())
        --------------------------------------------------------------------------
        PWitnessStake _ -> unTermCont $ do
          tcassert "ST at inputs must be 1" $
            spentST #== 1

          let AssetClass (propCs, propTn) = stake.proposalSTClass
              propAssetClass = passetClass # pconstant propCs # pconstant propTn
              proposalTokenMoved =
                ptokenSpent
                  # propAssetClass
                  # txInfoF.inputs

          -- In order for cosignature to be witnessed, it must be possible for a
          -- proposal to allow this transaction to happen. This puts trust into the Proposal.
          -- The Proposal must necessarily check that this is not abused.
          tcassert
            "Owner signs this transaction OR proposal token is spent"
            (ownerSignsTransaction #|| proposalTokenMoved)

          tcassert "A UTXO must exist with the correct output" $
            anyOutput @PStakeDatum # txInfo
              #$ plam
              $ \value address newStakeDatum' ->
                let isScriptAddress = pdata address #== ownAddress
                    correctOutputDatum = pdata newStakeDatum' #== pdata stakeDatum'
                    valueCorrect = pdata continuingValue #== pdata value
                 in pif
                      isScriptAddress
                      ( foldl1
                          (#&&)
                          [ ptraceIfFalse "valueCorrect" valueCorrect
                          , ptraceIfFalse "correctOutputDatum" correctOutputDatum
                          ]
                      )
                      (pcon PFalse)
          pure $ popaque (pconstant ())
        PDepositWithdraw r -> unTermCont $ do
          tcassert "ST at inputs must be 1" $
            spentST #== 1
          tcassert "Stake unlocked" $
            pnot #$ stakeIsLocked
          tcassert
            "Owner signs this transaction"
            ownerSignsTransaction
          tcassert "A UTXO must exist with the correct output" $
            anyOutput @PStakeDatum # txInfo
              #$ plam
              $ \value address newStakeDatum' -> unTermCont $ do
                newStakeDatum <- tcont $ pletFields @'["owner", "stakedAmount"] newStakeDatum'
                delta <- tclet $ pfield @"delta" # r
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

                pure $
                  foldr1
                    (#&&)
                    [ ptraceIfFalse "isScriptAddress" isScriptAddress
                    , ptraceIfFalse "correctOutputDatum" correctOutputDatum
                    , ptraceIfFalse "valueCorrect" valueCorrect
                    ]

          pure $ popaque (pconstant ())
