{- |
Module     : Agora.Stake.Scripts
Maintainer : emi@haskell.fyi
Description: Plutus Scripts for Stakes.

Plutus Scripts for Stakes.
-}
module Agora.Stake.Scripts (stakePolicy, stakeValidator) where

import Agora.SafeMoney (GTTag)
import Agora.Stake
import Agora.Utils (
  mustBePJust,
  mustFindDatum',
  paddValue,
  pfindTxInByTxOutRef,
  pgeqByClass',
  pgeqBySymbol,
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
import Data.Tagged (Tagged (..), untag)
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
import Plutarch.Api.V1.AssetClass (passetClass, passetClassValueOf, pvalueOf)
import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import Plutarch.Internal (punsafeCoerce)
import Plutarch.Numeric.Additive (AdditiveMonoid (zero), AdditiveSemigroup ((+)))
import Plutarch.SafeMoney (
  pdiscreteValue',
  pvalueDiscrete',
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
    txInfoF <- tcont $ pletFields @'["mint", "inputs", "outputs", "signatories", "datums"] txInfo

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
            pany
              # plam
                ( \((pfield @"resolved" #) -> txOut) -> unTermCont $ do
                    txOutF <- tcont $ pletFields @'["value", "datumHash"] txOut
                    pure $
                      pif
                        (psymbolValueOf # ownSymbol # txOutF.value #== 1)
                        ( let datum = mustFindDatum' @PStakeDatum # txOutF.datumHash # txInfoF.datums
                           in pnot # (stakeLocked # datum)
                        )
                        (pconstant False)
                )
              # pfromData txInfoF.inputs

          pure $ popaque (pconstant ())

    let minting = unTermCont $ do
          tcassert "ST at inputs must be 0" $
            spentST #== 0

          tcassert "Minted ST must be exactly 1" $
            mintedST #== 1

          tcassert "A UTXO must exist with the correct output" $
            unTermCont $ do
              let scriptOutputWithStakeST =
                    mustBePJust
                      # "Output to script not found"
                        #$ pfind
                      # plam
                        ( \output -> unTermCont $ do
                            outputF <- tcont $ pletFields @'["value", "address"] output
                            pure $
                              pmatch (pfromData $ pfield @"credential" # outputF.address) $ \case
                                -- Should pay to a script address
                                PPubKeyCredential _ -> pcon PFalse
                                PScriptCredential ((pfield @"_0" #) -> validatorHash) ->
                                  let tn :: Term _ PTokenName
                                      tn = pvalidatorHashToTokenName validatorHash
                                   in pvalueOf # outputF.value # ownSymbol # tn #== 1
                        )
                      # pfromData txInfoF.outputs

              outputF <-
                tcont $
                  pletFields @'["value", "address", "datumHash"] scriptOutputWithStakeST
              datumF <-
                tcont $
                  pletFields @'["owner", "stakedAmount"] $
                    mustFindDatum' @PStakeDatum # outputF.datumHash # txInfoF.datums

              let hasExpectedStake =
                    ptraceIfFalse "Stake ouput has expected amount of stake token" $
                      pvalueDiscrete' gtClassRef # outputF.value #== datumF.stakedAmount
              let ownerSignsTransaction =
                    ptraceIfFalse "Stake Owner should sign the transaction" $
                      ptxSignedBy
                        # txInfoF.signatories
                        # datumF.owner

              pure $ hasExpectedStake #&& ownerSignsTransaction

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
    txInfoF <- tcont $ pletFields @'["mint", "inputs", "outputs", "signatories", "datums"] txInfo

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
        -- Handle redeemers that require own stake output.
        _ -> unTermCont $ do
          -- Filter out own output with own address and PST.
          ownOutput <-
            tclet $
              mustBePJust # "Own output should be present" #$ pfind
                # plam
                  ( \input -> unTermCont $ do
                      inputF <- tcont $ pletFields @'["address", "value"] input
                      pure $
                        inputF.address #== ownAddress
                          #&& psymbolValueOf # stCurrencySymbol # inputF.value #== 1
                  )
                # pfromData txInfoF.outputs

          stakeOut <-
            tclet $
              mustFindDatum' @PStakeDatum
                # (pfield @"datumHash" # ownOutput)
                # txInfoF.datums

          ownOutputValue <-
            tclet $
              pfield @"value" # ownOutput

          ownOutputValueUnchanged <-
            tclet $
              pdata continuingValue #== pdata ownOutputValue

          stakeOutUnchanged <-
            tclet $
              pdata stakeOut #== pdata stakeDatum'

          pure $
            pmatch stakeRedeemer $ \case
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
                  unTermCont $ do
                    let valueCorrect = ownOutputValueUnchanged

                    -- TODO: check output datum is expected.

                    pure $
                      foldl1
                        (#&&)
                        [ ptraceIfFalse "valueCorrect" valueCorrect
                        ]

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
                    mkRecordConstr
                      PStakeDatum
                      ( #stakedAmount .= stakeDatum.stakedAmount
                          .& #owner .= stakeDatum.owner
                          .& #lockedBy .= pdata expectedLocks
                      )

                tcassert "A UTXO must exist with the correct output" $
                  let correctOutputDatum = stakeOut #== expectedDatum
                      valueCorrect = ownOutputValueUnchanged
                   in foldl1
                        (#&&)
                        [ ptraceIfFalse "valueCorrect" valueCorrect
                        , ptraceIfFalse "datumCorrect" correctOutputDatum
                        ]

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
                  let correctOutputDatum = stakeOutUnchanged
                      valueCorrect = ownOutputValueUnchanged
                   in foldl1
                        (#&&)
                        [ ptraceIfFalse "valueCorrect" valueCorrect
                        , ptraceIfFalse "correctOutputDatum" correctOutputDatum
                        ]
                pure $ popaque (pconstant ())
              --------------------------------------------------------------------------
              PDepositWithdraw r -> unTermCont $ do
                tcassert "ST at inputs must be 1" $
                  spentST #== 1
                tcassert "Stake unlocked" $
                  pnot #$ stakeIsLocked
                tcassert
                  "Owner signs this transaction"
                  ownerSignsTransaction
                tcassert "A UTXO must exist with the correct output" $
                  unTermCont $ do
                    let oldStakedAmount = pfromData $ stakeDatum.stakedAmount
                        delta = pfromData $ pfield @"delta" # r

                    newStakedAmount <- tclet $ oldStakedAmount + delta

                    tcassert "New staked amount shoudl be greater than or equal to 0" $
                      zero #<= newStakedAmount

                    let expectedDatum =
                          mkRecordConstr
                            PStakeDatum
                            ( #stakedAmount .= pdata newStakedAmount
                                .& #owner .= stakeDatum.owner
                                .& #lockedBy .= stakeDatum.lockedBy
                            )
                        datumCorrect = stakeOut #== expectedDatum

                    let expectedValue =
                          paddValue # continuingValue # (pdiscreteValue' stake.gtClassRef # delta)

                        valueCorrect =
                          foldr1
                            (#&&)
                            [ pgeqByClass' (AssetClass ("", ""))
                                # ownOutputValue
                                # expectedValue
                            , pgeqByClass' (untag stake.gtClassRef)
                                # ownOutputValue
                                # expectedValue
                            , pgeqBySymbol
                                # stCurrencySymbol
                                # ownOutputValue
                                # expectedValue
                            ]
                    --
                    pure $
                      foldl1
                        (#&&)
                        [ ptraceIfFalse "valueCorrect" valueCorrect
                        , ptraceIfFalse "datumCorrect" datumCorrect
                        ]
                --
                pure $ popaque (pconstant ())
              _ -> popaque (pconstant ())
