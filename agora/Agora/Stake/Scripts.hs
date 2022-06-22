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
  pvalidatorHashToTokenName,
 )
import Data.Tagged (Tagged (..), untag)
import Plutarch.Api.V1 (
  AmountGuarantees (Positive),
  PCredential (PPubKeyCredential, PScriptCredential),
  PMintingPolicy,
  PScriptPurpose (PMinting, PSpending),
  PTokenName,
  PTxInfo,
  PValidator,
  PValue,
  mintingPolicySymbol,
  mkMintingPolicy,
 )
import Plutarch.Api.V1.AssetClass (passetClass, passetClassValueOf, pvalueOf)
import Plutarch.Api.V1.ScriptContext (pfindTxInByTxOutRef, pisTokenSpent, ptxSignedBy, pvalueSpent)
import "liqwid-plutarch-extra" Plutarch.Api.V1.Value (pgeqByClass', pgeqBySymbol, psymbolValueOf)
import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import Plutarch.Extra.TermCont (pguardC, pletC, pmatchC, ptryFromC)
import Plutarch.Internal (punsafeCoerce)
import Plutarch.Numeric.Additive (AdditiveMonoid (zero), AdditiveSemigroup ((+)))
import Plutarch.SafeMoney (
  pdiscreteValue',
  pvalueDiscrete',
 )
import PlutusLedgerApi.V1.Value (AssetClass (AssetClass))
import Prelude hiding (Num (..))

{- | Policy for Stake state threads.

   == What this Policy does

   === For minting:

   - Check that exactly one state thread is minted.
   - Check that an output exists with a state thread and a valid datum.
   - Check that no state thread is an input.
   - assert @'PlutusLedgerApi.V1.TokenName' == 'PlutusLedgerApi.V1.ValidatorHash'@
     of the script that we pay to.

   === For burning:

   - Check that exactly one state thread is burned.
   - Check that datum at state thread is valid and not locked.

   @since 0.1.0
-}
stakePolicy ::
  -- | The (governance) token that a Stake can store.
  Tagged GTTag AssetClass ->
  ClosedTerm PMintingPolicy
stakePolicy gtClassRef =
  plam $ \_redeemer ctx' -> unTermCont $ do
    ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
    txInfo <- pletC $ ctx.txInfo
    let _a :: Term _ PTxInfo
        _a = txInfo
    txInfoF <- tcont $ pletFields @'["mint", "inputs", "outputs", "signatories", "datums"] txInfo

    PMinting ownSymbol' <- pmatchC $ pfromData ctx.purpose
    ownSymbol <- pletC $ pfield @"_0" # ownSymbol'
    spentST <- pletC $ psymbolValueOf # ownSymbol #$ pvalueSpent # txInfoF.inputs
    mintedST <- pletC $ psymbolValueOf # ownSymbol # txInfoF.mint

    let burning = unTermCont $ do
          pguardC "ST at inputs must be 1" $
            spentST #== 1

          pguardC "ST burned" $
            mintedST #== -1

          pguardC "An unlocked input existed containing an ST" $
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
          pguardC "ST at inputs must be 0" $
            spentST #== 0

          pguardC "Minted ST must be exactly 1" $
            mintedST #== 1

          pguardC "A UTXO must exist with the correct output" $
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

     @since 0.1.0
-}
stakeValidator :: Stake -> ClosedTerm PValidator
stakeValidator stake =
  plam $ \datum redeemer ctx' -> unTermCont $ do
    ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
    txInfo <- pletC $ pfromData ctx.txInfo
    txInfoF <- tcont $ pletFields @'["mint", "inputs", "outputs", "signatories", "datums"] txInfo

    (pfromData -> stakeRedeemer, _) <- ptryFromC redeemer

    -- TODO: Use PTryFrom
    let stakeDatum' :: Term _ PStakeDatum
        stakeDatum' = pfromData $ punsafeCoerce datum
    stakeDatum <- tcont $ pletFields @'["owner", "stakedAmount", "lockedBy"] stakeDatum'

    PSpending txOutRef <- pmatchC $ pfromData ctx.purpose

    PJust txInInfo <- pmatchC $ pfindTxInByTxOutRef # (pfield @"_0" # txOutRef) # txInfoF.inputs
    ownAddress <- pletC $ pfield @"address" #$ pfield @"resolved" # txInInfo
    let continuingValue :: Term _ (PValue _ _)
        continuingValue = pfield @"value" #$ pfield @"resolved" # txInInfo

    -- Whether the owner signs this transaction or not.
    ownerSignsTransaction <- pletC $ ptxSignedBy # txInfoF.signatories # stakeDatum.owner

    stCurrencySymbol <- pletC $ pconstant $ mintingPolicySymbol $ mkMintingPolicy (stakePolicy stake.gtClassRef)
    mintedST <- pletC $ psymbolValueOf # stCurrencySymbol # txInfoF.mint
    valueSpent <- pletC $ pvalueSpent # txInfoF.inputs
    spentST <- pletC $ psymbolValueOf # stCurrencySymbol #$ valueSpent

    let AssetClass (propCs, propTn) = stake.proposalSTClass
        proposalSTClass = passetClass # pconstant propCs # pconstant propTn
    spentProposalST <- pletC $ passetClassValueOf # valueSpent # proposalSTClass

    -- Is the stake currently locked?
    stakeIsLocked <- pletC $ stakeLocked # stakeDatum'

    pure $
      pmatch stakeRedeemer $ \case
        PDestroy _ -> unTermCont $ do
          pguardC "ST at inputs must be 1" $
            spentST #== 1

          pguardC "Should burn ST" $
            mintedST #== -1

          pguardC "Stake unlocked" $ pnot # stakeIsLocked

          pguardC "Owner signs this transaction" ownerSignsTransaction

          pure $ popaque (pconstant ())
        --------------------------------------------------------------------------
        -- Handle redeemers that require own stake output.
        _ -> unTermCont $ do
          -- Filter out own output with own address and PST.
          ownOutput <-
            pletC $
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
            pletC $
              mustFindDatum' @PStakeDatum
                # (pfield @"datumHash" # ownOutput)
                # txInfoF.datums

          ownOutputValue <-
            pletC $
              pfield @"value" # ownOutput

          ownOutputValueUnchanged <-
            pletC $
              pdata continuingValue #== pdata ownOutputValue

          stakeOutUnchanged <-
            pletC $
              pdata stakeOut #== pdata stakeDatum'

          pure $
            pmatch stakeRedeemer $ \case
              PRetractVotes l -> unTermCont $ do
                pguardC
                  "Owner signs this transaction"
                  ownerSignsTransaction

                pguardC "ST at inputs must be 1" $
                  spentST #== 1

                -- This puts trust into the Proposal. The Proposal must necessarily check
                -- that this is not abused.
                pguardC "Proposal ST spent" $
                  spentProposalST #== 1

                pguardC "A UTXO must exist with the correct output" $
                  let expectedLocks = pfield @"locks" # l

                      expectedDatum =
                        mkRecordConstr
                          PStakeDatum
                          ( #stakedAmount .= stakeDatum.stakedAmount
                              .& #owner .= stakeDatum.owner
                              .& #lockedBy .= expectedLocks
                          )

                      valueCorrect = ownOutputValueUnchanged
                      outputDatumCorrect = stakeOut #== expectedDatum
                   in foldl1
                        (#&&)
                        [ ptraceIfFalse "valueCorrect" valueCorrect
                        , ptraceIfFalse "datumCorrect" outputDatumCorrect
                        ]

                pure $ popaque (pconstant ())
              --------------------------------------------------------------------------
              PPermitVote l -> unTermCont $ do
                pguardC
                  "Owner signs this transaction"
                  ownerSignsTransaction

                -- This puts trust into the Proposal. The Proposal must necessarily check
                -- that this is not abused.
                pguardC "Proposal ST spent" $
                  spentProposalST #== 1

                -- Update the stake datum, but only the 'lockedBy' field.

                let -- We actually don't know whether the given lock is valid or not.
                    -- This is checked in the proposal validator.
                    newLock = pfield @"lock" # l
                    -- Prepend the new lock to the existing locks.
                    expectedLocks = pcons # newLock # stakeDatum.lockedBy

                expectedDatum <-
                  pletC $
                    mkRecordConstr
                      PStakeDatum
                      ( #stakedAmount .= stakeDatum.stakedAmount
                          .& #owner .= stakeDatum.owner
                          .& #lockedBy .= pdata expectedLocks
                      )

                pguardC "A UTXO must exist with the correct output" $
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
                pguardC "ST at inputs must be 1" $
                  spentST #== 1

                let AssetClass (propCs, propTn) = stake.proposalSTClass
                    propAssetClass = passetClass # pconstant propCs # pconstant propTn
                    proposalTokenMoved =
                      pisTokenSpent
                        # propAssetClass
                        # txInfoF.inputs

                -- In order for cosignature to be witnessed, it must be possible for a
                -- proposal to allow this transaction to happen. This puts trust into the Proposal.
                -- The Proposal must necessarily check that this is not abused.
                pguardC
                  "Owner signs this transaction OR proposal token is spent"
                  (ownerSignsTransaction #|| proposalTokenMoved)

                pguardC "A UTXO must exist with the correct output" $
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
                pguardC "ST at inputs must be 1" $
                  spentST #== 1
                pguardC "Stake unlocked" $
                  pnot #$ stakeIsLocked
                pguardC
                  "Owner signs this transaction"
                  ownerSignsTransaction
                pguardC "A UTXO must exist with the correct output" $
                  unTermCont $ do
                    let oldStakedAmount = pfromData $ stakeDatum.stakedAmount
                        delta = pfromData $ pfield @"delta" # r

                    newStakedAmount <- pletC $ oldStakedAmount + delta

                    pguardC "New staked amount shoudl be greater than or equal to 0" $
                      zero #<= newStakedAmount

                    let expectedDatum =
                          mkRecordConstr
                            PStakeDatum
                            ( #stakedAmount .= pdata newStakedAmount
                                .& #owner .= stakeDatum.owner
                                .& #lockedBy .= stakeDatum.lockedBy
                            )
                        datumCorrect = stakeOut #== expectedDatum

                    let valueDelta :: Term _ (PValue _ 'Positive)
                        valueDelta = pdiscreteValue' stake.gtClassRef # delta

                        expectedValue =
                          continuingValue <> valueDelta

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
