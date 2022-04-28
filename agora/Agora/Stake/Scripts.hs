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
  ptokenSpent,
  ptxSignedBy,
  pvalueSpent,
  validatorHashToTokenName,
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
import Plutarch.Monadic qualified as P
import Plutarch.Numeric
import Plutarch.SafeMoney (
  Tagged (..),
  pdiscreteValue',
  untag,
 )
import Plutarch.TryFrom (ptryFrom)
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
  plam $ \_redeemer ctx' -> P.do
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    txInfo <- plet $ ctx.txInfo
    let _a :: Term _ PTxInfo
        _a = txInfo
    txInfoF <- pletFields @'["mint", "inputs", "outputs", "signatories"] txInfo

    PMinting ownSymbol' <- pmatch $ pfromData ctx.purpose
    ownSymbol <- plet $ pfield @"_0" # ownSymbol'
    spentST <- plet $ psymbolValueOf # ownSymbol #$ pvalueSpent # txInfoF.inputs
    mintedST <- plet $ psymbolValueOf # ownSymbol # txInfoF.mint

    let burning = P.do
          passert "ST at inputs must be 1" $
            spentST #== 1

          passert "ST burned" $
            mintedST #== -1

          passert "An unlocked input existed containing an ST" $
            anyInput @PStakeDatum # txInfo
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
            anyOutput @PStakeDatum # txInfo
              #$ plam
              $ \value address stakeDatum' -> P.do
                let cred = pfield @"credential" # address
                pmatch cred $ \case
                  -- Should pay to a script address
                  PPubKeyCredential _ -> pcon PFalse
                  PScriptCredential validatorHash -> P.do
                    stakeDatum <- pletFields @'["owner", "stakedAmount"] stakeDatum'

                    tn :: Term _ PTokenName <- plet (validatorHashToTokenName $ pfromData $ pfield @"_0" # validatorHash)

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

                    ownerSignsTransaction
                      #&& valueCorrect
          popaque (pconstant ())

    pif (0 #< mintedST) minting burning

--------------------------------------------------------------------------------

-- | Validator intended for Stake UTXOs to be locked by.
--
--
-- == What this Validator does:
--
-- === 'DepositWithdraw'
--
-- Deposit or withdraw some GT to the stake.
--
-- - Tx must be signed by the owner.
-- - The 'stakedAmount' field must be updated.
-- - The stake must not be locked.
-- - The new UTXO must have the previous value plus the difference
--   as stated by the redeemer.
--
-- === 'PermitVote'
--
-- Allow a 'ProposalLock' to be put on the stake in order to vote
-- on a proposal.
--
-- - A proposal token must be spent alongside the stake.
--
--   * Its total votes must be correctly updated to include this stake's
--     contribution.
--
-- - Tx must be signed by the owner.
--
--
-- === 'RetractVotes'
--
-- Remove a 'ProposalLock' set when voting on a proposal.
--
-- - A proposal token must be spent alongside the stake.
-- - Tx must be signed by the owner.
--
--
-- === 'Destroy'
--
-- Destroy the stake in order to reclaim the min ADA.
--
-- - The stake must not be locked.
-- - Tx must be signed by the owner.
--
--
-- === 'WitnessStake'
--
-- Allow this Stake to be included in a transaction without making
-- any changes to it. In the future,
-- this could use [CIP-31](https://cips.cardano.org/cips/cip31/) instead.
--
-- - Tx must be signed by the owner __or__ a proposal ST token must be spent
--   alongside the stake.
-- - The datum and value must remain unchanged.
stakeValidator :: Stake -> ClosedTerm PValidator
stakeValidator stake =
  plam $ \datum redeemer ctx' -> P.do
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    txInfo <- plet $ pfromData ctx.txInfo
    txInfoF <- pletFields @'["mint", "inputs", "outputs", "signatories"] txInfo

    (pfromData -> stakeRedeemer, _) <- ptryFrom redeemer

    -- TODO: Use PTryFrom
    let stakeDatum' :: Term _ PStakeDatum
        stakeDatum' = pfromData $ punsafeCoerce datum
    stakeDatum <- pletFields @'["owner", "stakedAmount"] stakeDatum'

    PSpending txOutRef <- pmatch $ pfromData ctx.purpose

    PJust txInInfo <- pmatch $ pfindTxInByTxOutRef # (pfield @"_0" # txOutRef) # txInfoF.inputs
    ownAddress <- plet $ pfield @"address" #$ pfield @"resolved" # txInInfo
    let continuingValue = pfield @"value" #$ pfield @"resolved" # txInInfo

    -- Whether the owner signs this transaction or not.
    ownerSignsTransaction <- plet $ ptxSignedBy # txInfoF.signatories # stakeDatum.owner

    stCurrencySymbol <- plet $ pconstant $ mintingPolicySymbol $ mkMintingPolicy (stakePolicy stake.gtClassRef)
    mintedST <- plet $ psymbolValueOf # stCurrencySymbol # txInfoF.mint
    valueSpent <- plet $ pvalueSpent # txInfoF.inputs
    spentST <- plet $ psymbolValueOf # stCurrencySymbol #$ valueSpent

    let AssetClass (propCs, propTn) = stake.proposalSTClass
        proposalSTClass = passetClass # pconstant propCs # pconstant propTn
    spentProposalST <- plet $ passetClassValueOf # valueSpent # proposalSTClass

    -- Is the stake currently locked?
    stakeIsLocked <- plet $ stakeLocked # stakeDatum'

    pmatch stakeRedeemer $ \case
      PDestroy _ -> P.do
        passert "ST at inputs must be 1" $
          spentST #== 1

        passert "Should burn ST" $
          mintedST #== -1

        passert "Stake unlocked" $ pnot # stakeIsLocked

        passert "Owner signs this transaction" ownerSignsTransaction

        popaque (pconstant ())
      --------------------------------------------------------------------------
      PRetractVotes _ -> P.do
        passert
          "Owner signs this transaction"
          ownerSignsTransaction

        passert "ST at inputs must be 1" $
          spentST #== 1

        -- This puts trust into the Proposal. The Proposal must necessarily check
        -- that this is not abused.
        passert "Proposal ST spent" $
          spentProposalST #== 1

        passert "A UTXO must exist with the correct output" $
          anyOutput @PStakeDatum # txInfo
            #$ plam
            $ \value address newStakeDatum' -> P.do
              let isScriptAddress = pdata address #== ownAddress
              let _correctOutputDatum = pdata newStakeDatum' #== pdata stakeDatum'
              let valueCorrect = pdata continuingValue #== pdata value
              pif
                isScriptAddress
                ( foldl1
                    (#&&)
                    [ ptraceIfFalse "valueCorrect" valueCorrect
                    ]
                )
                (pcon PFalse)

        popaque (pconstant ())
      --------------------------------------------------------------------------
      PPermitVote _ -> P.do
        passert
          "Owner signs this transaction"
          ownerSignsTransaction


        -- This puts trust into the Proposal. The Proposal must necessarily check
        -- that this is not abused.
        passert "Proposal ST spent" $
          spentProposalST #== 1

        passert "A UTXO must exist with the correct output" $
          anyOutput @PStakeDatum # txInfo
            #$ plam
            $ \value address newStakeDatum' -> P.do
              let isScriptAddress = pdata address #== ownAddress
              let _correctOutputDatum = pdata newStakeDatum' #== pdata stakeDatum'
              let valueCorrect = pdata continuingValue #== pdata value
              pif
                isScriptAddress
                ( foldl1
                    (#&&)
                    [ ptraceIfFalse "valueCorrect" valueCorrect
                    ]
                )
                (pcon PFalse)

        popaque (pconstant ())
      --------------------------------------------------------------------------
      PWitnessStake _ -> P.do
        passert "ST at inputs must be 1" $
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
        passert
          "Owner signs this transaction OR proposal token is spent"
          (ownerSignsTransaction #|| proposalTokenMoved)

        passert "A UTXO must exist with the correct output" $
          anyOutput @PStakeDatum # txInfo
            #$ plam
            $ \value address newStakeDatum' -> P.do
              let isScriptAddress = pdata address #== ownAddress
              let correctOutputDatum = pdata newStakeDatum' #== pdata stakeDatum'
              let valueCorrect = pdata continuingValue #== pdata value
              pif
                isScriptAddress
                ( foldl1
                    (#&&)
                    [ ptraceIfFalse "valueCorrect" valueCorrect
                    , ptraceIfFalse "correctOutputDatum" correctOutputDatum
                    ]
                )
                (pcon PFalse)
        popaque (pconstant ())
      PDepositWithdraw r -> P.do
        passert "ST at inputs must be 1" $
          spentST #== 1
        passert "Stake unlocked" $
          pnot #$ stakeIsLocked
        passert
          "Owner signs this transaction"
          ownerSignsTransaction
        passert "A UTXO must exist with the correct output" $
          anyOutput @PStakeDatum # txInfo
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
