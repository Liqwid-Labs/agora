module Agora.Stake.Redeemers (
  ppermitVote,
  pretractVote,
  pdelegateTo,
  pclearDelegate,
  pdestroy,
  pdepositWithdraw,
  pdepositWithdraw',
) where

import Agora.Proposal (PProposalRedeemer (PUnlock, PVote))
import Agora.Stake (
  PExtraTxContext (inputs),
  PProposalContext (PNewProposal, PWithProposalRedeemer),
  PSigContext (PSignedByOwner, PUnknownSig),
  PStakeDatum (PStakeDatum),
  PStakeInputContext (PStakeInput),
  PStakeOutputContext (PStakeBurnt, PStakeOutput),
  PStakeRedeemerContext (PDepositWithdrawDelta, PNoMetadata, PSetDelegateTo),
  PStakeRedeemerHandler,
  PStakeRedeemerHandlerContext (..),
  pstakeLocked,
 )
import Plutarch.Api.V1.Address (PCredential)
import Plutarch.Api.V1.Value (AmountGuarantees (Positive), PValue)
import Plutarch.Api.V2 (PMaybeData)
import Plutarch.Extra.Field (pletAllC)
import Plutarch.Extra.Maybe (pdjust, pdnothing, pmaybeData)
import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import Plutarch.Extra.TermCont (pguardC, pletC, pmatchC, ptraceC)
import Plutarch.Extra.Value (pgeqByClass, pgeqByClass')
import Plutarch.Numeric.Additive (AdditiveMonoid (zero), AdditiveSemigroup ((+)))
import Plutarch.SafeMoney (pdiscreteValue)
import PlutusLedgerApi.V1.Value (AssetClass (..))
import Prelude hiding (Num ((+)))

pownOutputValueUnchanged ::
  forall (s :: S).
  Term s (PStakeRedeemerHandlerContext :--> PBool)
pownOutputValueUnchanged = phoistAcyclic $
  plam $
    flip pmatch $ \ctxF -> unTermCont $ do
      PStakeInput _ inVal <- pmatchC ctxF.stakeInput
      PStakeOutput _ outVal <- pmatchC ctxF.stakeOutput

      pure $ inVal #== outVal

ponlyLocksUpdated ::
  forall (s :: S).
  Term s (PStakeRedeemerHandlerContext :--> PBool)
ponlyLocksUpdated = phoistAcyclic $
  plam $
    flip pmatch $ \ctxF -> unTermCont $ do
      PStakeInput inDat _ <- pmatchC ctxF.stakeInput
      PStakeOutput outDat _ <- pmatchC ctxF.stakeOutput

      inDatF <- pletAllC inDat

      let onlyLocksUpdated =
            let templateStakeDatum =
                  mkRecordConstr
                    PStakeDatum
                    ( #stakedAmount .= inDatF.stakedAmount
                        .& #owner .= inDatF.owner
                        .& #delegatedTo .= inDatF.delegatedTo
                        .& #lockedBy .= pfield @"lockedBy" # outDat
                    )
             in outDat #== templateStakeDatum

      pure onlyLocksUpdated

psignedByOwner ::
  forall (s :: S).
  Term s (PStakeRedeemerHandlerContext :--> PBool)
psignedByOwner = phoistAcyclic $
  plam $
    flip pmatch $ \ctxF -> pmatch ctxF.sigContext $ \case
      PSignedByOwner -> pconstant True
      _ -> pconstant False

pvoteHelper ::
  forall (s :: S).
  Term
    s
    ( (PProposalContext :--> PBool)
        :--> PStakeRedeemerHandler
    )
pvoteHelper = phoistAcyclic $
  plam $ \valProposalCtx ctx -> unTermCont $ do
    ctxF <- pmatchC ctx

    pguardC "Owner or delegate signs this transaction" $
      pmatch ctxF.sigContext $ \case
        PUnknownSig -> pconstant False
        _ -> pconstant True

    -- This puts trust into the Proposal. The Proposal must necessarily check
    -- that this is not abused.

    pguardC "Proposal ST spent" $
      valProposalCtx # ctxF.proposalContext

    pguardC "A UTXO must exist with the correct output" $
      let valueCorrect = pownOutputValueUnchanged # ctx
          outputDatumCorrect = ponlyLocksUpdated # ctx
       in foldl1
            (#&&)
            [ ptraceIfFalse "valueCorrect" valueCorrect
            , ptraceIfFalse "datumCorrect" outputDatumCorrect
            ]

    pure $ pconstant ()

ppermitVote :: forall (s :: S). Term s PStakeRedeemerHandler
ppermitVote = pvoteHelper #$ phoistAcyclic $
  plam $
    flip pmatch $ \case
      PWithProposalRedeemer r -> pmatch r $ \case
        PVote _ -> pconstant True
        _ -> ptrace "Expected Vote" $ pconstant False
      PNewProposal -> pconstant True
      _ -> pconstant False

pretractVote :: forall (s :: S). Term s PStakeRedeemerHandler
pretractVote = pvoteHelper #$ phoistAcyclic $
  plam $
    flip pmatch $ \case
      PWithProposalRedeemer r -> pmatch r $ \case
        PUnlock _ -> pconstant True
        _ -> ptrace "Expected Unlock" $ pconstant False
      _ -> pconstant False

pdelegateHelper ::
  forall (s :: S).
  Term
    s
    ( (PStakeRedeemerContext :--> PMaybeData (PAsData PCredential))
        :--> PStakeRedeemerHandler
    )
pdelegateHelper = phoistAcyclic $
  plam $ \f ctx -> unTermCont $ do
    ctxF <- pmatchC ctx

    pguardC "Owner signs this transaction" $ psignedByOwner # ctx

    PStakeInput inpDat _ <- pmatchC ctxF.stakeInput
    PStakeOutput outDat _ <- pmatchC ctxF.stakeOutput

    inpDatF <- pletAllC inpDat

    let maybePkh = f # ctxF.redeemerContext

    pguardC "Cannot delegate to the owner" $
      pmaybeData
        # pcon PTrue
        # plam (\pkh -> pnot #$ inpDatF.owner #== pkh)
        # maybePkh

    pguardC "A UTXO must exist with the correct output" $
      let correctOutputDatum =
            outDat
              #== mkRecordConstr
                PStakeDatum
                ( #stakedAmount .= inpDatF.stakedAmount
                    .& #owner .= inpDatF.owner
                    .& #delegatedTo .= pdata maybePkh
                    .& #lockedBy .= inpDatF.lockedBy
                )
          valueCorrect = pownOutputValueUnchanged # ctx
       in foldl1
            (#&&)
            [ ptraceIfFalse "valueCorrect" valueCorrect
            , ptraceIfFalse "datumCorrect" correctOutputDatum
            ]

    pure $ pconstant ()

pdelegateTo :: forall (s :: S). Term s PStakeRedeemerHandler
pdelegateTo = pdelegateHelper #$ phoistAcyclic $
  plam $
    flip pmatch $ \case
      PSetDelegateTo c -> pdjust # pdata c
      _ -> perror

pclearDelegate :: forall (s :: S). Term s PStakeRedeemerHandler
pclearDelegate = pdelegateHelper #$ phoistAcyclic $
  plam $
    flip pmatch $ \case
      PNoMetadata -> pdnothing
      _ -> perror

pdestroy :: forall (s :: S). Term s PStakeRedeemerHandler
pdestroy = phoistAcyclic $
  plam $ \ctx -> unTermCont $ do
    ctxF <- pmatchC ctx

    PStakeInput inpDat _ <- pmatchC ctxF.stakeInput
    PStakeBurnt <- pmatchC ctxF.stakeOutput

    pguardC "Owner signs this transaction" $
      psignedByOwner # ctx

    pguardC "Stake unlocked" $ pnot #$ pstakeLocked # inpDat

    pure $ pconstant ()

pdepositWithdraw' :: forall (s :: S). Term s PStakeRedeemerHandler
pdepositWithdraw' = phoistAcyclic $
  plam $ \ctx -> unTermCont $ do
    ctxF <- pmatchC ctx

    extraTxContextF <- pmatchC ctxF.extraTxContext

    ptraceC $ pshow $ plength # extraTxContextF.inputs

    pure $ pdepositWithdraw # ctx

pdepositWithdraw :: forall (s :: S). Term s PStakeRedeemerHandler
pdepositWithdraw = phoistAcyclic $
  plam $ \ctx -> unTermCont $ do
    ctxF <- pmatchC ctx

    PStakeInput inpDat inpVal <- pmatchC ctxF.stakeInput
    PStakeOutput outDat outVal <- pmatchC ctxF.stakeOutput

    pguardC "Stake unlocked" $ pnot #$ pstakeLocked # inpDat

    pguardC "Owner signs this transaction" $ psignedByOwner # ctx

    pguardC
      "A UTXO must exist with the correct output"
      $ unTermCont $ do
        inpDatF <- pletAllC inpDat
        PDepositWithdrawDelta delta <- pmatchC ctxF.redeemerContext

        let oldStakedAmount = pfromData $ inpDatF.stakedAmount

        newStakedAmount <- pletC $ oldStakedAmount + delta

        pguardC "New staked amount should be greater than or equal to 0" $
          zero #<= newStakedAmount

        let expectedDatum =
              mkRecordConstr
                PStakeDatum
                ( #stakedAmount .= pdata newStakedAmount
                    .& #owner .= inpDatF.owner
                    .& #delegatedTo .= inpDatF.delegatedTo
                    .& #lockedBy .= inpDatF.lockedBy
                )
            datumCorrect = outDat #== expectedDatum

        let valueDelta :: Term _ (PValue _ 'Positive)
            valueDelta = pdiscreteValue # ctxF.gtAssetClass # delta

            expectedValue =
              inpVal <> valueDelta

        gtAssetClassF <- pletAllC ctxF.gtAssetClass

        let valueCorrect =
              foldr1
                (#&&)
                [ pgeqByClass' (AssetClass ("", ""))
                    # outVal
                    # expectedValue
                , pgeqByClass
                    # gtAssetClassF.currencySymbol
                    # gtAssetClassF.tokenName
                    # outVal
                    # expectedValue
                ]
        --
        pure $
          foldl1
            (#&&)
            [ ptraceIfFalse "valueCorrect" valueCorrect
            , ptraceIfFalse "datumCorrect" datumCorrect
            ]
    pure $ pconstant ()
