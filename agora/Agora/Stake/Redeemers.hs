{- |
Module     : Agora.Stake.Redeemers
Maintainer : connor@mlabs.city
Description: Default implementation of stake redeemer handlers

Default implementation of stake redeemer handlers.
-}
module Agora.Stake.Redeemers (
  ppermitVote,
  pretractVote,
  pdelegateTo,
  pclearDelegate,
  pdestroy,
  pdepositWithdraw,
) where

import Agora.Proposal (PProposalRedeemer (PUnlock, PVote))
import Agora.Stake (
  PProposalContext (
    PNewProposal,
    PWithProposalRedeemer
  ),
  PSigContext (owner, signedBy),
  PSignedBy (
    PSignedByDelegate,
    PSignedByOwner,
    PUnknownSig
  ),
  PStakeDatum (PStakeDatum),
  PStakeRedeemerContext (
    PDepositWithdrawDelta,
    PNoMetadata,
    PSetDelegateTo
  ),
  PStakeRedeemerHandler,
  PStakeRedeemerHandlerContext (
    proposalContext,
    redeemerContext,
    sigContext,
    stakeInputDatums,
    stakeOutputDatums
  ),
  pstakeLocked,
 )
import Agora.Utils (pdeleteBy, pfromSingleton)
import Plutarch.Api.V1.Address (PCredential)
import Plutarch.Api.V2 (PMaybeData)
import Plutarch.Extra.Field (pletAll, pletAllC)
import Plutarch.Extra.Maybe (pdjust, pdnothing, pmaybeData)
import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pguardC, pletC, pmatchC)
import Plutarch.Numeric.Additive (AdditiveMonoid (zero), AdditiveSemigroup ((+)))
import Prelude hiding (Num ((+)))

pbatchUpdateInputs ::
  forall (s :: S).
  Term
    s
    ( (PStakeDatum :--> PStakeDatum :--> PBool)
        :--> PStakeRedeemerHandlerContext
        :--> PBool
    )
pbatchUpdateInputs = phoistAcyclic $
  plam $ \f -> flip pmatch $ \ctxF ->
    pnull #$ pfoldr
      # (pdeleteBy # f)
      # ctxF.stakeOutputDatums
      # ctxF.stakeInputDatums

pgetSignedBy ::
  forall (s :: S).
  Term
    s
    (PStakeRedeemerHandlerContext :--> PSignedBy)
pgetSignedBy = phoistAcyclic $
  plam $ \ctx -> unTermCont $ do
    ctxF <- pmatchC ctx
    sctxF <- pmatchC ctxF.sigContext
    pure sctxF.signedBy

pisSignedBy ::
  forall (s :: S).
  Term
    s
    (PBool :--> PBool :--> PStakeRedeemerHandlerContext :--> PBool)
pisSignedBy = phoistAcyclic $
  plam $ \byOwner byDelegate ctx ->
    pmatch (pgetSignedBy # ctx) $ \case
      PSignedByOwner -> byOwner
      PSignedByDelegate -> byDelegate
      PUnknownSig -> pconstant False

-- | Return true if only the @lockedBy@ field of the stake datum is updated.
ponlyLocksUpdated ::
  forall (s :: S).
  Term s (PStakeRedeemerHandlerContext :--> PBool)
ponlyLocksUpdated = phoistAcyclic $
  pbatchUpdateInputs #$ plam $ \i o ->
    pletAll i $ \iF ->
      let newLocks = pfield @"lockedBy" # o
       in mkRecordConstr
            PStakeDatum
            ( #stakedAmount .= iF.stakedAmount
                .& #owner .= iF.owner
                .& #delegatedTo .= iF.delegatedTo
                .& #lockedBy .= newLocks
            )
            #== o

-- | Validation logic shared between 'ppermitVote' and 'retractVote'.
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
      pisSignedBy # pconstant True # pconstant True # ctx

    -- This puts trust into the Proposal. The Proposal must necessarily check
    -- that this is not abused.

    pguardC "Proposal ST spent" $
      valProposalCtx # ctxF.proposalContext

    pguardC "Correct outputs" $
      ponlyLocksUpdated # ctx

    pure $ pconstant ()

{- | Default implementation of 'Agora.Stake.PermitVote'.

     @since 1.0.0
-}
ppermitVote :: forall (s :: S). Term s PStakeRedeemerHandler
ppermitVote = pvoteHelper #$ phoistAcyclic $
  plam $
    flip pmatch $ \case
      PWithProposalRedeemer r -> pmatch r $ \case
        PVote _ -> pconstant True
        _ -> ptrace "Expected Vote" $ pconstant False
      PNewProposal -> pconstant True
      _ -> pconstant False

{- | Default implementation of 'Agora.Stake.RetractVotes'.

     @since 1.0.0
-}
pretractVote :: forall (s :: S). Term s PStakeRedeemerHandler
pretractVote = pvoteHelper #$ phoistAcyclic $
  plam $
    flip pmatch $ \case
      PWithProposalRedeemer r -> pmatch r $ \case
        PUnlock _ -> pconstant True
        _ -> ptrace "Expected Unlock" $ pconstant False
      _ -> pconstant False

-- | Validation logic shared by 'pdelegateTo' and 'pclearDelegate'.
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
    sigCtxF <- pmatchC ctxF.sigContext

    pguardC "Owner signs this transaction" $
      pisSignedBy # pconstant True # pconstant False # ctx

    let newDelegate = f # ctxF.redeemerContext

    pguardC "Cannot delegate to the owner" $
      pmaybeData
        # pcon PTrue
        # plam (\pkh -> pnot #$ sigCtxF.owner #== pfromData pkh)
        # newDelegate

    pguardC "Correct outputs" $
      pbatchUpdateInputs
        # plam
          ( \i o -> pletAll i $ \iF ->
              mkRecordConstr
                PStakeDatum
                ( #stakedAmount .= iF.stakedAmount
                    .& #owner .= iF.owner
                    .& #delegatedTo .= pdata newDelegate
                    .& #lockedBy .= iF.lockedBy
                )
                #== o
          )
        # ctx

    pure $ pconstant ()

{- | Default implementation of 'Agora.Stake.DelegateTo'.

     @since 1.0.0
-}
pdelegateTo :: forall (s :: S). Term s PStakeRedeemerHandler
pdelegateTo = pdelegateHelper #$ phoistAcyclic $
  plam $
    flip pmatch $ \case
      PSetDelegateTo c -> pdjust # pdata c
      _ -> perror

{- | Default implementation of 'Agora.Stake.ClearDelegate'.

     @since 1.0.0
-}
pclearDelegate :: forall (s :: S). Term s PStakeRedeemerHandler
pclearDelegate = pdelegateHelper #$ phoistAcyclic $
  plam $
    flip pmatch $ \case
      PNoMetadata -> pdnothing
      _ -> perror

{- | Default implementation of 'Agora.Stake.Destroy'.

     @since 1.0.0
-}
pdestroy :: forall (s :: S). Term s PStakeRedeemerHandler
pdestroy = phoistAcyclic $
  plam $ \ctx -> unTermCont $ do
    ctxF <- pmatchC ctx

    pguardC "Owner signs this transaction" $
      pisSignedBy # pconstant True # pconstant False # ctx

    pguardC "Stake unlocked" $
      pnot #$ pany # pstakeLocked # ctxF.stakeInputDatums

    pure $ pconstant ()

{- | Default implementation of 'Agora.Stake.DepositWithdraw'.

     @since 1.0.0
-}
pdepositWithdraw :: forall (s :: S). Term s PStakeRedeemerHandler
pdepositWithdraw = phoistAcyclic $
  plam $ \ctx -> unTermCont $ do
    ctxF <- pmatchC ctx

    pguardC "Owner signs this transaction" $
      pisSignedBy # pconstant True # pconstant False # ctx

    ----------------------------------------------------------------------------

    stakeInputDatum <-
      pletC $
        ptrace "Single stake input" $
          pfromSingleton # ctxF.stakeInputDatums
    stakeInputDatumF <- pletAllC stakeInputDatum

    let stakeOutputDatum =
          ptrace "Single stake output" $
            pfromSingleton # ctxF.stakeOutputDatums

    ----------------------------------------------------------------------------

    pguardC "Stake unlocked" $
      pnot #$ pstakeLocked # stakeInputDatum

    ----------------------------------------------------------------------------

    PDepositWithdrawDelta delta <- pmatchC ctxF.redeemerContext

    newStakedAmount <- pletC $ stakeInputDatumF.stakedAmount + delta

    pguardC "Non-negative staked amount" $ zero #<= newStakedAmount

    let expectedDatum =
          mkRecordConstr
            PStakeDatum
            ( #stakedAmount .= pdata newStakedAmount
                .& #owner .= stakeInputDatumF.owner
                .& #delegatedTo .= stakeInputDatumF.delegatedTo
                .& #lockedBy .= stakeInputDatumF.lockedBy
            )

    pguardC "Valid output datum" $ expectedDatum #== stakeOutputDatum

    pure $ pconstant ()
