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

import Agora.Proposal (
  PProposalId,
  PProposalRedeemer (PCosign, PUnlockStake, PVote),
  ProposalStatus (Finished),
 )
import Agora.Proposal.Time (PProposalTime)
import Agora.Stake (
  PProposalAction (PCosigned, PCreated, PVoted),
  PProposalContext (
    PNewProposal,
    PNoProposal,
    PSpendProposal
  ),
  PProposalLock (PProposalLock),
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
import Data.Functor ((<&>))
import Plutarch.Api.V1.Address (PCredential)
import Plutarch.Api.V2 (PMaybeData, PPOSIXTime)
import Plutarch.Extra.Bool (passert)
import Plutarch.Extra.Field (pletAll, pletAllC)
import "liqwid-plutarch-extra" Plutarch.Extra.List (
  pisSingleton,
  ptryDeleteFirstBy,
  ptryFromSingleton,
 )
import Plutarch.Extra.Maybe (pdjust, pdnothing, pjust, pmaybe, pmaybeData, pnothing)
import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC)
import Plutarch.Extra.Time (PCurrentTime (PCurrentTime))

-- | A wrapper which ensures that no proposal is presented in the transaction.
pwithoutProposal ::
  forall (s :: S).
  Term
    s
    (PStakeRedeemerHandler :--> PStakeRedeemerHandler)
pwithoutProposal = phoistAcyclic $
  plam $ \f ctx -> pmatch ctx $ \ctxF ->
    pif
      ( pmatch ctxF.proposalContext $ \case
          PNoProposal -> pconstant True
          _ -> pconstant False
      )
      (f # ctx)
      (ptraceError "No proposal is allowed")

{- | Validate stake outputs given a function that converts an input stake datum
      to an ouput stake datum. / O(n^2) /.
-}
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
    pnull
      #$ pfoldr
      # plam (\x -> ptryDeleteFirstBy # (f # x))
      # ctxF.stakeOutputDatums
      # ctxF.stakeInputDatums

-- | Extract the 'PSigContext.signedBy' field from 'PStakeRedeemerHandlerContext'.
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

-- | Return true if the tx is authorized by either the owner or the delegatee.
pisSignedBy ::
  forall (s :: S).
  Term
    s
    (PBool :--> PStakeRedeemerHandlerContext :--> PBool)
pisSignedBy = phoistAcyclic $
  plam $ \byDelegate ctx ->
    pmatch (pgetSignedBy # ctx) $ \case
      PSignedByOwner -> pconstant True
      PSignedByDelegate -> byDelegate
      PUnknownSig -> pconstant False

-- | Return true if only the @lockedBy@ field of the stake datum is updated.
ponlyLocksUpdated ::
  forall (s :: S).
  Term
    s
    ( ( PBuiltinList (PAsData PProposalLock)
          :--> PBuiltinList (PAsData PProposalLock)
      )
        :--> PStakeRedeemerHandlerContext
        :--> PBool
    )
ponlyLocksUpdated = phoistAcyclic $
  plam $ \f ->
    pbatchUpdateInputs #$ plam $ \i o ->
      pletAll i $ \iF ->
        let newLocks = f # pfromData iF.lockedBy

            expected =
              mkRecordConstr
                PStakeDatum
                ( #stakedAmount
                    .= iF.stakedAmount
                    .& #owner
                    .= iF.owner
                    .& #delegatedTo
                    .= iF.delegatedTo
                    .& #lockedBy
                    .= pdata newLocks
                )
         in expected #== o

-- | Validation logic shared between 'ppermitVote' and 'retractVote'.
pvoteHelper ::
  forall (s :: S).
  Term
    s
    ( ( PStakeRedeemerHandlerContext
          :--> PBuiltinList (PAsData PProposalLock)
          :--> PBuiltinList (PAsData PProposalLock)
      )
        :--> PStakeRedeemerHandler
    )
pvoteHelper = phoistAcyclic $
  plam $ \valProposalCtx ctx ->
    -- This puts trust into the Proposal. The Proposal must necessarily check
    -- that this is not abused.
    passert
      "Correct outputs"
      (ponlyLocksUpdated # (valProposalCtx # ctx) # ctx)
      (pconstant ())

-- | Add new lock the the existing list of locked.
paddNewLock ::
  forall (s :: S).
  Term
    s
    ( PProposalLock
        :--> PBuiltinList (PAsData PProposalLock)
        :--> PBuiltinList (PAsData PProposalLock)
    )
paddNewLock = phoistAcyclic $
  plam $
    -- Prepend the lock.
    \newLock -> pcons # pdata newLock

{- | Default implementation of 'Agora.Stake.PermitVote'.

     @since 1.0.0
-}
ppermitVote :: forall (s :: S). Term s PStakeRedeemerHandler
ppermitVote = pvoteHelper #$ phoistAcyclic $
  plam $ \ctx -> unTermCont $ do
    ctxF <- pmatchC ctx

    withOnlyOneStakeInput <- pletC $
      plam $ \lock -> unTermCont $ do
        pguardC "Only one stake input allowed" $
          pisSingleton # ctxF.stakeInputDatums

        pguardC "Owner signs this transaction" $
          pisSignedBy # pconstant False # ctx

        pure lock

    pure $
      paddNewLock #$ pmatch ctxF.proposalContext $ \case
        PSpendProposal proposal redeemer currentTime -> unTermCont $ do
          mkLock <- pletC $
            plam $ \action ->
              mkRecordConstr
                PProposalLock
                ( #proposalId
                    .= pfield @"proposalId"
                    # proposal
                    .& #action
                    .= pdata action
                )

          pure $
            pmatch redeemer $ \case
              PVote ((pfromData . (pfield @"resultTag" #)) -> voteFor) ->
                unTermCont $ do
                  pguardC "Owner or delegatee signs the transaction" $
                    pisSignedBy # pconstant True # ctx

                  PCurrentTime _ upperBound <- pmatchC currentTime

                  let action =
                        mkRecordConstr
                          PVoted
                          ( #votedFor
                              .= pdata voteFor
                              .& #createdAt
                              .= pdata upperBound
                          )

                  pure $ mkLock # action
              PCosign _ ->
                let action = pcon $ PCosigned pdnil
                 in withOnlyOneStakeInput #$ mkLock # action
              _ -> ptraceError "Expected Vote or Cosign"
        PNewProposal proposalId ->
          let action = pcon $ PCreated pdnil
              lock =
                mkRecordConstr
                  PProposalLock
                  ( #proposalId
                      .= pdata proposalId
                      .& #action
                      .= pdata action
                  )
           in withOnlyOneStakeInput # lock
        _ -> ptraceError "Expected a proposal to be spent or created"

data PRemoveLocksMode (s :: S) = PRemoveVoterLockOnly | PRemoveAllLocks
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PRemoveLocksMode where
  type DPTStrat _ = PlutusTypeScott

{- | Remove stake locks with the proposal id given the list of existing locks.
     The first parameter controls whether to remove creator locks or not. If
     one of the locks performed voting action, the unlock cooldown will be
     checked if it's given.
-}
premoveLocks ::
  forall (s :: S).
  Term
    s
    ( PProposalId
        :--> PMaybe PPOSIXTime
        :--> PProposalTime
        :--> PRemoveLocksMode
        :--> PBuiltinList (PAsData PProposalLock)
        :--> PBuiltinList (PAsData PProposalLock)
    )
premoveLocks =
  phoistAcyclic $
    plam $ \proposalId unlockCooldown currentTime mode -> unTermCont $ do
      shouldRemoveAllLocks <- pletC $ mode #== pcon PRemoveAllLocks

      PCurrentTime lowerBound _ <- pmatchC currentTime

      let handleVoter
            ( (pfield @"createdAt" #) ->
                createdAt
              ) =
              let notInCooldown =
                    pmaybe
                      # pconstant True
                      # plam (\c -> createdAt + c #<= lowerBound)
                      # unlockCooldown
               in foldl1
                    (#||)
                    [ shouldRemoveAllLocks
                    , ptraceIfFalse "Stake lock in cooldown" notInCooldown
                    ]

          handleLock =
            plam $
              flip
                pletAll
                ( \lockF ->
                    foldl1
                      (#&&)
                      [ proposalId #== lockF.proposalId
                      , pmatch lockF.action $ \case
                          PVoted r -> handleVoter r
                          _ -> shouldRemoveAllLocks
                      ]
                )
                . pfromData

      pure $ pfilter # handleLock

{- | Default implementation of 'Agora.Stake.RetractVotes'.

     @since 1.0.0
-}
pretractVote :: forall (s :: S). Term s PStakeRedeemerHandler
pretractVote = pvoteHelper #$ phoistAcyclic $
  plam $ \ctx ->
    pmatch ctx $ \ctxF ->
      pmatch ctxF.proposalContext $ \case
        PSpendProposal proposal redeemer currentTime -> pmatch redeemer $ \case
          PUnlockStake _ -> unTermCont $ do
            proposalF <-
              pletFieldsC
                @'[ "proposalId"
                  , "status"
                  , "timingConfig"
                  ]
                proposal

            (mode, unlockCooldown) <-
              pmatchC (proposalF.status #== pconstant Finished) <&> \case
                PTrue ->
                  ( pcon PRemoveAllLocks
                  , pnothing
                  )
                _ ->
                  ( pcon PRemoveVoterLockOnly
                  , pjust
                      #$ pfield @"minStakeVotingTime"
                      # proposalF.timingConfig
                  )

            pguardC "Authorized by either opwner or delegatee" $
              pisSignedBy # pconstant True # ctx

            pure $
              premoveLocks
                # proposalF.proposalId
                # unlockCooldown
                # currentTime
                # mode
          _ -> ptraceError "Expected unlock"
        _ -> ptraceError "Expected spending proposal"

-- | Validation logic shared by 'pdelegateTo' and 'pclearDelegate'.
pdelegateHelper ::
  forall (s :: S).
  Term
    s
    ( (PStakeRedeemerContext :--> PMaybeData (PAsData PCredential))
        :--> PStakeRedeemerHandler
    )
pdelegateHelper = phoistAcyclic $
  plam $ \f -> pwithoutProposal #$ plam $ \ctx -> unTermCont $ do
    ctxF <- pmatchC ctx
    sigCtxF <- pmatchC ctxF.sigContext

    pguardC "Owner signs this transaction" $
      pisSignedBy # pconstant False # ctx

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
                ( #stakedAmount
                    .= iF.stakedAmount
                    .& #owner
                    .= iF.owner
                    .& #delegatedTo
                    .= pdata newDelegate
                    .& #lockedBy
                    .= iF.lockedBy
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
  pwithoutProposal #$ plam $ \ctx -> unTermCont $ do
    ctxF <- pmatchC ctx

    pguardC "Owner signs this transaction" $
      pisSignedBy # pconstant False # ctx

    pguardC "All stakes unlocked" $
      pnot #$ pany # pstakeLocked # ctxF.stakeInputDatums

    pguardC "All stakes burnt" $
      pnull # ctxF.stakeOutputDatums

    pure $ pconstant ()

{- | Default implementation of 'Agora.Stake.DepositWithdraw'.

     @since 1.0.0
-}
pdepositWithdraw :: forall (s :: S). Term s PStakeRedeemerHandler
pdepositWithdraw = phoistAcyclic $
  pwithoutProposal #$ plam $ \ctx -> unTermCont $ do
    ctxF <- pmatchC ctx

    pguardC "Owner signs this transaction" $
      pisSignedBy # pconstant False # ctx

    ----------------------------------------------------------------------------

    stakeInputDatum <-
      pletC $
        ptrace "Single stake input" $
          ptryFromSingleton # ctxF.stakeInputDatums
    stakeInputDatumF <- pletAllC stakeInputDatum

    let stakeOutputDatum =
          ptrace "Single stake output" $
            ptryFromSingleton # ctxF.stakeOutputDatums

    ----------------------------------------------------------------------------

    pguardC "Stake unlocked" $
      pnot #$ pstakeLocked # stakeInputDatum

    ----------------------------------------------------------------------------

    PDepositWithdrawDelta delta <- pmatchC ctxF.redeemerContext

    newStakedAmount <- pletC $ stakeInputDatumF.stakedAmount + delta

    pguardC "Non-negative staked amount" $ 0 #<= newStakedAmount

    let expectedDatum =
          mkRecordConstr
            PStakeDatum
            ( #stakedAmount
                .= pdata newStakedAmount
                .& #owner
                .= stakeInputDatumF.owner
                .& #delegatedTo
                .= stakeInputDatumF.delegatedTo
                .& #lockedBy
                .= stakeInputDatumF.lockedBy
            )

    pguardC "Valid output datum" $ expectedDatum #== stakeOutputDatum

    pure $ pconstant ()
