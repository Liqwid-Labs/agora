{-# LANGUAGE ExistentialQuantification #-}

module Sample.Stake.Create (
  StakeDatumWrapper (..),
  Parameters (..),
  create,
  mkTestCase,
  ownerIsPubKeyTotallyValid,
  ownerIsScriptTotallyValid,
  createMoreThanOneStake,
  spendStake,
  unexpectedStakedAmount,
  noStakeDatum,
  malformedStakeDatum,
  notAuthorizedByOwner,
  setDelegatee,
  alreadyHasLocks,
) where

import Agora.Governor (Governor (gtClassRef))
import Agora.Proposal (ProposalId (ProposalId))
import Agora.SafeMoney (GTTag)
import Agora.Stake (ProposalAction (Created), ProposalLock (ProposalLock), StakeDatum (..))
import Data.Semigroup (stimesMonoid)
import Data.Tagged (Tagged)
import Plutarch.Context (
  input,
  mint,
  normalizeValue,
  output,
  pubKey,
  script,
  signedWith,
  withDatum,
  withValue,
 )
import Plutarch.Extra.AssetClass (assetClassValue)
import Plutarch.Extra.ScriptContext (validatorHashToTokenName)
import Plutarch.Lift (PUnsafeLiftDecl (PLifted))
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 (
  Credential (
    PubKeyCredential,
    ScriptCredential
  ),
 )
import Sample.Shared (
  governor,
  signer,
  signer2,
  stakePolicy,
  stakeSymbol,
  stakeValidatorHash,
 )
import Test.Specification (SpecificationTree, testPolicy)
import Test.Util (CombinableBuilder, mkMinting, validatorHashes)

data StakeDatumWrapper
  = forall (b :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    StakeDatumWrapper b

data Parameters = Parameters
  { numSSTMinted :: Integer
  , invalidSSTName :: Bool
  , stakeAtInput :: Bool
  , numGTsInValue :: Tagged GTTag Integer
  , stakeDatum :: Maybe StakeDatumWrapper
  , authorizedBy :: Maybe Credential
  }

create :: forall b. CombinableBuilder b => Parameters -> b
create ps@Parameters {stakeDatum} =
  let perStakeGTs =
        assetClassValue
          governor.gtClassRef
          ps.numGTsInValue

      gtValue =
        stimesMonoid ps.numSSTMinted perStakeGTs

      gtInputBuilder =
        mconcat
          [ input $
              mconcat
                [ pubKey signer
                , withValue $ normalizeValue gtValue
                ]
          ]

      ---

      sstName =
        if ps.invalidSSTName
          then "114514"
          else validatorHashToTokenName stakeValidatorHash

      sst = Value.singleton stakeSymbol sstName 1

      withStakeDatum =
        maybe
          mempty
          (\(StakeDatumWrapper stakeDatum) -> withDatum stakeDatum)
          stakeDatum

      stakeBuilder =
        mconcat
          [ script stakeValidatorHash
          , withValue $ normalizeValue $ sst <> perStakeGTs
          , withStakeDatum
          ]

      stakeInputBuilder =
        if ps.stakeAtInput
          then input stakeBuilder
          else mempty

      stakeOutputBuilder =
        stimesMonoid ps.numSSTMinted $
          output stakeBuilder

      ---

      withAuthorization =
        maybe
          mempty
          ( \case
              PubKeyCredential pkh -> signedWith pkh
              ScriptCredential val -> input $ script val
          )
          ps.authorizedBy

      ---

      mintSSTs = mint $ stimesMonoid ps.numSSTMinted sst
   in mconcat
        [ gtInputBuilder
        , stakeInputBuilder
        , stakeOutputBuilder
        , withAuthorization
        , mintSSTs
        ]

mkTestCase :: String -> Parameters -> Bool -> SpecificationTree
mkTestCase name ps val = stake
  where
    mint = mkMinting create ps

    stake =
      testPolicy
        val
        name
        stakePolicy
        ()
        (mint stakeSymbol)

mkTotallyValid :: Integer -> Credential -> Parameters
mkTotallyValid gts owner =
  Parameters
    { numSSTMinted = 1
    , invalidSSTName = False
    , numGTsInValue = fromInteger gts
    , stakeAtInput = False
    , stakeDatum =
        Just $
          StakeDatumWrapper $
            StakeDatum
              { stakedAmount = fromInteger gts
              , owner = owner
              , delegatedTo = Nothing
              , lockedBy = []
              }
    , authorizedBy = Just owner
    }

ownerIsPubKeyTotallyValid :: Parameters
ownerIsPubKeyTotallyValid = mkTotallyValid 114514 (PubKeyCredential signer)

ownerIsScriptTotallyValid :: Parameters
ownerIsScriptTotallyValid =
  mkTotallyValid
    114514
    ( ScriptCredential $
        head validatorHashes
    )

createMoreThanOneStake :: Parameters
createMoreThanOneStake =
  ownerIsPubKeyTotallyValid
    { numSSTMinted = 5
    }

spendStake :: Parameters
spendStake =
  ownerIsPubKeyTotallyValid
    { stakeAtInput = True
    }

unexpectedStakedAmount :: Parameters
unexpectedStakedAmount =
  ownerIsPubKeyTotallyValid
    { numGTsInValue = 114514
    , stakeDatum =
        Just $
          StakeDatumWrapper $
            StakeDatum
              { stakedAmount = 1919810
              , owner = PubKeyCredential signer
              , delegatedTo = Nothing
              , lockedBy = []
              }
    }

noStakeDatum :: Parameters
noStakeDatum =
  ownerIsPubKeyTotallyValid
    { stakeDatum = Nothing
    }

malformedStakeDatum :: Parameters
malformedStakeDatum =
  ownerIsPubKeyTotallyValid
    { stakeDatum = Just $ StakeDatumWrapper (1 :: Integer)
    }

notAuthorizedByOwner :: Parameters
notAuthorizedByOwner =
  ownerIsPubKeyTotallyValid
    { authorizedBy = Nothing
    }

setDelegatee :: Parameters
setDelegatee =
  ownerIsPubKeyTotallyValid
    { numGTsInValue = 114514
    , stakeDatum =
        Just $
          StakeDatumWrapper $
            StakeDatum
              { stakedAmount = 114514
              , owner = PubKeyCredential signer
              , delegatedTo = Just $ PubKeyCredential signer2
              , lockedBy = []
              }
    }

alreadyHasLocks :: Parameters
alreadyHasLocks =
  ownerIsPubKeyTotallyValid
    { numGTsInValue = 114514
    , stakeDatum =
        Just $
          StakeDatumWrapper $
            StakeDatum
              { stakedAmount = 114514
              , owner = PubKeyCredential signer
              , delegatedTo = Nothing
              , lockedBy = [ProposalLock (ProposalId 0) Created]
              }
    }
