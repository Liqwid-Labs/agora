module Sample.Stake.Destroy (
  ParameterBundle (..),
  StakeInputParameters (..),
  StakeBurningParameters (..),
  LeftOverStakeMode (..),
  AuthorizedBy (..),
  Validity (..),
  destroy,
  mkTestTree,
  mkTotallyValid,
  oneStake,
  multipleStakes,
  stealSST,
  stealSST1,
  stealSST3,
  lockedStakes,
  authorizedByDelegatee,
  notAuthorized,
) where

import Agora.Proposal (ProposalId (..))
import Agora.Stake (
  ProposalLock (Created),
  StakeDatum (..),
  StakeRedeemer (Destroy),
 )
import Control.Exception (assert)
import Data.Maybe (catMaybes, fromJust)
import Data.Semigroup (stimesMonoid)
import Plutarch.Context (
  input,
  mint,
  normalizeValue,
  output,
  pubKey,
  script,
  signedWith,
  withDatum,
  withRedeemer,
  withRef,
  withValue,
 )
import Plutarch.Extra.AssetClass (assetClassValue)
import PlutusLedgerApi.V1 (
  Credential (PubKeyCredential),
  TxOutRef (TxOutRef),
 )
import PlutusLedgerApi.V2 (PubKeyHash)
import Sample.Proposal.Shared (stakeTxRef)
import Sample.Shared (
  minAda,
  signer2,
  stakeAssetClass,
  stakePolicy,
  stakeSymbol,
  stakeValidator,
  stakeValidatorHash,
 )
import Test.Specification (
  SpecificationTree,
  group,
  testPolicy,
  testValidator,
 )
import Test.Util (CombinableBuilder, mkMinting, mkSpending, pubKeyHashes)

data ParameterBundle = ParameterBundle
  { stakeInputParameters :: StakeInputParameters
  , stakeBurningParameters :: StakeBurningParameters
  , authorizedBy :: AuthorizedBy
  }

data StakeInputParameters = StakeInputParameters
  { numInputs :: Int
  , notUnlocked :: Bool
  }

data StakeBurningParameters = StakeBurningParameters
  { numBurnt :: Int
  , leftOverStakeMode :: Maybe LeftOverStakeMode
  }

data LeftOverStakeMode = OutputAsIs | CollectSSTInOneUTxO

data AuthorizedBy = Owner | Delegatee | NotAuthorized

data Validity = Validity
  { forStakePolicy :: Maybe Bool
  , forStakeValidator :: Bool
  }

--------------------------------------------------------------------------------

owner :: PubKeyHash
owner = pubKeyHashes !! 2

delegatee :: PubKeyHash
delegatee = pubKeyHashes !! 3

--------------------------------------------------------------------------------

mkStakeInputDatum :: StakeInputParameters -> StakeDatum
mkStakeInputDatum ps =
  StakeDatum
    { stakedAmount = 114514
    , owner = PubKeyCredential owner
    , delegatedTo = Just $ PubKeyCredential delegatee
    , lockedBy = [Created $ ProposalId 0 | ps.notUnlocked]
    }

mkStakeRef :: Int -> TxOutRef
mkStakeRef = TxOutRef stakeTxRef . fromIntegral

stakeRedeemer :: StakeRedeemer
stakeRedeemer = Destroy

--------------------------------------------------------------------------------

destroy :: forall b. CombinableBuilder b => ParameterBundle -> b
destroy ps =
  let stakeInputDatum = mkStakeInputDatum ps.stakeInputParameters

      sst = assetClassValue stakeAssetClass 1

      stakeUTxOTemplate =
        mconcat
          [ script stakeValidatorHash
          , withDatum stakeInputDatum
          , withValue $ normalizeValue $ sst <> minAda
          ]

      stakeInputBuilder =
        foldMap
          ( \i ->
              input $
                mconcat
                  [ stakeUTxOTemplate
                  , withRef $ mkStakeRef i
                  , withRedeemer stakeRedeemer
                  ]
          )
          [1 .. ps.stakeInputParameters.numInputs]

      withSSTsBurnt =
        mint $
          normalizeValue $
            assetClassValue stakeAssetClass $
              negate $
                fromIntegral ps.stakeBurningParameters.numBurnt

      ---

      leftOverStakes =
        ps.stakeInputParameters.numInputs
          - ps.stakeBurningParameters.numBurnt

      stealSSTs =
        case fromJust ps.stakeBurningParameters.leftOverStakeMode of
          OutputAsIs ->
            foldMap output $
              replicate
                leftOverStakes
                stakeUTxOTemplate
          CollectSSTInOneUTxO ->
            output $
              mconcat
                [ pubKey signer2
                , withValue $ stimesMonoid leftOverStakes sst
                ]

      stakeOutputBuilder =
        assert (leftOverStakes >= 0) $
          mconcat
            [ withSSTsBurnt
            , if leftOverStakes > 0
                then stealSSTs
                else mempty
            ]

      ---

      sigBuilder = case ps.authorizedBy of
        Owner -> signedWith owner
        Delegatee -> signedWith delegatee
        NotAuthorized -> mempty
   in mconcat
        [ stakeInputBuilder
        , stakeOutputBuilder
        , sigBuilder
        ]

--------------------------------------------------------------------------------

mkTestTree ::
  String ->
  ParameterBundle ->
  Validity ->
  SpecificationTree
mkTestTree name pb val = group name $ catMaybes [validator, policy]
  where
    spend = mkSpending destroy pb
    mint = mkMinting destroy pb
    validator =
      Just $
        testValidator
          val.forStakeValidator
          "stake validator"
          stakeValidator
          (mkStakeInputDatum pb.stakeInputParameters)
          stakeRedeemer
          (spend $ mkStakeRef 1)

    policy = case pb.stakeBurningParameters.numBurnt of
      0 -> Nothing
      _ ->
        Just $
          testPolicy
            (fromJust val.forStakePolicy)
            "stake policy"
            stakePolicy
            ()
            (mint stakeSymbol)

--------------------------------------------------------------------------------

mkTotallyValid :: Int -> ParameterBundle
mkTotallyValid numStakes =
  ParameterBundle
    { stakeInputParameters =
        StakeInputParameters
          { numInputs = numStakes
          , notUnlocked = False
          }
    , stakeBurningParameters =
        StakeBurningParameters
          { numBurnt = numStakes
          , leftOverStakeMode = Nothing
          }
    , authorizedBy = Owner
    }

oneStake :: ParameterBundle
oneStake = mkTotallyValid 1

multipleStakes :: ParameterBundle
multipleStakes = mkTotallyValid 10

stealSST :: ParameterBundle
stealSST =
  multipleStakes
    { stakeBurningParameters =
        StakeBurningParameters
          { numBurnt = 1
          , leftOverStakeMode = Just CollectSSTInOneUTxO
          }
    }

stealSST1 :: ParameterBundle
stealSST1 =
  multipleStakes
    { stakeBurningParameters =
        StakeBurningParameters
          { numBurnt = 0
          , leftOverStakeMode = Just CollectSSTInOneUTxO
          }
    }

stealSST3 :: ParameterBundle
stealSST3 =
  multipleStakes
    { stakeBurningParameters =
        StakeBurningParameters
          { numBurnt = 1
          , leftOverStakeMode = Just OutputAsIs
          }
    }

lockedStakes :: ParameterBundle
lockedStakes =
  multipleStakes
    { stakeInputParameters =
        multipleStakes.stakeInputParameters
          { notUnlocked = True
          }
    }

authorizedByDelegatee :: ParameterBundle
authorizedByDelegatee =
  multipleStakes
    { authorizedBy = Delegatee
    }

notAuthorized :: ParameterBundle
notAuthorized =
  multipleStakes
    { authorizedBy = NotAuthorized
    }
