{- |
Module     : Property.Governor
Maintainer : seungheon.ooh@gmail.com
Description: Property tests for 'Governor' related functions.

Property model and tests for 'Governor' related functions
-}
module Property.Governor (props) where

import Agora.Governor (
  GovernorDatum (GovernorDatum, proposalThresholds),
  governorDatumValid,
 )
import Agora.Proposal (
  ProposalId (ProposalId),
  ProposalThresholds (ProposalThresholds),
 )
import Agora.Proposal.Time (
  MaxTimeRangeWidth (MaxTimeRangeWidth),
  ProposalTimingConfig (ProposalTimingConfig),
 )
import Data.Tagged (Tagged (Tagged), untag)
import Data.Universe (Finite (..), Universe (..))
import Test.Tasty (TestTree)
import Test.Tasty.Plutarch.Property (classifiedPropertyNative)
import Test.Tasty.QuickCheck (
  Gen,
  Property,
  chooseInteger,
  testProperty,
 )

data GovernorDatumCases
  = ExecuteLE0
  | CreateLE0
  | VoteLE0
  | CreateLEVote
  | ExecuteLVote
  | Correct
  deriving stock (Eq, Show)

instance Universe GovernorDatumCases where
  universe =
    [ ExecuteLE0
    , CreateLE0
    , VoteLE0
    , CreateLEVote
    , ExecuteLVote
    , Correct
    ]

instance Finite GovernorDatumCases where
  universeF = universe
  cardinality = Tagged 6

{- | Property that checks `governorDatumValid`.
 `governorDatumValid` determines if given governor datum is valid or not. This property
 ensures `governorDatumValid` is checking the datum correctly and ruling out improper datum.
-}
governorDatumValidProperty :: Property
governorDatumValidProperty =
  classifiedPropertyNative gen (const []) expected classifier governorDatumValid
  where
    classifier :: GovernorDatum -> GovernorDatumCases
    classifier (proposalThresholds -> ProposalThresholds e c v)
      | e < 0 = ExecuteLE0
      | c < 0 = CreateLE0
      | v < 0 = VoteLE0
      | c > v = CreateLEVote
      | v >= e = ExecuteLVote
      | otherwise = Correct

    expected :: GovernorDatum -> Maybe Bool
    expected c = Just $ classifier c == Correct

    gen :: GovernorDatumCases -> Gen GovernorDatum
    gen c = do
      thres <- genProposalThresholds c

      let timing = ProposalTimingConfig 0 0 0 0
      return $ GovernorDatum thres (ProposalId 0) timing (MaxTimeRangeWidth 0)
      where
        taggedInteger p = Tagged <$> chooseInteger p
        genProposalThresholds :: GovernorDatumCases -> Gen ProposalThresholds
        genProposalThresholds c = do
          let validGT = taggedInteger (0, 1000000000)
          execute <- validGT
          create <- validGT
          vote <- validGT
          le0 <- taggedInteger (-1000, -1)

          case c of
            ExecuteLE0 ->
              -- execute < 0
              return $ ProposalThresholds le0 create vote
            CreateLE0 ->
              -- c < 0
              return $ ProposalThresholds execute le0 vote
            VoteLE0 ->
              -- vote < 0
              return $ ProposalThresholds execute create le0
            CreateLEVote -> do
              -- c > vote
              nv <- taggedInteger (0, untag create - 1)
              ne <- taggedInteger (untag nv + 1, 1000000000)
              return $ ProposalThresholds ne create nv
            ExecuteLVote -> do
              -- vote >= execute
              ne <- taggedInteger (0, untag vote)
              nc <- taggedInteger (0, untag vote)
              return $ ProposalThresholds ne nc vote
            Correct -> do
              -- c <= vote < execute
              nv <- taggedInteger (0, untag execute - 1)
              nc <- taggedInteger (0, untag nv)
              return $ ProposalThresholds execute nc nv

props :: [TestTree]
props =
  [ testProperty "governorDatumValid" governorDatumValidProperty
  ]
