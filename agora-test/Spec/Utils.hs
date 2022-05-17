{- |
Module     : Spec.Utils
Maintainer : emi@haskell.fyi
Description: Tests for utility functions in 'Agora.Utils'.

Tests for utility functions in 'Agora.Utils'.
-}
module Spec.Utils (tests) where

--------------------------------------------------------------------------------

import Agora.Utils (phalve, pisUniq, pmergeBy, pmsort, pnubSort)

--------------------------------------------------------------------------------

import Data.List (nub, sort)
import Data.Set as S

--------------------------------------------------------------------------------

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
  [ testProperty "'pmsort' sorts a list properly" prop_msortCorrect
  , testProperty "'pmerge' merges two sorted lists into one sorted list" prop_mergeCorrect
  , testProperty "'phalve' splits a list in half as expected" prop_halveCorrect
  , testProperty "'pnubSort' sorts a list and remove duplicate elements" prop_nubSortProperly
  , testProperty "'pisUniq' can tell whether all elements in a list are unique" prop_uniqueList
  ]

--------------------------------------------------------------------------------

-- | Yield true if 'Agora.Utils.pmsort' sorts a given list correctly.
prop_msortCorrect :: [Integer] -> Bool
prop_msortCorrect l = sorted == expected
  where
    -- Expected sorted list, using 'Data.List.sort'.
    expected :: [Integer]
    expected = sort l

    --

    psorted :: Term _ (PBuiltinList PInteger)
    psorted = pmsort # pconstant l

    sorted :: [Integer]
    sorted = plift psorted

-- | Yield true if 'Agora.Utils.pmerge' merges two list into a ordered list correctly.
prop_mergeCorrect :: [Integer] -> [Integer] -> Bool
prop_mergeCorrect a b = merged == expected
  where
    -- Sorted list a and b
    sa = sort a
    sb = sort b

    -- Merge two lists which are assumed to be ordered.
    merge :: [Integer] -> [Integer] -> [Integer]
    merge xs [] = xs
    merge [] ys = ys
    merge sx@(x : xs) sy@(y : ys)
      | x <= y = x : merge xs sy
      | otherwise = y : merge sx ys

    expected :: [Integer]
    expected = merge sa sb

    --

    pmerged :: Term _ (PBuiltinList PInteger)
    pmerged = pmergeBy # plam (#<) # pconstant sa # pconstant sb

    merged :: [Integer]
    merged = plift pmerged

{- | Yield true if Plutarch level 'Agora.Utils.phalve' splits a given list
   as its Haskell level counterpart does.
-}
prop_halveCorrect :: [Integer] -> Bool
prop_halveCorrect l = halved == expected
  where
    -- Halve a list.
    halve :: [Integer] -> ([Integer], [Integer])
    halve xs = go xs xs
      where
        go xs [] = ([], xs)
        go (x : xs) [_] = ([x], xs)
        go (x : xs) (_ : _ : ys) =
          let (first, last) =
                go xs ys
           in (x : first, last)
        go [] _ = ([], [])

    expected :: ([Integer], [Integer])
    expected = halve l

    --

    phalved :: Term _ (PPair (PBuiltinList PInteger) (PBuiltinList PInteger))
    phalved = phalve # pconstant l

    halved :: ([Integer], [Integer])
    halved =
      let f = plift $ pmatch phalved $ \(PPair x _) -> x
          s = plift $ pmatch phalved $ \(PPair _ x) -> x
       in (f, s)

{- | Yield true if 'Agora.Utils.pnubSort' sorts and removes
   duplicate elements from a given list.
-}
prop_nubSortProperly :: [Integer] -> Bool
prop_nubSortProperly l = nubbed == expected
  where
    -- Sort and list and then nub it.
    expected :: [Integer]
    expected = nub $ sort l

    --

    pnubbed :: Term _ (PBuiltinList PInteger)
    pnubbed = pnubSort # pconstant l

    nubbed :: [Integer]
    nubbed = plift pnubbed

{- | Yield true if 'Agora.Utils.isUnique' can correctly determine
   whether a given list only contains unique elements or not.
-}
prop_uniqueList :: [Integer] -> Bool
prop_uniqueList l = isUnique == expected
  where
    -- Convert input list to a set.
    -- If the set's size equals to list's size,
    --   the list only contains unique elements.
    expected :: Bool
    expected = S.size (S.fromList l) == length l

    --

    isUnique = plift $ pisUniq # pconstant l
