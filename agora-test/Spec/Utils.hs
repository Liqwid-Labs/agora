{- |
Module     : Spec.Utils
Maintainer : emi@haskell.fyi
Description: Tests for utility functions in 'Agora.Utils'.

Tests for utility functions in 'Agora.Utils'.
-}
module Spec.Utils (tests) where

import Agora.Utils (phalve, pmergeBy, pmsort)
import Data.List (sort)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

tests :: [TestTree]
tests =
  [ testProperty "Merge sort sorts a list properly" prop_msortSorted
  , testProperty "Two sorted lists are merged into one sorted list" prop_pmergeSorted
  , testProperty "Split a list in half as expected" prop_halveProperly
  ]

--------------------------------------------------------------------------------

prop_msortSorted :: [Integer] -> Bool
prop_msortSorted arr = sorted == expected
  where
    -- Expected sorted list, using 'Data.List.sort'.
    expected :: [Integer]
    expected = sort arr

    --

    psorted :: Term _ (PBuiltinList PInteger)
    psorted = pmsort # pconstant arr

    sorted :: [Integer]
    sorted = plift psorted

prop_pmergeSorted :: [Integer] -> [Integer] -> Bool
prop_pmergeSorted a b = merged == expected
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

prop_halveProperly :: [Integer] -> Bool
prop_halveProperly arr = halved == expected
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
    expected = halve arr

    --

    phalved :: Term _ (PPair (PBuiltinList PInteger) (PBuiltinList PInteger))
    phalved = phalve # pconstant arr

    halved :: ([Integer], [Integer])
    halved =
      let f = plift $ pmatch phalved $ \(PPair x _) -> x
          s = plift $ pmatch phalved $ \(PPair _ x) -> x
       in (f, s)
