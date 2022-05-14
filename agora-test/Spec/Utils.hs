{- |
Module     : Spec.Utils
Maintainer : emi@haskell.fyi
Description: Tests for utility functions in 'Agora.Utils'.

Tests for utility functions in 'Agora.Utils'.
-}
module Spec.Utils (tests) where

import Agora.Utils (phalve, pmerge, pmsortOrd, tcmatch)
import Data.List (sort)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

tests :: [TestTree]
tests =
  [ testProperty "Merge sort sorts a list properly" prop_msort_sorted
  , testProperty "Two sorted lists are merged into one sorted list" prop_pmerge_sorted
  , testProperty "Split a list in half as expected" prop_halve_properly
  ]

--------------------------------------------------------------------------------

prop_msort_sorted :: [Integer] -> Bool
prop_msort_sorted arr = sort arr == sorted
  where
    parr :: Term _ (PBuiltinList PInteger)
    parr = pconstant arr

    psorted :: Term _ (PBuiltinList PInteger)
    psorted = pmsortOrd # parr

    sorted :: [Integer]
    sorted = plift psorted

prop_pmerge_sorted :: ([Integer], [Integer]) -> Bool
prop_pmerge_sorted (a, b) = merge sa sb == merged
  where
    sa = sort a
    sb = sort b

    merge xs [] = xs
    merge [] ys = ys
    merge sx@(x : xs) sy@(y : ys)
      | x <= y = x : merge xs sy
      | otherwise = y : merge sx ys

    psa :: Term _ (PBuiltinList PInteger)
    psa = pconstant @(PBuiltinList PInteger) sa
    psb :: Term _ (PBuiltinList PInteger)
    psb = pconstant @(PBuiltinList PInteger) sb

    pmerged :: Term _ (PBuiltinList PInteger)
    pmerged = pmerge # plam (#<) # psa # psb

    merged :: [Integer]
    merged = plift pmerged

prop_halve_properly :: [Integer] -> Bool
prop_halve_properly arr = halve arr == halved
  where
    halve xs = go xs xs
      where
        go xs [] = ([], xs)
        go (x : xs) [_] = ([x], xs)
        go (x : xs) (_ : _ : ys) =
          let (first, last) =
                go xs ys
           in (x : first, last)
        go [] _ = ([], [])

    parr :: Term _ (PBuiltinList PInteger)
    parr = pconstant arr

    ppairFst :: Term _ (PPair a b :--> a)
    ppairFst = phoistAcyclic $
      plam $ \p -> unTermCont $ do
        PPair x _ <- tcmatch p
        return x

    ppairSnd :: Term _ (PPair a b :--> b)
    ppairSnd = phoistAcyclic $
      plam $ \p -> unTermCont $ do
        PPair _ y <- tcmatch p
        return y

    phalved :: Term _ (PPair (PBuiltinList PInteger) (PBuiltinList PInteger))
    phalved = phalve # parr

    halved :: ([Integer], [Integer])
    halved =
      let f = plift $ ppairFst # phalved
          s = plift $ ppairSnd # phalved
       in (f, s)
