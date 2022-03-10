module Main (main) where

import Prelude

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Value qualified as Value

--------------------------------------------------------------------------------

import Plutarch.Benchmark

--------------------------------------------------------------------------------

import Agora.AuthorityToken (
  AuthorityToken (AuthorityToken),
  authorityTokenPolicy,
 )
import Agora.SafeMoney (LQ)
import Agora.Stake (
  Stake (Stake),
  stakePolicy,
  stakeValidator,
 )

--------------------------------------------------------------------------------

main :: IO ()
main = do
  benchMain benchmarks

benchmarks :: [NamedBenchmark]
benchmarks =
  benchGroup
    "full_scripts"
    [ bench "authorityTokenPolicy" $ authorityTokenPolicy authorityToken
    , bench "stakePolicy" $ stakePolicy (Stake @LQ)
    , bench "stakeValidator" $ stakeValidator (Stake @LQ)
    ]

authorityToken :: AuthorityToken
authorityToken = AuthorityToken (Value.assetClass "" "")
