module Main (main) where

import Prelude

--------------------------------------------------------------------------------

import Plutarch.Benchmark
import Plutus.V1.Ledger.Value qualified as Value

--------------------------------------------------------------------------------

import Agora.AuthorityToken qualified as Agora

--------------------------------------------------------------------------------

main :: IO ()
main = do
  benchMain benchmarks

benchmarks :: [NamedBenchmark]
benchmarks =
  benchGroup
    "full_scripts"
    [ bench "authorityTokenPolicy" $ Agora.authorityTokenPolicy (Agora.AuthorityToken (Value.assetClass "" ""))
    ]
