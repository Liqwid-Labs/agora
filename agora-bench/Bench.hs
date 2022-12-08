{-# LANGUAGE RecordWildCards #-}

module Bench (Benchmark (..), benchmarkScript, specificationTreeToBenchmarks) where

import Data.ByteString.Short qualified as SBS
import Data.Csv (DefaultOrdered, ToNamedRecord, header, headerOrder, namedRecord, toNamedRecord, (.=))
import Data.List (intercalate)
import Data.Text (Text, pack)
import Plutarch.Evaluate (evalScript)
import Plutarch.Script (Script, serialiseScript)
import PlutusLedgerApi.V2 (
  ExBudget (ExBudget),
  ExCPU (..),
  ExMemory (..),
 )
import Prettyprinter (Pretty (pretty), indent, vsep)
import Test.Specification (
  Specification (Specification),
  SpecificationExpectation (Success),
  SpecificationTree (..),
 )

--------------------------------------------------------------------------------

-- | Represents the benchmark of a plutus script.
data Benchmark = Benchmark
  { name :: Text
  -- ^ Human readable name describing script.
  , cpuBudget :: ExCPU
  -- ^ The on-chain execution cost of a script.
  , memoryBudget :: ExMemory
  -- ^ The on-chain memory budget of a script.
  , scriptSize :: Int
  -- ^ The on-chain size of a script.
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Pretty Benchmark where
  pretty (Benchmark name (ExCPU (toInteger -> cpu)) (ExMemory (toInteger -> mem)) size) =
    vsep
      [ pretty name
      , indent 4 $
          vsep
            [ "CPU: " <> pretty cpu
            , "MEM: " <> pretty mem
            , "SIZE: " <> pretty size
            ]
      ]

instance ToNamedRecord Benchmark where
  toNamedRecord (Benchmark {..}) =
    namedRecord
      [ "name" .= name
      , "cpu" .= cpuBudget
      , "mem" .= memoryBudget
      , "size" .= scriptSize
      ]

instance DefaultOrdered Benchmark where
  headerOrder _ = header ["name", "cpu", "mem", "size"]

benchmarkScript :: String -> Script -> Benchmark
benchmarkScript name script = Benchmark (pack name) cpu mem size
  where
    (_res, ExBudget cpu mem, _traces) = evalScript script

    size = SBS.length . serialiseScript $ script

specificationTreeToBenchmarks :: SpecificationTree -> [Benchmark]
specificationTreeToBenchmarks = go []
  where
    go names (Terminal ((Specification n ex s))) = case ex of
      Success -> [benchmarkScript (intercalate "/" (names <> [n])) s]
      _ -> []
    go names (Group gn tree) = mconcat $ go (names <> [gn]) <$> tree
