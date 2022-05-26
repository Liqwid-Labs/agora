{-# LANGUAGE RecordWildCards #-}

module Bench (Benchmark (..), benchmarkScript, specificationTreeToBenchmarks) where

import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Csv (DefaultOrdered, ToNamedRecord, header, headerOrder, namedRecord, toNamedRecord, (.=))
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Api (
  ExBudget (ExBudget),
  ExCPU (..),
  ExMemory (..),
  Script,
 )
import Plutus.V1.Ledger.Api qualified as Plutus
import Prettyprinter (Pretty (pretty), indent, vsep)

import Spec.Specification (
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
    (ExBudget cpu mem) = evalScriptCounting . serialiseScriptShort $ script
    size = SBS.length . SBS.toShort . LBS.toStrict . serialise $ script

    serialiseScriptShort :: Script -> SBS.ShortByteString
    serialiseScriptShort = SBS.toShort . LBS.toStrict . serialise -- Using `flat` here breaks `evalScriptCounting`
    evalScriptCounting :: Plutus.SerializedScript -> Plutus.ExBudget
    evalScriptCounting script =
      let costModel = fromJust Plutus.defaultCostModelParams
          (_logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose costModel script []
       in case e of
            Left evalError -> error ("Eval Error: " <> show evalError)
            Right exbudget -> exbudget

specificationTreeToBenchmarks :: SpecificationTree -> [Benchmark]
specificationTreeToBenchmarks = go []
  where
    go names (Terminal ((Specification n ex s))) = case ex of
      Success -> [benchmarkScript (intercalate "/" (names <> [n])) s]
      _ -> []
    go names (Group gn tree) = mconcat $ go (names <> [gn]) <$> tree
