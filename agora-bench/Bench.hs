module Bench (Benchmark (..), benchmarkScript, specificationTreeToBenchmarks) where

import Codec.Serialise (serialise)
import Data.Aeson hiding (Success)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Plutus.V1.Ledger.Api (
  ExBudget (ExBudget),
  ExCPU,
  ExMemory,
  Script,
 )
import Plutus.V1.Ledger.Api qualified as Plutus

import Spec.Spec (
  Specification (Specification),
  SpecificationExpectation (Success),
  SpecificationTree (..),
 )

--------------------------------------------------------------------------------

-- | Represents the benchmark of a plutus script.
data Benchmark = Benchmark
  { name :: Text
  -- ^ Human readable name describing script.
  , bCPUBudget :: ExCPU
  -- ^ The on-chain execution cost of a script.
  , bMemoryBudget :: ExMemory
  -- ^ The on-chain memory budget of a script.
  , bScriptSize :: Int
  -- ^ The on-chain size of a script.
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON Benchmark where
  parseJSON (Object v) =
    Benchmark <$> v .: "name"
      <*> v .: "cpu"
      <*> v .: "mem"
      <*> v .: "size"
  parseJSON _ = mempty

instance ToJSON Benchmark where
  toJSON (Benchmark name cpu mem size) =
    object
      [ "name" .= name
      , "cpu" .= cpu
      , "mem" .= mem
      , "size" .= size
      ]

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
