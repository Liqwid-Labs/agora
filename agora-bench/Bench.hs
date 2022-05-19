module Bench (Benchmark (..), benchmarkSize) where

import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Plutus.V1.Ledger.Scripts qualified as Plutus

--------------------------------------------------------------------------------

-- | Represents the benchmark of a plutus script.
data Benchmark = Benchmark
  { name :: Text
  -- ^ Human readable name describing script.
  , size :: Int
  -- ^ The on-chain size of a script.
  }
  deriving stock (Show, Eq, Ord)

-- | Create a benchmark containing only the size of the script.
benchmarkSize :: Text -> Plutus.Script -> Set Benchmark
benchmarkSize name script =
  Set.singleton $
    Benchmark
      { name = name
      , size = scriptSize script
      }

-- | Compute the size of a script on-chain.
scriptSize :: Plutus.Script -> Int
scriptSize = SBS.length . SBS.toShort . LBS.toStrict . serialise
