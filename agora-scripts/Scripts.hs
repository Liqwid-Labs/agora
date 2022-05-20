{- |
Module     : Scripts
Maintainer : emi@haskell.fyi
Description: Export scripts given configuration.

Export scripts given configuration.
-}
module Scripts (main) where

import Agora.Governor (Governor (Governor))
import Agora.Governor qualified as Governor
import Agora.Governor.Scripts (governorPolicy)
import Agora.SafeMoney (GTTag)
import Agora.ScriptInfo (PolicyInfo, mkPolicyInfo)
import Control.Monad ((>=>))
import Data.Aeson qualified as Aeson
import GHC.Generics qualified as GHC
import Options (Options (..), parseOptions)
import Plutarch.SafeMoney (Tagged)
import Plutus.V1.Ledger.Api (TxOutRef)
import Plutus.V1.Ledger.Value (AssetClass)
import System.Exit (exitFailure)

data ScriptsConfig = ScriptsConfig
  { governorInitialSpend :: TxOutRef
  , gtClassRef :: Tagged GTTag AssetClass
  , maximumCosigners :: Integer
  }
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
  deriving stock (Show, Eq, GHC.Generic)

data AgoraScripts = AgoraScripts
  { governorPolicyInfo :: PolicyInfo
  }
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
  deriving stock (Show, Eq, GHC.Generic)

main :: IO ()
main = do
  putStrLn "Hello, world!"

  options <- parseOptions

  params <-
    Aeson.eitherDecodeFileStrict @ScriptsConfig options.config
      >>= either (putStrLn >=> const exitFailure) pure

  let scripts = agoraScripts params

  print params
  print scripts

  pure ()

agoraScripts :: ScriptsConfig -> AgoraScripts
agoraScripts config =
  AgoraScripts
    { governorPolicyInfo = mkPolicyInfo (governorPolicy governor)
    }
  where
    governor =
      Governor
        { Governor.gstOutRef = config.governorInitialSpend
        , Governor.gtClassRef = config.gtClassRef
        , Governor.maximumCosigners = config.maximumCosigners
        }
