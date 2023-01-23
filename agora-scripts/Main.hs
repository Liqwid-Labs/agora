{-# LANGUAGE TemplateHaskell #-}

{- | Module     : Main
     Maintainer : emi@haskell.fyi
     Description: Export scripts given configuration.

     Export scripts given configuration.
-}
module Main (main) where

import Agora.Bootstrap (alwaysSucceedsPolicyRoledScript)
import Agora.Bootstrap qualified as Bootstrap
import Agora.Linker (linker)
import Data.Aeson qualified as Aeson
import Data.Default (def)
import Plutarch (Config (Config), TracingMode (DoTracingAndBinds))
import ScriptExport.Export (exportMain)
import ScriptExport.Types (
  Builders,
  insertBuilder,
  insertScriptExportWithLinker,
 )

main :: IO ()
main = exportMain builders

builders :: Builders
builders =
  mconcat
    [ insertScriptExportWithLinker "agora" (Bootstrap.agoraScripts def) linker
    , insertScriptExportWithLinker
        "agoraDebug"
        ( Bootstrap.agoraScripts
            (Config DoTracingAndBinds)
        )
        linker
    , -- Note: To be compatible with current off-chain setup, we are not using
      --  static builder here.
      insertBuilder
        "alwaysSucceedsPolicy"
        (const @_ @Aeson.Value alwaysSucceedsPolicyRoledScript)
    ]
