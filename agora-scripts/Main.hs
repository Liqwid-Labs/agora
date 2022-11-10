{-# LANGUAGE TemplateHaskell #-}

{- | Module     : Main
     Maintainer : emi@haskell.fyi
     Description: Export scripts given configuration.

     Export scripts given configuration.
-}
module Main (main) where

import Agora.Bootstrap qualified as Bootstrap
import Agora.Linker (linker)
import Data.Default (def)
import ScriptExport.Export (exportMain)
import ScriptExport.Types (
  Builders,
  insertScriptExportWithLinker,
 )

main :: IO ()
main = exportMain builders

builders :: Builders
builders =
  mconcat
    [ insertScriptExportWithLinker "agora" (Bootstrap.agoraScripts def) linker
    ]
