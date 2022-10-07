{-# LANGUAGE TemplateHaskell #-}

{- | Module     : Main
     Maintainer : emi@haskell.fyi
     Description: Export scripts given configuration.

     Export scripts given configuration.
-}
module Main (main) where

import Agora.Bootstrap qualified as Bootstrap
import Agora.Linker
import Data.Default (def)
import ScriptExport.Export
import ScriptExport.Types

main :: IO ()
main = exportMain builders

builders :: Builders
builders =
  insertScriptExportWithLinker "agora" (Bootstrap.agoraScripts def) linker def
