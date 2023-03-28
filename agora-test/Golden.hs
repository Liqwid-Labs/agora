module Golden (testGolden) where

import Agora.Bootstrap qualified as Bootstrap
import Agora.Linker (linker)
import Data.Text qualified as Text
import Plutarch (Config (Config), TracingMode (DoTracing, NoTracing))
import ScriptExport.File qualified as ScriptExport
import ScriptExport.Options qualified as ScriptExport
import ScriptExport.Types qualified as ScriptExport
import System.Directory (createDirectoryIfMissing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile)
import Test.Tasty.Providers (TestName)

builders :: ScriptExport.Builders
builders =
  mconcat
    [ ScriptExport.insertScriptExportWithLinker "agora" (Bootstrap.agoraScripts (Config NoTracing)) linker
    , ScriptExport.insertScriptExportWithLinker "agoraDebug" (Bootstrap.agoraScripts (Config DoTracing)) linker
    ]

testGolden :: TestTree
testGolden =
  testGroup
    "Golden tests for script export"
    [ goldenTest "agora" "./agora-test/goldens/"
    , goldenTest "agoraDebug" "./agora-test/goldens/"
    ]

goldenTest :: TestName -> FilePath -> TestTree
goldenTest builder outputPath =
  let mkFilename suffix = outputPath <> builder <> suffix <> ".json"
      goldenFilename = mkFilename "-golden"
      sampleFilename = mkFilename ""
   in goldenVsFile
        builder
        goldenFilename
        sampleFilename
        $ callExportScript builder outputPath

-- Call the script server and generate an unapplied script set.
callExportScript :: String -> FilePath -> IO ()
callExportScript builder outputPath = do
  _ <- createDirectoryIfMissing False outputPath
  ScriptExport.runFile
    builders
    ( ScriptExport.FileOptions
        { out = outputPath
        , param = ""
        , builder = Text.pack builder
        }
    )
