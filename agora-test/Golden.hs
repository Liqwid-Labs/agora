module Golden (testGolden) where

import Agora.Bootstrap qualified as Bootstrap
import Agora.Linker (linker)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Plutarch (Config (Config), TracingMode (DoTracing, NoTracing))
import ScriptExport.File qualified as ScriptExport
import ScriptExport.Options qualified as ScriptExport
import ScriptExport.Types qualified as ScriptExport
import System.Directory (createDirectoryIfMissing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
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
  goldenVsString
    builder
    (outputPath <> builder <> "-golden.json")
    (callExportScript builder outputPath)

-- Call the script server and generate an unapplied script set.
callExportScript :: String -> FilePath -> IO LBS.ByteString
callExportScript builder outputPath = do
  _ <- createDirectoryIfMissing False outputPath
  let sampleFilePath = outputPath <> builder <> ".json"
  ScriptExport.runFile builders (ScriptExport.FileOptions {out = outputPath, param = "", builder = Text.pack builder})
  LBS.readFile sampleFilePath
