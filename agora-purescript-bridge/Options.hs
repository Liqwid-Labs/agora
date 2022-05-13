module Options (Options (..), parseOptions) where

import Options.Applicative ((<**>))
import Options.Applicative qualified as Opt

import Data.Maybe (fromJust)
import Path (fromRelDir, parseRelDir, (</>))

data Options = Options
  { output :: FilePath
  , quiet :: Bool
  }

outputOpt :: Opt.Parser FilePath
outputOpt =
  srcFilePath
    <$> ( Opt.strOption $
            Opt.long "output-path"
              <> Opt.short 'o'
              <> Opt.metavar "OUTPUT_PATH"
              <> Opt.value "./"
              <> Opt.help "Output purescripts will be in OUTPUT_PATH/src"
        )

quietOpt :: Opt.Parser Bool
quietOpt =
  Opt.switch $
    Opt.long "quiet"
      <> Opt.short 'q'
      <> Opt.help "Disable verbose log messages"

bridgeOpt :: Opt.Parser Options
bridgeOpt = Options <$> outputOpt <*> quietOpt

parseOptions :: IO Options
parseOptions = Opt.execParser p
  where
    p =
      Opt.info
        (bridgeOpt <**> Opt.helper)
        ( Opt.fullDesc
            <> Opt.progDesc "Generate purescript types of Agora types"
        )

-- Give a directory path, return the path of its src subdirectory.
srcFilePath :: FilePath -> FilePath
srcFilePath path = fromRelDir $
  fromJust $ do
    dir <- parseRelDir path
    srcSubDir <- parseRelDir "src"
    return $ dir </> srcSubDir
