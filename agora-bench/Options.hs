module Options (Options (..), parseOptions) where

import Options.Applicative ((<**>))
import Options.Applicative qualified as Opt

newtype Options = Options
  { output :: FilePath
  }

outputOpt :: Opt.Parser FilePath
outputOpt =
  Opt.strOption
    ( Opt.long "output-path"
        <> Opt.short 'o'
        <> Opt.metavar "OUTPUT_PATH"
        <> Opt.value "./bench.csv"
        <> Opt.help "The path of the bench report file."
    )

benchOpt :: Opt.Parser Options
benchOpt = Options <$> outputOpt

parseOptions :: IO Options
parseOptions = Opt.execParser p
  where
    p =
      Opt.info
        (benchOpt <**> Opt.helper)
        ( Opt.fullDesc
            <> Opt.progDesc "Generate benchmark report of agora scripts"
        )
