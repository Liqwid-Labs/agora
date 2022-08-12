module Options (Options (..), parseOptions) where

import Control.Applicative (optional)
import Options.Applicative ((<**>))
import Options.Applicative qualified as Opt

newtype Options = Options
  { output :: Maybe FilePath
  }

outputOpt :: Opt.Parser (Maybe FilePath)
outputOpt =
  optional $
    Opt.strOption
      ( Opt.long "output-path"
          <> Opt.short 'o'
          <> Opt.metavar "OUTPUT_PATH"
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
