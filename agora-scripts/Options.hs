{- |
Module     : Options
Maintainer : emi@haskell.fyi
Description: Command line options for 'agora-scripts'.

Command line options for 'agora-scripts'.
-}
module Options (Options (..), parseOptions) where

import Options.Applicative ((<**>))
import Options.Applicative qualified as Opt

data Options = Options
  { config :: FilePath
  , output :: FilePath
  }
  deriving stock (Show, Eq)

opt :: Opt.Parser Options
opt =
  Options
    <$> Opt.strOption
      ( Opt.long "config"
          <> Opt.short 'c'
          <> Opt.metavar "CONFIG_PATH"
          <> Opt.value "./"
          <> Opt.help "The path where the script configuration is."
      )
    <*> Opt.strOption
      ( Opt.long "output"
          <> Opt.short 'o'
          <> Opt.metavar "OUTPUT_PATH"
          <> Opt.value "./"
          <> Opt.help "Output where generated scripts will be."
      )

parseOptions :: IO Options
parseOptions = Opt.execParser p
  where
    p =
      Opt.info
        (opt <**> Opt.helper)
        ( Opt.fullDesc
            <> Opt.progDesc "Generate Agora scripts for off-chain use."
        )
