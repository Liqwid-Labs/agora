module Main (main) where

import Language.PureScript.Bridge (
  buildBridge,
  defaultBridge,
  writePSTypes,
 )

--------------------------------------------------------------------------------

import Control.Monad (unless)

--------------------------------------------------------------------------------

import AgoraTypes (agoraTypes)
import Options (Options (..), parseOptions)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  options <- parseOptions

  unless options.quiet $ do
    putStrLn $ "Writing purescript stuff to " <> options.output
    putStrLn ""

  writePSTypes options.output (buildBridge defaultBridge) agoraTypes
