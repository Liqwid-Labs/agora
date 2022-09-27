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

  unless (getField @"quiet" options) $ do
    putStrLn $ "Writing purescript stuff to " <> getField @"output" options
    putStrLn ""

  writePSTypes (getField @"output" options) (buildBridge defaultBridge) agoraTypes
