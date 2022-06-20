{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : Scripts
Maintainer : emi@haskell.fyi
Description: Export scripts given configuration.

Export scripts given configuration.
-}
module Main (main) where

import Options (parseOptions)
import API (runServer)

main :: IO ()
main = do
  options <- parseOptions

  runServer options
