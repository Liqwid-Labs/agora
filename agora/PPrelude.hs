{- |
Module     : PPrelude
Maintainer : emi@haskell.fyi
Description: Prelude imported throughout this project

Prelude imported throughout this project
-}
module PPrelude (
  module Prelude,
  module Plutarch.Prelude,
  module Plutarch,
) where

-- 'compile' is not exported by Plutarch.Prelude.
import Plutarch (compile)
import Plutarch.Prelude
import Prelude
