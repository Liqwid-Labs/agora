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

-- NOTE: These are not exported by Plutarch.Prelude, for some reason.
-- Maybe we can 'fix' this upstream?
import Plutarch (ClosedTerm, POpaque, compile)
import Plutarch.Prelude
import Prelude
