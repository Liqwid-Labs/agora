{- | Module     : Scripts
     Maintainer : emi@haskell.fyi
     Description: Export scripts given configuration.

     Export scripts given configuration.
-}
module Scripts (main) where

import API (runServer)
import Options (parseOptions)

main :: IO ()
main =
  parseOptions >>= runServer
