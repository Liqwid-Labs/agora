--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Test.Tasty (defaultMain, testGroup)

--------------------------------------------------------------------------------

import Model.MultiSig qualified
import Spec.Int
import Spec.Stake qualified as Stake

main :: IO ()
main =
  defaultMain $
    testGroup
      "test suite"
      [ testGroup
          "sample-tests"
          Stake.tests
      , testGroup
          "apropos-tx"
          [ testGroup
              "Int"
              [ intPlutarchTests
              ]
          , testGroup
              "MultiSig"
              [ Model.MultiSig.plutarchTests
              , Model.MultiSig.genTests
              ]
          ]
      ]
