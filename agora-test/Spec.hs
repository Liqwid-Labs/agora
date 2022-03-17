--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Test.Tasty (defaultMain, testGroup)

--------------------------------------------------------------------------------

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
          ]
      ]
