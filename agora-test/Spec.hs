--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Test.Tasty (defaultMain, testGroup)

--------------------------------------------------------------------------------

import Spec.Int

main :: IO ()
main =
  defaultMain $
    testGroup
      "apropos-tx"
      [ testGroup
          "Int"
          [ intPlutarchTests
          ]
      ]
