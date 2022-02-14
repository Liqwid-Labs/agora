--------------------------------------------------------------------------------

import Prelude

--------------------------------------------------------------------------------

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

--------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain $
    testGroup
      "Suites"
      [ testCase "will fail" $ assertBool "false" False
      ]
