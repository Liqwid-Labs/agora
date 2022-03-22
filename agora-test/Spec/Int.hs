module Spec.Int (
  HasLogicalModel (..),
  IntProp (..),
  intGenTests,
  intPureTests,
  intPlutarchTests,
) where

import Apropos
import Apropos.Script
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

import Plutarch (compile)

data IntProp
  = IsNegative
  | IsPositive
  | IsZero
  | IsLarge
  | IsSmall
  | IsMaxBound
  | IsMinBound
  deriving stock (Eq, Ord, Enum, Show, Bounded)

instance Enumerable IntProp where
  enumerated = [minBound .. maxBound]

instance LogicalModel IntProp where
  logic =
    ExactlyOne [Var IsNegative, Var IsPositive, Var IsZero]
      :&&: ExactlyOne [Var IsLarge, Var IsSmall]
      :&&: (Var IsZero :->: Var IsSmall)
      :&&: (Var IsMaxBound :->: (Var IsLarge :&&: Var IsPositive))
      :&&: (Var IsMinBound :->: (Var IsLarge :&&: Var IsNegative))

instance HasLogicalModel IntProp Int where
  satisfiesProperty IsNegative i = i < 0
  satisfiesProperty IsPositive i = i > 0
  satisfiesProperty IsMaxBound i = i == maxBound
  satisfiesProperty IsMinBound i = i == minBound
  satisfiesProperty IsZero i = i == 0
  satisfiesProperty IsLarge i = i > 10 || i < -10
  satisfiesProperty IsSmall i = i <= 10 && i >= -10

instance HasParameterisedGenerator IntProp Int where
  parameterisedGenerator s = do
    i <-
      if IsZero `elem` s
        then pure 0
        else
          if IsSmall `elem` s
            then int (linear 1 10)
            else
              if IsMaxBound `elem` s
                then pure maxBound
                else int (linear 11 (maxBound - 1))
    if IsNegative `elem` s
      then
        if IsMinBound `elem` s
          then pure minBound
          else pure (-i)
      else pure i

intGenTests :: TestTree
intGenTests =
  testGroup "intGenTests" $
    fromGroup
      <$> [ runGeneratorTestsWhere (Apropos :: Int :+ IntProp) "Int Generator" Yes
          ]

instance HasPureRunner IntProp Int where
  script _ = (/= 0)
  expect _ = Var IsZero :->: Var IsSmall

intPureTests :: TestTree
intPureTests =
  testGroup "intPureTests" $
    fromGroup
      <$> [ runPureTestsWhere
              (Apropos :: Int :+ IntProp)
              "AcceptsSmallNegativeInts"
              Yes -- Constraints the test space.
          ]

instance HasScriptRunner IntProp Int where
  expect _ = Var IsSmall :&&: Var IsNegative
  script _ i =
    let ii = fromIntegral i :: Integer
     in compile
          ( pif
              ( ( fromInteger ii
                    #< (0 :: Term s PInteger)
                )
                  #&& ((fromInteger (-10) :: Term s PInteger) #<= fromInteger ii)
              )
              (pcon PUnit)
              perror
          )

intPlutarchTests :: TestTree
intPlutarchTests =
  testGroup "intPlutarchTests" $
    fromGroup
      <$> [ runScriptTestsWhere (Apropos :: Int :+ IntProp) "AcceptsSmallNegativeInts" Yes
          ]
