{- |
Module     : Spec.Effect.TreasuryWithdrawalEffect
Maintainer : seungheon.ooh@gmail.com
Description: Sample based testing for Treasury Withdrawal Effect

This module specs the Treasury Withdrawal Effect.
-}
module Spec.Effect.TreasuryWithdrawal (specs) where

import Sample.Effect.TreasuryWithdrawal (
  Parameters (..),
  Validity (..),
  mkTestTree,
  totallyValidParameters,
 )
import Test.Specification (
  SpecificationTree,
 )

specs :: [SpecificationTree]
specs =
  [ mkTestTree
      "totally valid"
      totallyValidParameters
      Validity
        { forGATPolicy = True
        , forEffectValidator = True
        , forTreasury = True
        }
  , mkTestTree
      "bad received value"
      totallyValidParameters
        { badReceivedValue = True
        }
      Validity
        { forGATPolicy = True
        , forEffectValidator = False
        , forTreasury = True
        }
  , mkTestTree
      "bad receiver order"
      totallyValidParameters
        { badReceiverOrder = True
        }
      Validity
        { forGATPolicy = True
        , forEffectValidator = False
        , forTreasury = True
        }
  ]
