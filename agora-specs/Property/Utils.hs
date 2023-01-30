module Property.Utils (props) where

import Agora.Utils (phashDatum)
import Generics.SOP (NP (Nil, (:*)))
import Plutarch.Api.V2 (datumHash)
import Plutarch.Test.QuickCheck (
  Equality (OnPEq),
  Partiality (ByComplete),
  TestableTerm (TestableTerm),
  arbitraryPLift,
  haskEquiv,
 )
import Plutarch.Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, adjustOption)
import Test.Tasty.QuickCheck (
  Property,
  QuickCheckTests,
  resize,
  testProperty,
 )

propHashDatumCorrect :: Property
propHashDatumCorrect =
  haskEquiv
    @'OnPEq
    @'ByComplete
    datumHash
    (TestableTerm phashDatum)
    (resize 5 arbitraryPLift :* Nil)

props :: [TestTree]
props =
  [ adjustOption go $ testProperty "Correct 'phashDatum'" propHashDatumCorrect
  ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 20_000
