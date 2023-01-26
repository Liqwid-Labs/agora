module Property.Utils (props) where

import Agora.Utils (phashDatum)
import Cardano.Prelude (ByteString)
import Plutarch.Api.V2 (datumHash)
import Plutarch.Test.QuickCheck (
  Equality (OnPEq),
  Partiality (ByComplete),
  haskEquiv',
 )
import Plutarch.Test.QuickCheck.Instances ()
import PlutusCore.Data (Data (B))
import PlutusLedgerApi.V1 (Datum (Datum))
import PlutusLedgerApi.V2 (BuiltinData (BuiltinData), DatumHash)
import Test.Tasty (TestTree, adjustOption)
import Test.Tasty.QuickCheck (
  Property,
  QuickCheckTests,
  testProperty,
 )

propHashDatumCorrect :: Property
propHashDatumCorrect =
  haskEquiv'
    @'OnPEq
    @'ByComplete
    hashDatum
    phashDatum
  where
    hashDatum :: ByteString -> DatumHash
    hashDatum = datumHash . Datum . BuiltinData . B

props :: [TestTree]
props =
  [ adjustOption go $ testProperty "Correct 'phashDatum'" propHashDatumCorrect
  ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 20_000
