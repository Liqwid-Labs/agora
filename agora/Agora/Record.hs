{- |
Module     : Agora.Record
Maintainer : emi@haskell.fyi
Description: PDataRecord helper functions.

'PDataRecord' helper functions.
-}
module Agora.Record (
  mkRecord,
  mkRecordConstr,
  (.=),
  (.&),
  RecordMorphism,
  FieldName,
) where

import Control.Category (Category (..))
import Data.Coerce (coerce)
import GHC.OverloadedLabels (IsLabel (fromLabel))
import GHC.TypeLits (Symbol)
import Plutarch.DataRepr (PDataRecord (PDCons))
import Prelude hiding (id, (.))

-- | Like 'Data.Proxy.Proxy' but local to this module.
data FieldName (sym :: Symbol) = FieldName

{- | The use of two different 'Symbol's here allows unification to happen,
     ensuring 'FieldName' has a fully inferred 'Symbol'.

     For example, @'mkRecord' (#foo .= 'pconstantData' (42 :: 'Integer'))@ gets
     the correct type. Namely, @'Term' s ('PDataRecord' '["foo" ':= 'PInteger'])@.
-}
instance forall (sym :: Symbol) (sym' :: Symbol). sym ~ sym' => IsLabel sym (FieldName sym) where
  fromLabel = FieldName

-- | Turn a constant 'RecordMorphism' into a fully built 'PDataRecord'.
mkRecord :: forall (r :: [PLabeledType]) (s :: S). RecordMorphism s '[] r -> Term s (PDataRecord r)
mkRecord f = f.runRecordMorphism pdnil

{- | 'mkRecord' but for known data-types.

This allows you to dynamically construct a record type constructor.

=== Example:
@
'mkRecordConstr'
  'Agora.Stake.PStakeDatum'
  ( #stakedAmount '.=' 'pconstantData' ('Plutarch.SafeMoney.Tagged' @GTTag 42)
      '.&' #owner '.=' 'pconstantData' "aabbcc"
      '.&' #lockedBy '.=' 'pdata' pnil
  )
@
Is the same as

@
'pconstant' ('Agora.Stake.StakeDatum' ('Plutarch.SafeMoney.Tagged' 42) "aabbcc" [])
@
-}
mkRecordConstr ::
  forall (r :: [PLabeledType]) (s :: S) (pt :: PType).
  PlutusType pt =>
  -- | The constructor. This is just the Haskell-level constructor for the type.
  --   For 'Plutarch.Api.V1.Maybe.PMaybeData', this would
  --   be 'Plutarch.Api.V1.Maybe.PDJust', or 'Plutarch.Api.V1.Maybe.PNothing'.
  (forall s'. Term s' (PDataRecord r) -> pt s') ->
  -- | The morphism that builds the record.
  RecordMorphism s '[] r ->
  Term s pt
mkRecordConstr ctr = pcon . ctr . mkRecord

-- | A morphism from one 'PDataRecord' to another, representing some sort of consing of data.
newtype RecordMorphism (s :: S) (as :: [PLabeledType]) (bs :: [PLabeledType]) = RecordMorphism
  { runRecordMorphism ::
    Term s (PDataRecord as) ->
    Term s (PDataRecord bs)
  }

instance Category (RecordMorphism s) where
  id = RecordMorphism id
  f . g = coerce $ f.runRecordMorphism . g.runRecordMorphism

infix 7 .=

-- | Cons a labeled type as a 'RecordMorphism'.
(.=) ::
  forall (sym :: Symbol) (a :: PType) (as :: [PLabeledType]) (s :: S).
  -- | The field name. You can use @-XOverloadedLabels@ to enable the syntax:
  --   @#hello ~ 'FieldName' "hello"@
  FieldName sym ->
  -- | The value at that field. This must be 'PAsData', because the underlying
  --   type is @'PlutusCore.Data.Constr' 'Integer' ['PlutusCore.Data.Data']@.
  Term s (PAsData a) ->
  RecordMorphism s as ((sym ':= a) ': as)
_ .= x = RecordMorphism $ pcon . PDCons x

infixr 6 .&

-- | Compose two 'RecordMorphism's.
(.&) ::
  forall
    (s :: S)
    (a :: [PLabeledType])
    (b :: [PLabeledType])
    (c :: [PLabeledType]).
  RecordMorphism s b c ->
  RecordMorphism s a b ->
  RecordMorphism s a c
(.&) = (.)
