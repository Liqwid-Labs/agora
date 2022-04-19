{- |
Module     : Agora.Record
Maintainer : emi@haskell.fyi
Description: PDataRecord helper functions.

PDataRecord helper functions.
-}
module Agora.Record (build, (.=), (.&)) where

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

     For example, @'build' (#foo .= 'pconstantData' (42 :: 'Integer'))@ gets
     the correct type. Namely, @'Term' s ('PDataRecord' '["foo" ':= 'PInteger'])@.
-}
instance forall (sym :: Symbol) (sym' :: Symbol). sym ~ sym' => IsLabel sym (FieldName sym') where
  fromLabel = FieldName

-- | Turn a builder into a fully built 'PDataRecord'.
build :: forall (s :: S) (r :: [PLabeledType]). RecordMorphism s '[] r -> Term s (PDataRecord r)
build f = coerce f pdnil

-- | A morphism from one PDataRecord to another, representing some sort of consing of data.
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
  FieldName sym ->
  Term s (PAsData a) ->
  ( RecordMorphism s as ((sym ':= a) ': as)
  )
_ .= x = RecordMorphism $ pcon . PDCons x

infixr 6 .&

-- | Compose two morphisms between records.
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
