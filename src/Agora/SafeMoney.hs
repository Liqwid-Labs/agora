{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wwarn=missing-methods #-}
{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Agora.SafeMoney (module Agora.SafeMoney) where

import Data.Proxy (Proxy (Proxy))
import Data.String
import GHC.TypeLits (
  CmpNat,
  KnownNat,
  KnownSymbol,
  Nat,
  SomeNat (..),
  SomeSymbol (..),
  Symbol,
  natVal,
  someNatVal,
  someSymbolVal,
  symbolVal,
 )
import Prelude

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Value (AssetClass (..))
import Plutus.V1.Ledger.Value qualified as Ledger

--------------------------------------------------------------------------------

import Plutarch.Api.V1
import Plutarch.Internal
import Plutarch.Prelude

--------------------------------------------------------------------------------

-- | Type-level unique identifier for an AssetClass
type MoneyClass =
  ( -- AssetClass
    Symbol
  , -- TokenName
    Symbol
  , -- Decimal places
    Nat
  )

newtype Discrete (mc :: MoneyClass) (s :: S)
  = Discrete (Term s PInteger)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype (Discrete mc) PInteger)

instance Num (Term s (Discrete mc)) where
  (+) x y = pcon $
    Discrete . unTermCont $ do
      Discrete x' <- tcont $ pmatch x
      Discrete y' <- tcont $ pmatch y
      pure (x' + y')
  abs x = pcon $
    Discrete . unTermCont $ do
      Discrete x' <- tcont $ pmatch x
      pure (abs x')
  negate x = pcon $
    Discrete . unTermCont $ do
      Discrete x' <- tcont $ pmatch x
      pure (negate x')
  (*) x y = pcon $
    Discrete . unTermCont $ do
      Discrete x' <- tcont $ pmatch x
      Discrete y' <- tcont $ pmatch y
      pure (x' * y')
  fromInteger = error "Tried to `fromInteger` for a Discrete type. use `discrete` quasiquoter instead."

(^*) :: Term s (Discrete mc) -> Term s PInteger -> Term s (Discrete mc)
(^*) x y = pcon $
  Discrete . unTermCont $ do
    Discrete x' <- tcont $ pmatch x
    pure (x' * y)

type LQ :: MoneyClass
type LQ = '("da8c30857834c6ae7203935b89278c532b3995245295456f993e1d24", "LQ", 6)

type ADA :: MoneyClass
type ADA = '("", "", 6)

--------------------------------------------------------------------------------

-- TODO: upstream something like this
pfind' ::
  PIsListLike list a =>
  (Term s a -> Term s PBool) ->
  Term s (list a :--> PMaybe a)
pfind' p =
  precList
    (\self x xs -> pif (p x) (pcon (PJust x)) (self # xs))
    (const $ pcon PNothing)

-- TODO: upstream something like this
plookup ::
  (PEq a, PIsListLike list (PBuiltinPair a b)) =>
  Term s (a :--> list (PBuiltinPair a b) :--> PMaybe b)
plookup =
  phoistAcyclic $
    plam $ \k xs ->
      pmatch (pfind' (\p -> pfstBuiltin # p #== k) # xs) $ \case
        PNothing -> pcon PNothing
        PJust p -> pcon (PJust (psndBuiltin # p))

matchMaybe :: Term s r -> Term s (PMaybe a) -> TermCont @r s (Term s a)
matchMaybe r f = TermCont $ \k ->
  pmatch f $ \case
    PJust v -> k v
    PNothing -> r

passetClassValueOf ::
  Term s (PCurrencySymbol :--> PTokenName :--> PValue :--> PInteger)
passetClassValueOf =
  phoistAcyclic $
    plam $ \sym token value'' -> unTermCont $ do
      PValue value' <- tcont $ pmatch value''
      PMap value <- tcont $ pmatch value'
      m' <- matchMaybe 0 (plookup # pdata sym # value)
      PMap m <- tcont (pmatch (pfromData m'))
      v <- matchMaybe 0 (plookup # pdata token # m)
      pure (pfromData v)

passetClassValueOf' :: AssetClass -> Term s (PValue :--> PInteger)
passetClassValueOf' (AssetClass (sym, token)) =
  passetClassValueOf # pconstant sym # pconstant token

-- | Downcast a 'PValue' to a 'Discrete' unit
valueDiscrete ::
  forall (moneyClass :: MoneyClass) (ac :: Symbol) (n :: Symbol) (scale :: Nat) s.
  ( KnownSymbol ac
  , KnownSymbol n
  , moneyClass ~ '(ac, n, scale)
  ) =>
  Term s (PValue :--> Discrete moneyClass)
valueDiscrete = phoistAcyclic $
  plam $ \f ->
    pcon . Discrete $
      passetClassValueOf # (pconstant $ fromString $ symbolVal $ Proxy @ac)
        # (pconstant $ fromString $ symbolVal $ Proxy @n)
        # f
