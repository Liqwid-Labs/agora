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
import Plutarch.Builtin
import Plutarch.Internal
import Plutarch.Prelude

--------------------------------------------------------------------------------

import Agora.Utils

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

-- NOTE: discreteValue after valueDiscrete is loses information

-- | Get a 'PValue' from a 'Discrete'
discreteValue ::
  forall (moneyClass :: MoneyClass) (ac :: Symbol) (n :: Symbol) (scale :: Nat) s.
  ( KnownSymbol ac
  , KnownSymbol n
  , moneyClass ~ '(ac, n, scale)
  ) =>
  Term s (Discrete moneyClass :--> PValue)
discreteValue = phoistAcyclic $
  plam $ \f -> pmatch f $ \case
    Discrete p ->
      psingletonValue
        # (pconstant $ fromString $ symbolVal $ Proxy @ac)
        # (pconstant $ fromString $ symbolVal $ Proxy @n)
        # p

-- | Create a value with a single asset class
psingletonValue :: forall s. Term s (PCurrencySymbol :--> PTokenName :--> PInteger :--> PValue)
psingletonValue = phoistAcyclic $
  plam $ \sym tok int ->
    let innerTup = pcon $ PMap $ psingleton #$ ppairDataBuiltin # pdata tok # pdata int
        outerTup = pcon $ PMap $ psingleton #$ ppairDataBuiltin # pdata sym # pdata innerTup
        res = pcon $ PValue outerTup
     in res
